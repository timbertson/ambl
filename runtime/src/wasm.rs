use std::{mem::size_of, ops::Deref, cell::{Cell, RefCell, Ref}, rc::Rc, sync::{Arc, RwLock, RwLockReadGuard, LockResult, RwLockWriteGuard, TryLockResult, Mutex}, env, collections::{HashMap, hash_map::Entry}};
use log::*;

use anyhow::*;
use serde::{Serialize, Deserialize, de::DeserializeOwned};
use serde_json::map::OccupiedEntry;
use trou_common::build::*;
use trou_common::ctx::*;
use trou_common::ffi::ResultFFI;
use trou_common::rule::*;
use wasmtime::*;

use crate::{sync::{RwLockReadRef, RwLockWriteRef}, project::{Project, ProjectRef, BuildReason, ProjectHandle, ActiveBuildToken, BuildFnCall, BuildRequest}, persist::PersistFile, module::{BuildModule}, path_util::{Scoped, CPath, Scope, Unscoped}, err::result_block};

const U32_SIZE: u32 = size_of::<u32>() as u32;

#[derive(Copy, Clone)]
#[repr(transparent)]
struct WasmOffset {
	raw: u32
}

impl WasmOffset {
	fn new(raw: u32) -> Self {
		Self { raw }
	}
}
fn offset(raw: u32) -> WasmOffset { WasmOffset::new(raw) }

type StringInOutFFI =TypedFunc<(
		u32, u32, // in str
		u32, u32, // out str
	), ()>;

pub struct State {
	instance: Instance,
	memory: Memory,

	trou_alloc: TypedFunc<u32, u32>,
	trou_free: TypedFunc<(u32, u32), ()>,

	// outbox for receiving dynamically-sized results from a call
	outbox_ptr: WasmOffset,
	outbox_len: WasmOffset,
}


// State stores a cell for dynamic borrowing.
// This means we can share it between closures,
// as long as we promise not to violate borrowing rules
// at runtime
pub struct StateRef(Arc<RwLock<Option<State>>>);
impl StateRef {
	pub fn empty() -> Self {
		Self(Arc::new(RwLock::new(None)))
	}
	
	pub fn set(&mut self, inner: State) -> Result<()> {
		let mut mut_ref = self.0.write().map_err(|_| anyhow!("StateRef.set() failed"))?;
		*mut_ref = Some(inner);
		Ok(())
	}

	pub fn clone(&self) -> Self {
		Self(Arc::clone(&self.0))
	}
	
	pub fn read(&self) -> RwLockReadRef<State> {
		debug!("read");
		RwLockReadRef(self.0.try_read())
	}

	pub fn write(&self) -> RwLockWriteRef<State> {
		debug!("write");
		RwLockWriteRef(self.0.try_write())
	}

	fn copy_bytes<C: AsContext>(&self, store: C, offset: WasmOffset, len: u32) -> Result<Vec<u8>> {
		debug!("copy_bytes");
		let mut buf = Vec::with_capacity(len as usize);
		buf.resize(len as usize, 0); // `read` looks at len(), not capacity
		let ctx = store.as_context();
		self.read().as_ref()?.memory.read(&ctx, offset.raw as usize, &mut buf)?;
		debug!("copy_bytes complete");
		Ok(buf)
	}

	fn read_u32<C: AsContext>(&self, store: C, offset: WasmOffset) -> Result<u32> {
		debug!("read_u32");
		let mut buf: [u8; size_of::<u32>()] = [0; 4];
		let ctx = store.as_context();
		self.read().as_ref()?.memory.read(&ctx, offset.raw as usize, &mut buf)?;
		debug!("read_u32 complete");
		Ok(u32::from_le_bytes(buf))
	}

	fn write_u32<C: AsContextMut>(&mut self, mut store: C, offset: WasmOffset, value: u32) -> Result<()> {
		debug!("write_u32");
		let mut ctx = store.as_context_mut();
		let bytes = value.to_le_bytes();
		self.write().as_ref()?.memory.write(&mut ctx, offset.raw as usize, &bytes)?;
		debug!("write_u32 complete");
		Ok(())
	}

	// return a string by writing its coorinates into the provided out and len pointers
	fn return_string<C: AsContextMut>
		(&mut self, mut store: C, s: &str, offset_out: WasmOffset, len_out: WasmOffset) -> Result<()> {
		debug!("return_string");
		let (buf_offset, len) = self.send_string(store.as_context_mut(), s)?;
		self.write_u32(store.as_context_mut(), offset_out, buf_offset.raw)?;
		self.write_u32(store.as_context_mut(), len_out, len)?;
		debug!("return_string wrote u32s");
		Ok(())
	}

	// send a string, returning the pointer + length to be passed as arguments
	fn send_string<C: AsContextMut>
		(&self, mut store: C, s: &str) -> Result<(WasmOffset, u32)> {
		debug!("send_string");
		let mut write = self.write();
		let state = write.as_ref()?;
		let strlen = s.len() as u32;

		let buf_offset = WasmOffset::new(state.trou_alloc.call(store.as_context_mut(), strlen)?);
		state.memory.write(store.as_context_mut(), buf_offset.raw as usize, s.as_bytes())?;
		debug!("send_string complete");
		Ok((buf_offset, strlen))
	}
	
	fn call_str<O, FO: FnOnce(Vec<u8>) -> Result<O>, C: AsContextMut>(&self, mut store: C, f: TypedFunc<(u32, u32, u32, u32), ()>, arg: &str, fo: FO) -> (C, Result<O>) {
		let result = result_block(|| {
			let (buf, len) = self.send_string(store.as_context_mut(), arg)?;

			let read = self.read();
			// Grab a few things from state and then drop read before calling.
			// Calling the function could reentrantly invoke anything, we need to leave state unlocked
			let state = read.as_ref()?;
			let outbox_ptr = state.outbox_ptr;
			let outbox_len = state.outbox_len;
			drop(read);

			f.call(store.as_context_mut(), (buf.raw, len, outbox_ptr.raw, outbox_len.raw))?;
			
			let ret_offset = WasmOffset::new(self.read_u32(store.as_context(), outbox_ptr)?);

			// we need to read the u32 pointers via the memory API, interpreting as little-endian (wasm) u32
			let ret_len = self.read_u32(store.as_context(), outbox_len)?;
			let ret_bytes = self.copy_bytes(store.as_context(), ret_offset, ret_len)?;
			fo(ret_bytes)
			// TODO free ret_bytes from guest memory
		});
		(store, result)
	}

	// can't be a real Drop because it needs access to mut store
	fn drop<R>(&self, mut store: Store<R>) -> Result<()> {
		let read = self.read();
		let state = read.as_ref()?;
		state.trou_free.call(&mut store, (state.outbox_ptr.raw, U32_SIZE))?;
		state.trou_free.call(&mut store, (state.outbox_len.raw, U32_SIZE))?;
		drop(store);
		Ok(())
	}

	pub fn call<C: AsContextMut<Data = StoreInner>, Ctx: Serialize + AsRef<BaseCtx>>(&self, mut store: C, f: &BuildFnCall, arg: &Ctx, _unlocked_evidence: &ProjectHandle<WasmModule>) -> Result<Vec<u8>> {
		debug!("call({:?})", f);
		let mut write = self.write();
		let state = write.as_ref()?;

		// We wrap every call by inserting the scope into the store. This
		// lets us reattach the (implicit) scope within `invoke`
		let mut store_ctx = store.as_context_mut();
		let scope_map = store_ctx.data_mut();
		let token = ActiveBuildToken::from_raw(arg.as_ref().token);
		let inserted = match scope_map.entry(token) {
			Entry::Occupied(_) => false,
			Entry::Vacant(entry) => {
				entry.insert(f.scope.to_owned());
				true
			}
		};
		
		let call_ffi = state.instance.get_typed_func::<(u32, u32, u32, u32), (), _>(
			store_ctx, &f.fn_name)?;
		drop(write);
		let (mut store, result) = self.call_str(store, call_ffi, &serde_json::to_string(arg)?, |b| Ok(b));

		if inserted {
			let mut store_ctx = store.as_context_mut();
			let scope_map = store_ctx.data_mut();
			scope_map.remove(&token);
		}

		result
	}
}

// #[derive(Debug, Clone, PartialEq)]
// struct ActiveBuildContext {
// 	scope: Scope,
// 	module: Unscoped, // TODO arc?
// }

type StoreInner = HashMap<ActiveBuildToken, Scope>;

pub struct WasmModule {
	state: StateRef,
	store: Store<StoreInner>,
}

#[derive(Clone)]
pub struct Compiled {
	wasm: Module,
	path: Unscoped,
}

impl BuildModule for WasmModule {
	type Compiled = Compiled;

	fn compile(engine: &Engine, path: &Unscoped) -> Result<Compiled> {
		debug!("Loading {}", path);
		Ok(Compiled {
			wasm: Module::from_file(&engine, path.0.as_path())?,
			path: path.clone(),
		})
	}
	
	fn load(engine: &Engine, module: &Compiled, project: ProjectRef<Self>) -> Result<WasmModule> {
		let mut linker = Linker::<StoreInner>::new(&engine);

		let mut state = StateRef::empty();
		
		let path_arc = Arc::new(module.path.clone());
		let state_invoke = state.clone(); // to move into closure
		let state_module = Arc::new(module.path.to_owned());
		linker.func_wrap("env", "trou_invoke", move |mut caller: Caller<'_, StoreInner>, data: u32, data_len: u32, out_offset: u32, out_len_offset: u32| {
			debug!("trou_invoke");
			let mut state = state_invoke.clone();
			let response: Result<DependencyResponse> = (|| { // TODO: return DependencyResponse
				let data_bytes = state.copy_bytes(&mut caller, offset(data), data_len)?;
				let s = String::from_utf8(data_bytes)?;
				debug!("Got string from wasm: {}", &s);
				let request: TaggedDependencyRequest = serde_json::from_str(&s)?;
				debug!("Got dep request: {:?} from WebAssembly", &request);
				let mut project_handle = project.handle();
				let TaggedDependencyRequest { token, request } = request;
				let token = ActiveBuildToken::from_raw(token);
				let scope_map = caller.data_mut();
				let scope = scope_map.get(&token)
					.ok_or_else(|| anyhow!("invoke called without an extive scope; this should be impossible"))?;

				let build_request: BuildRequest = BuildRequest::from(request, Some(&path_arc), scope)?;

				let module = state_module.clone();
				let scoped_request = Scoped::new(scope.to_owned(), &build_request);

				let project = project_handle.lock("trou_invoke")?;
				let (project, persist) = Project::build(project, scoped_request, &BuildReason::Dependency(token))?;

				let file_request = match build_request {
					BuildRequest::FileDependency(ref f) => Some(f),
					_ => None,
				};
				persist.into_response(&project, file_request)
			})();
			debug!("trou_invoke: returning {:?}", response);
			let result: Result<()> = (|| {
				let response_str = ResultFFI::serialize(response)?;
				state.return_string(caller, &response_str, WasmOffset::new(out_offset), WasmOffset::new(out_len_offset))
			})();
			result.expect("trou_invoke couldn't send return value")
		})?;

		let state_debug = state.clone(); // to move into closure
		linker.func_wrap("env", "trou_debug", move |mut caller: Caller<'_, _>, data: u32, data_len: u32| {
			let state = state_debug.clone();
			let s: Result<String> = (||{
				let data_bytes = state.copy_bytes(&mut caller, offset(data), data_len)?;
				Ok(String::from_utf8(data_bytes)?)
			})();
			println!("debug: {}", &s.expect("failed to decode debug string"));
		})?;

		let mut store = Store::new(&engine, Default::default());
		debug!("instantiating...");
		let instance = linker.instantiate(&mut store, &module.wasm)?;

		let trou_alloc = instance.get_typed_func::<u32, u32, _>(&mut store, "trou_alloc")?;

		let outbox_ptr = offset(trou_alloc.call(&mut store, U32_SIZE)?);
		let outbox_len = offset(trou_alloc.call(&mut store, U32_SIZE)?);

		let inner = State {
			instance,
			memory: instance
				.get_memory(&mut store, "memory")
				.ok_or(anyhow!("failed to find `memory` export"))?,

			trou_alloc,
			trou_free: instance.get_typed_func::<(u32, u32), (), _>(&mut store, "trou_free")?,

			outbox_ptr,
			outbox_len,
		};

		state.set(inner)?;

		let trou_api_version: TypedFunc<(), u32> = instance.get_typed_func::<(), u32, _>(&mut store, "trou_api_version")?;
		debug!("API version: {}", trou_api_version.call(&mut store, ())?);
		Ok(WasmModule { state, store })
	}

	fn call<Ctx: AsRef<BaseCtx> + Serialize>(&mut self, f: &BuildFnCall, arg: &Ctx, _unlocked_evidence: &ProjectHandle<Self>) -> Result<Vec<u8>> {
		self.state.call(&mut self.store, f, arg, _unlocked_evidence)
	}
}
