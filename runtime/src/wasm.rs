use std::{mem::size_of, ops::Deref, cell::{Cell, RefCell, Ref}, rc::Rc, sync::{Arc, RwLock, RwLockReadGuard, LockResult, RwLockWriteGuard, TryLockResult, Mutex}, env, collections::{HashMap, hash_map::Entry}};
use log::*;

use anyhow::*;
use serde::{Serialize, Deserialize, de::DeserializeOwned};
use serde_json::map::OccupiedEntry;
use trou_common::{build::*, ctx::{BaseCtx, RawTargetCtx}};
use trou_common::ffi::ResultFFI;
use trou_common::rule::*;
use wasmtime::*;

use crate::{sync::{RwLockReadRef, RwLockWriteRef}, project::{Project, ProjectRef, BuildReason, ProjectHandle, ActiveBuildToken}, persist::PersistFile};

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
	
	// serialize an argument and call a guest function taking (buf, len, buf_out, len_out). Return the deserialized output
	fn call_serde<I: Serialize, O: DeserializeOwned, C: AsContextMut>(&self, store: C, f: TypedFunc<(u32, u32, u32, u32), ()>, arg: &I) -> Result<O> {
		self.call_str(store, f, &serde_json::to_string(arg)?, |b| ResultFFI::deserialize(&b))
	}

	fn call_str<O, FO: FnOnce(Vec<u8>) -> Result<O>, C: AsContextMut>(&self, mut store: C, f: TypedFunc<(u32, u32, u32, u32), ()>, arg: &str, fo: FO) -> Result<O> {
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

	// TODO: accept config and pass it through
	pub fn get_rules<C: AsContextMut>(&self, mut store: C) -> Result<Vec<Rule>> {
		debug!("get_rules");
		let mut write = self.write();
		let state = write.as_ref()?;
		let rules_ffi = state.instance.get_typed_func::<(u32, u32, u32, u32), (), _>(
			store.as_context_mut(), "rules_ffi")?;
		drop(write);
		self.call_serde(store, rules_ffi, &BaseCtx::new())
	}

	pub fn run_builder<C: AsContextMut>(&self, mut store: C, token: ActiveBuildToken, path: &str, builder: &Target, _unlocked_evidence: &ProjectHandle) -> Result<Option<PersistFile>> {
		debug!("run_builder");
		let mut write = self.write();
		let state = write.as_ref()?;
		let f = state.instance.get_typed_func::<(u32, u32, u32, u32), (), _>(store.as_context_mut(), &builder.build.fn_name)?;
		let target = RawTargetCtx::new(path.to_owned(), token.raw());
		drop(write);
		debug!("run_builder call_serde");
		self.call_serde(store, f, &target)?;
		PersistFile::from_path(path)
	}

	pub fn call_fn<C: AsContextMut>(&self, mut store: C, call: &FunctionSpec, _unlocked_evidence: &ProjectHandle) -> Result<String> {
		debug!("call_fn");
		let mut write = self.write();
		let state = write.as_ref()?;
		let f = state.instance.get_typed_func::<(u32, u32, u32, u32), (), _>(store.as_context_mut(), &call.fn_name)?;
		drop(write);
		debug!("run_builder call_serde");
		let arg = call.config.as_ref().expect("TODO implement default arg for missing config");
		self.call_str(store, f, &arg, |b| Ok(String::from_utf8(b)?))
	}
}

pub struct WasmModule {
	pub state: StateRef,
	pub store: Store<()>,
}

impl WasmModule {
	pub fn compile(engine: &Engine, path: &str) -> Result<Module> {
		debug!("Loading {}", path);
		Ok(Module::from_file(&engine, &path)?)
	}
	
	pub fn load(engine: &Engine, module: &Module, project: ProjectRef) -> Result<WasmModule> {
		let mut linker = Linker::new(&engine);

		let mut state = StateRef::empty();
		
		let state_invoke = state.clone(); // to move into closure
		linker.func_wrap("env", "trou_invoke", move |mut caller: Caller<'_, ()>, data: u32, data_len: u32, out_offset: u32, out_len_offset: u32| {
			debug!("trou_invoke");
			let mut state = state_invoke.clone();
			let response: Result<DependencyResponse> = (|| { // TODO: return DependencyResponse
				let data_bytes = state.copy_bytes(&mut caller, offset(data), data_len)?;
				let s = String::from_utf8(data_bytes)?;
				debug!("Got string from wasm: {}", &s);
				let request: TaggedDependencyRequest = serde_json::from_str(&s)?;
				println!("Got dep request: {:?} from WebAssembly", &request);
				let mut project_handle = project.handle();
				let project = project_handle.lock("trou_invoke")?;
				let TaggedDependencyRequest { token, request } = request;
				let (_, persist) = Project::build(project, &request, &BuildReason::Dependency(ActiveBuildToken::from_raw(token)))?;
				Ok(persist.into_response())
			})();
			debug!("trou_invoke: returning {:?}", response);
			let result: Result<()> = (|| {
				let response_str = ResultFFI::serialize(response)?;
				state.return_string(caller, &response_str, WasmOffset::new(out_offset), WasmOffset::new(out_len_offset))
			})();
			result.expect("trou_invoke couldn't send return value")
		})?;

		let state_debug = state.clone(); // to move into closure
		linker.func_wrap("env", "trou_debug", move |mut caller: Caller<'_, ()>, data: u32, data_len: u32| {
			let state = state_debug.clone();
			let s: Result<String> = (||{
				let data_bytes = state.copy_bytes(&mut caller, offset(data), data_len)?;
				Ok(String::from_utf8(data_bytes)?)
			})();
			println!("debug: {}", &s.expect("failed to decode debug string"));
		})?;

		let mut store = Store::new(&engine, ());
		debug!("instantiating...");
		let instance = linker.instantiate(&mut store, &module)?;

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
}
