#![allow(dead_code, unused_variables, unused_imports)]
use std::{mem::size_of, ops::Deref, cell::{Cell, RefCell, Ref}, rc::Rc, sync::{Arc, RwLock, RwLockReadGuard, LockResult, RwLockWriteGuard}, env};
use log::*;

use anyhow::*;
use serde::{Serialize, Deserialize, de::DeserializeOwned};
use trou_common::{build::{DependencyRequest, DependencyResponse}, ffi::ResultFFI, target::{Target, DirectTarget, RawTargetCtx, BaseCtx}};
use wasmtime::*;

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

struct TargetFunctions {
	// trou_init_base_ctx: TypedFunc<(), u32>,
	// trou_deinit_base_ctx: TypedFunc<u32, ()>,

	targets_ffi: StringInOutFFI,
}

struct RuleFunctions {
	// trou_init_target_ctx: TypedFunc<(u32, u32), u32>,
	// trou_deinit_target_ctx: TypedFunc<u32, ()>,

	rule_fn: StringInOutFFI,
}

struct State<T: Sized + Send> {
	memory: Memory,

	trou_alloc: TypedFunc<u32, u32>,
	trou_free: TypedFunc<(u32, u32), ()>,

	functions: T,
	
	// outbox for receiving dynamically-sized results from a call
	outbox_ptr: WasmOffset,
	outbox_len: WasmOffset,
}

// rwlock*ref provides a convenient wrapper to pretend StateRef contains a State<T> instead of Option<State<T>>
struct RwLockReadRef<'a, T: Send>(LockResult<RwLockReadGuard<'a, Option<State<T>>>>);
impl<'a, T: Send> RwLockReadRef<'a, T> {
	fn as_ref(&self) -> Result<&State<T>> {
		self.0.as_ref()
			.map_err(|_| anyhow!("Can't acquire lock"))?
			.as_ref().ok_or_else(||anyhow!("Uninitialized store"))
	}
}

#[cfg(debug_assertions)]
impl<'a, T: Send> Drop for RwLockReadRef<'a, T> {
	fn drop(&mut self) {
		debug!("readLock: drop");
	}
}

struct RwLockWriteRef<'a, T: Send>(LockResult<RwLockWriteGuard<'a, Option<State<T>>>>);
impl<'a, T: Send> RwLockWriteRef<'a, T> {
	fn as_ref(&mut self) -> Result<&mut State<T>> {
		self.0.as_mut()
			.map_err(|_| anyhow!("Can't acquire lock"))?
			.as_mut().ok_or_else(||anyhow!("Uninitialized store"))
	}
}

#[cfg(debug_assertions)]
impl<'a, T: Send> Drop for RwLockWriteRef<'a, T> {
	fn drop(&mut self) {
		debug!("writeLock: drop");
	}
}

// State stores a cell for dynamic borrowing.
// This means we can share it between closures,
// as long as we promise not to violate borrowing rules
// at runtime
struct StateRef<T: Send + Sync + Sync>(Arc<RwLock<Option<State<T>>>>);
impl<T: Send + Sync + Sync> StateRef<T> {
	fn empty() -> Self {
		Self(Arc::new(RwLock::new(None)))
	}
	
	fn set(&mut self, inner: State<T>) -> Result<()> {
		let mut mut_ref = self.0.write().map_err(|_| anyhow!("StateRef.set() failed"))?;
		*mut_ref = Some(inner);
		Ok(())
	}

	fn clone(&self) -> Self {
		Self(Arc::clone(&self.0))
	}
	
	fn read(&self) -> RwLockReadRef<T> {
		debug!("read");
		RwLockReadRef(self.0.read())
	}

	fn write(&self) -> RwLockWriteRef<T> {
		debug!("write");
		RwLockWriteRef(self.0.write())
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
	fn call_serde<I: Serialize, O: DeserializeOwned, C: AsContextMut>(&self, mut store: C, f: TypedFunc<(u32, u32, u32, u32), ()>, arg: &I) -> Result<O> {
		let (buf, len) = self.send_string(store.as_context_mut(), &serde_json::to_string(arg)?)?;
		let read = self.read();

		let state = read.as_ref()?;
		f.call(store.as_context_mut(), (buf.raw, len, state.outbox_ptr.raw, state.outbox_len.raw))?;
		
		let ret_offset = WasmOffset::new(self.read_u32(store.as_context(), state.outbox_ptr)?);

		// we need to read the u32 pointers via the memory API, interpreting as little-endian (wasm) u32
		let ret_len = self.read_u32(store.as_context(), state.outbox_len)?;
		let ret_bytes = self.copy_bytes(store.as_context(), ret_offset, ret_len)?;

		ResultFFI::deserialize(&ret_bytes)
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
}

impl StateRef<TargetFunctions> {
	fn get_targets<C: AsContextMut>(&self, store: C) -> Result<Vec<Target>> {
		debug!("get_targets");
		let read = self.read();
		let state = read.as_ref()?;
		let targets_ffi = state.functions.targets_ffi;
		drop(read);
		self.call_serde(store, targets_ffi, &BaseCtx::new())
	}
}

impl StateRef<RuleFunctions> {
	fn run_builder<C: AsContextMut>(&self, store: C, target: &str, builder: &DirectTarget) -> Result<()> {
		let read = self.read();
		let state = read.as_ref()?;
		let f = state.functions.rule_fn;
		let target = RawTargetCtx::new(target.to_owned());
		drop(read);
		self.call_serde(store, f, &target)
	}
}

struct WasmModule {
}

impl WasmModule {
	fn load_target(engine: &Engine, module: &Module) -> Result<(StateRef<TargetFunctions>, Store<()>)> {
		Self::load(engine, module, |instance: &Instance, store: &mut Store<()>| {
			Ok(TargetFunctions {
				targets_ffi: instance.get_typed_func::<(u32, u32, u32, u32), (), _>(&mut *store, "targets_ffi")?,
			})
		})
	}

	fn load_rule(engine: &Engine, module: &Module, entrypoint: &str) -> Result<(StateRef<RuleFunctions>, Store<()>)> {
		Self::load(engine, module, |instance, store| Ok(RuleFunctions {
			// TODO bind rule_fn lazily? Then this result can be resused for many builds
			rule_fn: instance.get_typed_func::<(u32, u32, u32, u32), (), _>(store, entrypoint)?,
		}))
	}

	fn compile(engine: &Engine, path: &str) -> Result<Module> {
		debug!("Loading {}", path);
		Ok(Module::from_file(&engine, &path)?)
	}
	
	fn invoke<T>(caller: Caller<'_, ()>, request: DependencyRequest) -> Result<DependencyResponse> {
		todo!()
	}
	
	fn load<T: Send + Sync +'static, F: FnOnce(&Instance, &mut Store<()>) -> Result<T>>
		(engine: &Engine, module: &Module, build_functions: F) -> Result<(StateRef<T>, Store<()>)> {
		let mut linker = Linker::new(&engine);

		let mut state = StateRef::<T>::empty();

		let state_invoke = state.clone(); // to move into closure
		linker.func_wrap("env", "trou_invoke", move |mut caller: Caller<'_, ()>, data: u32, data_len: u32, out_offset: u32, out_len_offset: u32| {
			debug!("trou_invoke");
			let mut state = state_invoke.clone();
			let response: Result<()> = (|| { // TODO: return DependencyResponse
				let data_bytes = state.copy_bytes(&mut caller, offset(data), data_len)?;
				let s = String::from_utf8(data_bytes)?;
				let request = serde_json::from_str(&s)?;
				println!("Got dep request: {:?} from WebAssembly", &request);
				Ok(())
			})();
			debug!("trou_invoke: returning {:?}", response);
			let response_str = ResultFFI::serialize(response).unwrap();
			state.return_string(caller, &response_str, WasmOffset::new(out_offset), WasmOffset::new(out_len_offset)).unwrap()
		})?;

		let state_debug = state.clone(); // to move into closure
		linker.func_wrap("env", "trou_debug", move |mut caller: Caller<'_, ()>, data: u32, data_len: u32| {
			let state = state_debug.clone();
			let data_bytes = state.copy_bytes(&mut caller, offset(data), data_len).unwrap();
			let s = String::from_utf8(data_bytes).unwrap();
			println!("debug: {}", &s);
		})?;

		let mut store = Store::new(&engine, ());
		debug!("instantiating...");
		let instance = linker.instantiate(&mut store, &module)?;

		let trou_alloc = instance.get_typed_func::<u32, u32, _>(&mut store, "trou_alloc")?;

		let outbox_ptr = offset(trou_alloc.call(&mut store, U32_SIZE)?);
		let outbox_len = offset(trou_alloc.call(&mut store, U32_SIZE)?);

		let inner = State {
			memory: instance
				.get_memory(&mut store, "memory")
				.ok_or(anyhow!("failed to find `memory` export"))?,

			trou_alloc,
			trou_free: instance.get_typed_func::<(u32, u32), (), _>(&mut store, "trou_free")?,

			functions: build_functions(&instance, &mut store)?,
			outbox_ptr,
			outbox_len,
		};

		state.set(inner)?;

		let trou_api_version: TypedFunc<(), u32> = instance.get_typed_func::<(), u32, _>(&mut store, "trou_api_version")?;
		println!("API version: {}", trou_api_version.call(&mut store, ())?);
		Ok((state, store))
	}
}

fn main() -> Result<()> {
	env_logger::init_from_env(
		env_logger::Env::default().filter_or(env_logger::DEFAULT_FILTER_ENV, "info"));

	let engine = Engine::default();
	let module = WasmModule::compile(&engine, "target/wasm32-unknown-unknown/debug/trou_sample_builder.wasm")?;

	let (state, mut store) = WasmModule::load_target(&engine, &module)?;

	let targets = state.get_targets(&mut store)?;
	debug!("targets: {:?}", targets);

	let args: Vec<String> = env::args().skip(1).collect();
	for arg in args {
		let target = targets.iter().find(|t| match t {
			Target::Indirect(x) => { debug!("skipping indirect... {:?}", x); false },
			Target::Direct(t) => t.names.iter().any(|n| n == &arg),
		}).ok_or_else(|| anyhow!("Not a buildable target: {}", &arg))?;

		info!("found target for {}: {:?}", &arg, target);
		match target {
			Target::Indirect(x) => todo!(),
			// TODO this only supports a single module
			Target::Direct(t) => {
				let (state, mut store) = WasmModule::load_rule(&engine, &module, &t.build.name)?;
				state.run_builder(&mut store, &arg, t)?
			},
		}
	}

	state.drop(store)?;
	
	Ok(())
}
