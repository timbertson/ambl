#![allow(dead_code, unused_variables)]
use std::{mem::size_of, ops::Deref};

// use trou_common::*;
use anyhow::*;
use wasmtime::*;

const U32_SIZE: u32 = size_of::<u32>() as u32;

fn copy_bytes<T, C: AsContext<Data = State<T>>>(store: C, offset: WasmOffset, len: u32) -> Result<Vec<u8>> {
	let mut buf = Vec::with_capacity(len as usize);
	buf.resize(len as usize, 0); // `read` looks at len(), not capacity
	let ctx = store.as_context();
	ctx.data().memory.read(&ctx, offset.raw as usize, &mut buf)?;
	Ok(buf)
}

fn read_u32<T, C: AsContext<Data = State<T>>>(store: C, offset: WasmOffset) -> Result<u32> { let mut buf: [u8; size_of::<u32>()] = [0; 4];
	let ctx = store.as_context();
	ctx.data().memory.read(&ctx, offset.raw as usize, &mut buf)?;
	Ok(u32::from_le_bytes(buf))
}

// Only safe if we never modify store.data after construction
struct StateAlias<T>(*const State<T>);
impl<T> Deref for StateAlias<T> {
	type Target = State<T>;

	fn deref(&self) -> &Self::Target {
		unsafe { &(*self.0) }
	}
}

#[derive(Clone)]
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

struct TargetFunctions {
	trou_init_ctx: TypedFunc<(u32, u32), u32>,
	trou_deinit_ctx: TypedFunc<u32, ()>,
	targets_ffi: TypedFunc<(u32, u32, u32), ()>,
}

struct RuleFunctions {
	rule_fn: TypedFunc<(
		u32, // ctx pointer
		u32, u32, // in str
		u32, u32, // out str
	), ()>,
}

struct State<T: Sized> {
	memory: Memory,

	trou_alloc: TypedFunc<u32, u32>,
	trou_free: TypedFunc<(u32, u32), ()>,

	functions: T,
	
	// outbox for receiving dynamically-sized results from a call
	outbox_ptr: WasmOffset,
	outbox_len: WasmOffset,
}

// This is bad, see comments around usage
struct Nullable<T>(Option<T>);
impl<T> Deref for Nullable<T> {
	type Target = T;

	fn deref(&self) -> &Self::Target {
		self.0.as_ref().expect("Look ma, I reinvented a null pointer error")
	}
}

impl<T: Sized> State<T> {
	unsafe fn dummy() -> Self {
		// can't use size_of with a generic (yet):
		// https://github.com/rust-lang/rust/issues/43408
		// std::mem::transmute([0 as u8; size_of::<State<T>>()])
		todo!()
	}

	unsafe fn alias(&self) -> StateAlias<T> { StateAlias(self as *const State<T>) }
}

impl State<TargetFunctions> {
	fn deinit_ctx<C: AsContextMut<Data=State<TargetFunctions>>>(&self, store: C, value: WasmOffset) -> Result<()> {
		Ok(self.functions.trou_deinit_ctx.call(store, value.raw)?)
	}

	// can't be a real Drop because it needs access to mut store
	fn drop(mut store: Store<Self>) -> Result<()> {
		unsafe {
			let data = store.data() as *const Self; // alias the store's data unsafely
			(*data).trou_free.call(&mut store, ((*data).outbox_ptr.raw, U32_SIZE))?;
			(*data).trou_free.call(&mut store, ((*data).outbox_len.raw, U32_SIZE))?;
		}
		drop(store);
		Ok(())
	}

	fn init_ctx<C: AsContextMut<Data=State<TargetFunctions>>>(&self, mut store: C, target: &str) -> Result<WasmOffset> {
		// let mut store = store.as_context_mut();

		// make space
		let strlen = target.len() as u32;
		let buf_offset = self.trou_alloc.call(store.as_context_mut(), strlen)?;
		// write the string
		self.memory.write(store.as_context_mut(), buf_offset as usize, target.as_bytes())?;
		
		// init the ctx and get targets
		let ret = offset(self.functions.trou_init_ctx.call(store.as_context_mut(), (buf_offset, strlen))?);
		// done with string
		self.trou_free.call(store.as_context_mut(), (buf_offset, strlen))?;
		Ok(ret)
	}

	fn get_targets<C: AsContextMut<Data=State<TargetFunctions>>>(&self, mut store: C, ctx: WasmOffset) -> Result<()> {
		let mut store = store.as_context_mut();

		// get targets
		self.functions.targets_ffi.call(&mut store, (ctx.raw, self.outbox_ptr.raw, self.outbox_len.raw))?;

		// we need to read the u32 pointers via the memory API, interpreting as little-endian (wasm) u32
		let outbox_offset = offset(read_u32(&mut store, self.outbox_ptr.clone())?);
		let outbox_len = read_u32(&mut store, self.outbox_len.clone())?;

		let result_bytes = copy_bytes(&mut store, outbox_offset, outbox_len)?;
		println!("result[{}]: {:?}", outbox_len, String::from_utf8(result_bytes)?);

		// TODO ...
		Ok(())
	}
}

struct WasmModule {
}

impl WasmModule {
	fn load_target(engine: &Engine, module: &Module) -> Result<Store<State<TargetFunctions>>> {
		Self::load(engine, module, |instance, store: &mut Store<State<TargetFunctions>>| {
			Ok(TargetFunctions {
				// TODO &mut *store is weird...
				trou_init_ctx: instance.get_typed_func::<(u32, u32), u32, _>(&mut *store, "trou_init_ctx")?,
				trou_deinit_ctx: instance.get_typed_func::<u32, (), _>(&mut *store, "trou_deinit_ctx")?,
				targets_ffi: instance.get_typed_func::<(u32, u32, u32), (), _>(&mut *store, "targets_ffi")?,
			})
		})
	}

	fn load_rule<T>(engine: &Engine, module: &Module, entrypoint: &str) -> Result<Store<State<RuleFunctions>>> {
		Self::load(engine, module, |instance, store| Ok(RuleFunctions {
			rule_fn: instance.get_typed_func::<(u32, u32, u32, u32, u32), (), _>(store, entrypoint)?,
		}))
	}

	fn compile(engine: &Engine, path: &str) -> Result<Module> {
		println!("Loading {}", path);
		Ok(Module::from_file(&engine, &path)?)
	}

	fn load<T, F: FnOnce(&Instance, &mut Store<State<T>>) -> Result<T>>(engine: &Engine, module: &Module, build_functions: F) -> Result<Store<State<T>>> {
		let mut linker = Linker::<State<T>>::new(&engine);
		
		linker.func_wrap("env", "trou_invoke", |mut caller: Caller<'_, State<T>>, data: u32, data_len: u32, out_offset: u32, out_len_offset: u32| {
			// TODO return result
			let data_bytes = copy_bytes(&mut caller, offset(data), data_len).unwrap();
			let s = String::from_utf8(data_bytes).unwrap();
			println!("Got {} from WebAssembly", &s);
		})?;

		linker.func_wrap("env", "trou_debug", |mut caller: Caller<'_, State<T>>, data: u32, data_len: u32| {
			let data_bytes = copy_bytes(&mut caller, offset(data), data_len).unwrap();
			let s = String::from_utf8(data_bytes).unwrap();
			println!("debug: {}", &s);
		})?;

		// To work around object construction order, we pass an initial dummy value
		// which is completely invalid.
		let mut store = Store::<State<T>>::new(&engine, unsafe { State::<T>::dummy() });
		println!("instantiating...");
		let instance = linker.instantiate(&mut store, &module)?;

		let trou_alloc = instance.get_typed_func::<u32, u32, _>(&mut store, "trou_alloc")?;

		let outbox_ptr = offset(trou_alloc.call(&mut store, U32_SIZE)?);
		let outbox_len = offset(trou_alloc.call(&mut store, U32_SIZE)?);

		let state = State {
			memory: instance
				.get_memory(&mut store, "memory")
				.ok_or(anyhow!("failed to find `memory` export"))?,

			trou_alloc,
			trou_free: instance.get_typed_func::<(u32, u32), (), _>(&mut store, "trou_free")?,

			functions: build_functions(&instance, &mut store)?,
			outbox_ptr,
			outbox_len,
		};
		*store.data_mut() = state; // replace dummy

		let trou_api_version: TypedFunc<(), u32> = instance.get_typed_func::<(), u32, _>(&mut store, "trou_api_version")?;
		println!("API version: {}", trou_api_version.call(&mut store, ())?);
		Ok(store)
	}
}

fn main() -> Result<()> {
	let engine = Engine::default();
	let module = WasmModule::compile(&engine, "target/wasm32-unknown-unknown/debug/trou_sample_builder.wasm")?;
	let mut store = WasmModule::load_target(&engine, &module)?;

	let state = unsafe { store.data().alias() };

	println!("writing...");
	let target = "root";
	let ctx = state.init_ctx(&mut store, target)?;

	state.get_targets(&mut store, ctx.clone())?;

	// free it
	println!("freeing...");
	state.deinit_ctx(&mut store, ctx)?;

	State::drop(store)?;
	
	Ok(())
}
