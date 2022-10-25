use std::mem::size_of;

use anyhow::*;
use wasmtime::*;

fn copy_bytes(memory: &Memory, store: impl AsContext, offset: u32, len: u32) -> Result<Vec<u8>> {
	let mut buf = Vec::with_capacity(len as usize);
	buf.resize(len as usize, 0); // `read` looks at len(), not capacity
	memory.read(store, offset as usize, &mut buf)?;
	// println!("bytes: {:?}", &buf);
	Ok(buf)
}

fn read_u32(memory: &Memory, store: impl AsContext, offset: u32) -> Result<u32> {
	let mut buf: [u8; 4] = [0; 4];
	memory.read(store, offset as usize, &mut buf)?;
	Ok(u32::from_le_bytes(buf))
}

fn main() -> Result<()> {
	let engine = Engine::default();
	let module = Module::from_file(&engine, "target/wasm32-unknown-unknown/debug/trou_sample_builder.wasm")?;
	let mut linker = Linker::<()>::new(&engine);
	linker.func_wrap("env", "trou_invoke", |mut caller: Caller<'_, ()>, data: u32, data_len: u32, out_offset: u32, out_len_offset: u32| {
		// TODO reuse `memory` variable?
		// TODO return result
		let memory = caller.get_export("memory").unwrap().into_memory().unwrap();
		let data_bytes = copy_bytes(&memory, &mut caller, data, data_len).unwrap();
		let s = String::from_utf8(data_bytes).unwrap();
		println!("Got {} from WebAssembly", &s);
	})?;

	linker.func_wrap("env", "trou_debug", |mut caller: Caller<'_, ()>, data: u32, data_len: u32| {
		// TODO reuse `memory` variable?
		// TODO return result
		let memory = caller.get_export("memory").unwrap().into_memory().unwrap();
		let data_bytes = copy_bytes(&memory, &mut caller, data, data_len).unwrap();
		let s = String::from_utf8(data_bytes).unwrap();
		println!("debug: {}", &s);
	})?;

	let mut store = Store::new(&engine, ());
	println!("instantiating...");
	let instance = linker.instantiate(&mut store, &module)?;

	let memory = instance
		.get_memory(&mut store, "memory")
		.ok_or(anyhow!("failed to find `memory` export"))?;

	// base API
	let trou_api_version = instance.get_typed_func::<(), u32, _>(&mut store, "trou_api_version")?;
	let trou_alloc = instance.get_typed_func::<(u32,), u32, _>(&mut store, "trou_alloc")?;
	let trou_free = instance.get_typed_func::<(u32, u32), (), _>(&mut store, "trou_free")?;
	
	let u32_size = size_of::<u32>() as u32;
	let outbox_ptr_offset = trou_alloc.call(&mut store, (u32_size,))?;
	let outbox_len_offset = trou_alloc.call(&mut store, (u32_size,))?;
	
	println!("API version: {}", trou_api_version.call(&mut store, ())?);

	println!("writing...");
	let str = "root";
	// make space
	let buf_offset = trou_alloc.call(&mut store, (str.len() as u32,))?;
	// write the string
	memory.write(&mut store, buf_offset as usize, str.as_bytes())?;
	
	// init the ctx and get targets
	let trou_init_ctx = instance.get_typed_func::<(u32, u32), u32, _>(&mut store, "trou_init_ctx")?;
	let ctx = trou_init_ctx.call(&mut store, (buf_offset, str.len() as u32))?;
	// done with string
	trou_free.call(&mut store, (buf_offset, str.len() as u32))?;

	// get targets
	let targets_ffi = instance.get_typed_func::<(u32, u32, u32), (), _>(&mut store, "targets_ffi")?;
	targets_ffi.call(&mut store, (ctx, outbox_ptr_offset, outbox_len_offset))?;

	// we need to read the u32 pointers via the memory API, interpreting as little-endian (wasm) u32
	let outbox_offset = read_u32(&memory, &mut store, outbox_ptr_offset)?;
	let outbox_len = read_u32(&memory, &mut store, outbox_len_offset)?;

	let result_bytes = copy_bytes(&memory, &mut store, outbox_offset, outbox_len)?;
	println!("result[{}]: {:?}", outbox_len, String::from_utf8(result_bytes)?);
	trou_free.call(&mut store, (outbox_offset, outbox_len))?;

	// free it
	println!("freeing...");
	let trou_deinit_ctx = instance.get_typed_func::<u32, (), _>(&mut store, "trou_deinit_ctx")?;
	trou_deinit_ctx.call(&mut store, ctx)?;

	// trou_free.call(&mut store, (outbox, outbox_size))?;
	
	Ok(())
}
