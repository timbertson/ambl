#![allow(dead_code, unused_variables)]

use anyhow::*;

use trou_api::*;

// TODO: boilerplate via macro / derive?
#[no_mangle]
pub extern "C" fn targets_ffi(c: &mut BaseCtx, out: &mut *mut u8, len: &mut u32) {
	debug("HI!");
	let inner = targets_inner(c);
	c.encode_targets(inner, SizedPtrRef::wrap(out, len)).unwrap()
}


pub extern "C" fn build_all(c: &mut TargetCtx) -> Result<()> {
	debug(&format!("Building {}", c.target()));
	c.build("a")?;
	Ok(())
}

// pub extern "C" fn platform_targets(c: &mut TargetCtx) -> Result<Vec<Target>> {
// 	Ok(vec!(
// 		targets(vec!("x86", "aarch64"), |c: &Ctx| {
// 			println!("you built: {}", c.target());
// 			Ok(())
// 		}),
// 	))
// }

pub fn targets_inner(_: &mut BaseCtx) -> Result<Vec<Target>> {
	Ok(vec!(
		target("all", build_fn("build_all")),
		targets(vec!("a", "b", "c"), build_fn("build_all")),

		target("lint", build_via("builtin:lint.wasm", "build_all").config("...")),
		
		include(module("builtin:scala.wasm").config("{ x: 123 }")),

		include(module("builtin:lint.wasm").scope("lint").config("{ src: ./scala }")),
	))
}


// declarative build API
pub struct SampleOpts {
	debug: bool,
}

// TODO wrapper which takes String and invokes with SampleBuild
pub fn sample_build(ctx: &mut TargetCtx, opts: SampleOpts) -> Result<()> {
	Ok(())
}
