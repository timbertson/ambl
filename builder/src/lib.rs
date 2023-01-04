#![allow(dead_code, unused_variables)]

use anyhow::*;
use serde_json::json;
use ambl_api::*;

macro_rules! ffi {
	($orig:ident) => {
		// place it in a module to avoid namespace clash, but export it to C unmangled.
		// I'd rather export as e.g. ${orig}_ffi but I don't think that's possible
		pub mod $orig {
			// use ambl_api::*;

			#[no_mangle]
			pub extern "C" fn $orig<'a>(ptr_in: *const u8, len_in: u32, ptr_out: &'a mut *mut u8, len_out: &'a mut u32) {
				super::wrap_fn_mut1(super::$orig, ptr_in, len_in, ptr_out, len_out)
			}
		}
	}
}

// macro_rules! ffi2 {
// 	($orig:ident) => {
// 		// place it in a module to avoid namespace clash, but export it to C unmangled.
// 		// I'd rather export as e.g. ${orig}_ffi but I don't think that's possible
// 		pub mod $orig {
// 			// use ambl_api::*;

// 			#[no_mangle]
// 			pub extern "C" fn $orig<'a>(ptr_in: *const u8, len_in: u32, ptr_out: &'a mut *mut u8, len_out: &'a mut u32) {
// 				super::wrap_fn_mut2(super::$orig, ptr_in, len_in, ptr_out, len_out)
// 			}
// 		}
// 	}
// }

ffi!(build_all);
fn build_all(c: TargetCtx) -> Result<()> {
	debug(&format!("Building {}", c.target()));
	if c.target() != "a" {
		c.build("a")?;
	}
	Ok(())
}

// TODO clean up this macro invocation
ffi!(rules_ffi);
pub fn rules_ffi(_: BaseCtx) -> Result<Vec<Rule>> {
	Ok(vec!(
		target("all", build_fn("build_all")),
		targets(vec!("a", "b", "c"), build_fn("build_all")),

		target("lint", build_via("builtin:lint.wasm", "build_all").config(json!({}))?),
		
		include(module("builtin:scala.wasm").config(json!({ "x": 123 }))),

		include(module("builtin:lint.wasm").scope("lint").config(json!({ "src": "./scala" }))),
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
