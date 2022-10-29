#![allow(dead_code, unused_variables)]

use anyhow::*;

use serde::Serialize;
use trou_api::{*, BaseCtx};

macro_rules! ffi {
	($orig:ident, $t: ty) => {
		// place it in a module to avoid namespace clash, but export it to C unmangled.
		// I'd rather export as e.g. ${orig}_ffi but I don't think that's possible
		pub mod $orig {
			use trou_api::*;

			#[no_mangle]
			pub extern "C" fn $orig<'a>(c: &mut $t, ptr: &'a mut *mut u8, len: &'a mut u32) {
				super::wrap_fn_mut1(super::$orig, c, ptr, len)
			}
		}
	}
}


ffi!(build_all, TargetCtx);
fn build_all(c: &mut TargetCtx) -> Result<()> {
	debug(&format!("Building {}", c.target()));
	c.build("a")?;
	Ok(())
}

pub fn wrap_fn_mut1<'a, I, O: Serialize, F: FnOnce(&mut I)
	-> Result<O>>(f: F, i: &'a mut I, ptr: &'a mut *mut u8, len: &'a mut u32) {
	let mut out = SizedPtrRef::wrap(ptr, len);
	let result = ResultFFI::from(f(i));
	let bytes = serde_json::to_vec(&result).unwrap();
	// TODO nothing frees this yet!
	out.write_and_leak(bytes).unwrap()
}

// TODO automate this somehow
ffi!(targets_ffi, BaseCtx);
pub fn targets_ffi(_: &mut BaseCtx) -> Result<Vec<Target>> {
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
