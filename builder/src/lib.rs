#![allow(dead_code, unused_variables)]

use core::slice;

use anyhow::*;

use serde::{Serialize, de::DeserializeOwned};
use trou_api::{*, BaseCtx};

macro_rules! ffi {
	($orig:ident) => {
		// place it in a module to avoid namespace clash, but export it to C unmangled.
		// I'd rather export as e.g. ${orig}_ffi but I don't think that's possible
		pub mod $orig {
			// use trou_api::*;

			#[no_mangle]
			pub extern "C" fn $orig<'a>(ptr_in: *const u8, len_in: u32, ptr_out: &'a mut *mut u8, len_out: &'a mut u32) {
				super::wrap_fn_mut1(super::$orig, ptr_in, len_in, ptr_out, len_out)
			}
		}
	}
}


ffi!(build_all);
fn build_all(c: &TargetCtx) -> Result<()> {
	debug(&format!("Building {}", c.target()));
	c.build("a")?;
	Ok(())
}

pub fn wrap_fn_mut1<'a, I: DeserializeOwned, O: Serialize, F: FnOnce(&I)
	-> Result<O>>(f: F, ptr_in: *const u8, len_in: u32, ptr_out: &'a mut *mut u8, len_out: &'a mut u32) {
	let mut out = SizedPtrRef::wrap(ptr_out, len_out);
	let in_bytes = unsafe { slice::from_raw_parts(ptr_in, len_in as usize) };
	let result: Result<O> = (|| {
		let input = serde_json::from_slice::<I>(in_bytes)?;
		f(&input)
	})();
	let bytes = ResultFFI::serialize(result).unwrap().into_bytes();
	out.write_and_leak(bytes).unwrap()
}

// TODO automate this somehow
ffi!(targets_ffi);
pub fn targets_ffi(_: &BaseCtx) -> Result<Vec<Target>> {
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
