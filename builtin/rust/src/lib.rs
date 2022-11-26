use anyhow::*;
use trou_api::*;

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


ffi!(cargo);
fn cargo(c: &TargetCtx) -> Result<()> {
	debug(&format!("Building {}", c.target()));
	// TODO: depend on *.toml, src/**/*.rs
	c.always_rebuild()?;

	// TODO: determine package from Cargo.toml
	c.run(cmd("cargo").args(vec!("build", "--target", "wasm32-unknown-unknown", "--package", "trou-sample-builder")))?;
	Ok(())
}

ffi!(rules);
pub fn rules(_: &BaseCtx) -> Result<Vec<Rule>> {
	Ok(vec!(
		target("default", build_fn("cargo")),
	))
}
