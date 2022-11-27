use anyhow::*;
use serde::{Deserialize, Serialize};
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

// TODO how do we share this struct nicely without
// - making a second crate containing just the struct
// - exposing internal functions (like `cargo`) on any module that depends on us
#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct CargoOpts {
	package: Option<String>,
}
impl Default for CargoOpts {
	fn default() -> Self {
		Self { package: Default::default() }
	}
}

ffi!(cargo);
fn cargo(c: TargetCtx) -> Result<()> {
	let opts: CargoOpts = c.parse_config()?;
	debug(&format!("Building {} with {:?}", c.target(), &opts));
	// TODO: depend on *.toml, src/**/*.rs
	c.always_rebuild()?;

	let mut args = vec!("build", "--target", "wasm32-unknown-unknown");
	if let Some(package) = opts.package.as_ref() {
		args.push("--package");
		args.push(package);
	}
	// TODO: determine package from Cargo.toml
	c.run(cmd("cargo").args(args))?;
	Ok(())
}

ffi!(rules);
pub fn rules(_: BaseCtx) -> Result<Vec<Rule>> {
	Ok(vec!(
		target("default", build_fn("cargo")),
	))
}
