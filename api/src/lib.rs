// extern shims required by guests, plus a reexport of trou_common

pub use trou_common::build::*;
pub use trou_common::target::*;
pub use trou_common::ffi::*;

use serde::de::DeserializeOwned;
use anyhow::*;


// used to detect incompatible guest modules
#[no_mangle]
pub extern "C" fn trou_api_version() -> usize {
	1
}

extern {
	pub fn trou_invoke(data: *const u8, len: u32, out: &mut *mut u8, out_len: &mut u32);
	pub fn trou_debug(data: *const u8, len: u32);
}

pub fn debug(s: &str) {
	unsafe { trou_debug(s.as_ptr(), s.len() as u32) };
}

// target FFI
#[no_mangle]
pub extern "C" fn trou_init_base_ctx() -> *const BaseCtx {
	unsafe { leak_opaque(BaseCtx::new()) }
}

#[no_mangle]
pub extern "C" fn trou_init_target_ctx(target: *mut u8, len: u32) -> *const TargetCtx {
	unsafe { leak_opaque(TargetCtx::new(SizedPtr::wrap(target, len).to_str().to_owned())) }
}

#[no_mangle]
pub extern "C" fn trou_deinit_base_ctx(ctx: &'static mut BaseCtx) {
	drop(unsafe { Box::from_raw(ctx) })
}


#[repr(C)]
pub struct TargetCtx {
	target: String,
	// build_dir: String,
	// build_fns: Vec<BuildFn>,
	// target_fns: Vec<TargetFn>,
}

// needs to be in this crate to depend on trou_invoke
impl TargetCtx {
	pub fn new(target: String) -> Self {
		Self {
			target,
			// build_fns: Default::default(),
			// target_fns: Default::default(),
		}
	}

	pub fn target(&self) -> &str { todo!() }
	pub fn invoke<'de, Response: DeserializeOwned>(call: DependencyRequest) -> Result<Response> {
		let buf = serde_json::to_vec(&call)?;
		let mut response = SizedPtr::empty();
		let response_str = unsafe {
			trou_invoke(buf.as_ptr(), buf.len() as u32, &mut response.ptr, &mut response.len);
			response.to_str()
		};
		// TODO error?
		Ok(serde_json::from_str(response_str)?)
	}

	// invoke shortcuts

	// TODO: include / exclude filters, and distinguish file / dirs?
	// pub fn listdir(&self, path: &str) -> Result<Vec<String>> {
	// 	Self::invoke::<Listdir, Vec<String>>(Listdir{ path })
	// }

	// pub fn getenv(&self, key: &str) -> Option<String> {
	// 	Self::invoke::<Getenv, Option<String>>(Getenv{ key }).unwrap()
	// }

	pub fn build(&self, path: &str) -> Result<()> {
		Self::invoke::<()>(DependencyRequest::FileDependency(path))
	}
}
