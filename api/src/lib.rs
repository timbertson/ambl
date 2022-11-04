// extern shims required by guests, plus a reexport of trou_common

use serde::{Serialize, Deserialize};
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

#[no_mangle]
pub extern "C" fn trou_alloc(len: u32) -> *mut u8 {
	let mut buf = Vec::with_capacity(len as usize);
	let ptr = buf.as_mut_ptr();
	std::mem::forget(buf);
	ptr
}

#[no_mangle]
pub extern "C" fn trou_free(ptr: *mut u8, len: u32) {
	let size = len as usize;
	let data = unsafe { Vec::from_raw_parts(ptr, size, size) };
	std::mem::drop(data);
}

#[derive(Serialize, Deserialize)]
#[repr(transparent)]
pub struct TargetCtx(RawTargetCtx);

// needs to be in this crate to depend on trou_invoke
impl TargetCtx {
	pub fn new(target: String) -> Self {
		Self(RawTargetCtx::new(target))
	}

	pub fn target(&self) -> &str { &self.0.target }

	pub fn invoke<'de, Response: DeserializeOwned>(call: DependencyRequest) -> Result<Response> {
		let buf = serde_json::to_vec(&call)?;
		let mut response = SizedPtr::empty();
		let response_slice = unsafe {
			trou_invoke(buf.as_ptr(), buf.len() as u32, &mut response.ptr, &mut response.len);
			response.to_slice()
		};
		let result = ResultFFI::deserialize(response_slice);
		// debug(format!("invoke result: {:?}", result));
		result
	}

	// invoke shortcuts
	pub fn build(&self, path: &str) -> Result<DependencyResponse> {
		Self::invoke(DependencyRequest::FileDependency(path))
	}
}
