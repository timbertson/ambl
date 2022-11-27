// extern shims required by guests, plus a reexport of various trou_common APIs

pub use trou_common::build::*;
pub use trou_common::rule::*;
pub use trou_common::rule::dsl::*;
pub use trou_common::ffi::*;
pub use trou_common::ctx::*;

// used to detect incompatible guest modules
#[no_mangle]
pub extern "C" fn trou_api_version() -> usize {
	1
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
