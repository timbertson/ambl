// extern shims required by guests, plus a reexport of various ambl_common APIs

pub use ambl_common::build::*;
pub use ambl_common::rule::*;
pub use ambl_common::rule::dsl::*;
pub use ambl_common::ffi::*;
pub use ambl_common::ctx::*;
pub use ambl_macros::*;
use log::*;

// used to detect incompatible guest modules
#[no_mangle]
pub extern "C" fn ambl_api_version() -> usize {
	1
}

static AMBL_LOGGER: AmblLogger = AmblLogger;

#[no_mangle]
pub extern "C" fn ambl_init(level_int: u32) {
	let level = ambl_common::LogLevel::from_int(level_int);
	log::set_max_level(level.to_level_filter());
	log::set_logger(&AMBL_LOGGER).unwrap();
}

pub fn debug(s: &str) {
	unsafe { ambl_debug(s.as_ptr(), s.len() as u32) };
}

#[no_mangle]
pub extern "C" fn ambl_alloc(len: u32) -> *mut u8 {
	let mut buf = Vec::with_capacity(len as usize);
	let ptr = buf.as_mut_ptr();
	std::mem::forget(buf);
	ptr
}

#[no_mangle]
pub extern "C" fn ambl_free(ptr: *mut u8, len: u32) {
	let size = len as usize;
	let data = unsafe { Vec::from_raw_parts(ptr, size, size) };
	std::mem::drop(data);
}

struct AmblLogger;
impl log::Log for AmblLogger {
	fn enabled(&self, _metadata: &Metadata) -> bool {
		true
	}

	fn log(&self, record: &Record) {
		debug(&format!("{}", &record.args()));
	}

	fn flush(&self) {
		// noop
	}
}
