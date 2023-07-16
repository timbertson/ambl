// extern shims required by guests, plus a reexport of various ambl_common APIs

pub use ambl_common::build::*;
pub use ambl_common::rule::*;
pub use ambl_common::rule::dsl::*;
pub use ambl_common::ffi::*;
pub use ambl_common::ctx::*;
pub use ambl_macros::*;
use log::*;
use ambl_common::LogLevel;

// #[macro_use]
// extern crate ambl_common;

pub use ambl_common::ambl_export_builder_raw;

// // used to detect incompatible guest modules
// #[no_mangle]
// pub extern "C" fn ambl_api_version() -> usize {
// 	1
// }

static AMBL_LOGGER: AmblLogger = AmblLogger;

// #[no_mangle]
pub fn ambl_init(level_int: u8) {
	let level = LogLevel::from_int(level_int);
	log::set_max_level(level.to_level_filter());
	log::set_logger(&AMBL_LOGGER).unwrap();
}

// #[no_mangle]
// pub extern "C" fn ambl_alloc(len: u32) -> *mut u8 {
// 	let mut buf = Vec::with_capacity(len as usize);
// 	let ptr = buf.as_mut_ptr();
// 	std::mem::forget(buf);
// 	ptr
// }

// #[no_mangle]
// pub extern "C" fn ambl_free(ptr: *mut u8, len: u32) {
// 	let size = len as usize;
// 	let data = unsafe { Vec::from_raw_parts(ptr, size, size) };
// 	std::mem::drop(data);
// }

struct AmblLogger;
impl log::Log for AmblLogger {
	fn enabled(&self, _metadata: &Metadata) -> bool {
		true
	}

	fn log(&self, record: &Record) {
		let s = format!("{}", &record.args());
		ambllog(LogLevel::to_int(record.level()), &s);
	}

	fn flush(&self) {
		// noop
	}
}
