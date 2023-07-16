// extern shims required by guests, plus a reexport of various ambl_common APIs

pub use ambl_common::build::*;
pub use ambl_common::rule::*;
pub use ambl_common::rule::dsl::*;
pub use ambl_common::ffi::*;
pub use ambl_common::ctx::*;
pub use ambl_macros::*;
use log::*;
use ambl_common::LogLevel;

pub use ambl_common::ambl_export_builder_raw;
pub use ambl_macros::export;

static AMBL_LOGGER: AmblLogger = AmblLogger;

pub fn ambl_init(level_int: u8) {
	let level = LogLevel::from_int(level_int);
	log::set_max_level(level.to_level_filter());
	log::set_logger(&AMBL_LOGGER).unwrap();
}

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
