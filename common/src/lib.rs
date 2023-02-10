// API types shared by host + guest
pub mod build;
pub mod rule;
pub mod ffi;
pub mod ctx;

use log::*;
pub struct LogLevel;
impl LogLevel {
	pub fn from_int(i: u32) -> log::Level {
		match i {
			1 => Level::Error,
			2 => Level::Warn,
			3 => Level::Info,
			4 => Level::Debug,
			5 => Level::Trace,
			_ => panic!("Inavlid log level: {}", i),
		}
	}

	pub fn to_int(level: log::Level) -> u32 {
		match level {
			Level::Error => 1,
			Level::Warn => 2,
			Level::Info => 3,
			Level::Debug => 4,
			Level::Trace => 5,
		}
	}
}
