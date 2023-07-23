use log::LevelFilter;

use crate::ui;

fn init_(writer: ui::Writer, is_test: bool, default: &'static str) {
	let env = env_logger::Env::new().default_filter_or(default);
	let target: env_logger::fmt::Target = env_logger::fmt::Target::Pipe(Box::new(writer));
	let log_builder = env_logger::builder()
		.is_test(is_test)
		.parse_env(env)
		.filter_module("cranelift_codegen", LevelFilter::Info)
		.filter_module("wasmtime_cranelift", LevelFilter::Info)
		.target(target)
		.init();
}

pub fn init(writer: ui::Writer, verbose: bool) {
	init_(writer, false, if verbose { "debug" } else { "info" })
}

pub fn init_for_tests() {
	init_(panic!(), true, "debug")
}
