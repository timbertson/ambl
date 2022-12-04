use log::LevelFilter;

fn init_(is_test: bool, default: &'static str) {
	let env = env_logger::Env::new().default_filter_or(default);
	env_logger::builder()
		.is_test(is_test)
		.parse_env(env)
		.filter_module("cranelift_codegen", LevelFilter::Info)
		.filter_module("wasmtime_cranelift", LevelFilter::Info)
		.init();
}

pub fn init() {
	init_(false, "info")
}

pub fn init_for_tests() {
	init_(true, "debug")
}
