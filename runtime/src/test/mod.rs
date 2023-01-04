// test utilities
mod util;
mod test_module;

// modules that define tests
mod test_deps;
mod test_paths;
mod test_exec;

#[ctor::ctor]
fn init() {
	crate::init::init_for_tests();
}
