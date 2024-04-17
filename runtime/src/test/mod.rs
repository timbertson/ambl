// test utilities
mod util;
mod test_module;

// modules that define tests
mod test_deps;
mod test_paths;
mod test_exec;
mod test_checksum;
mod test_dsl;

#[ctor::ctor]
fn init() {
	crate::init::init_for_tests();
}
