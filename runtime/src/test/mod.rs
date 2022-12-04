// test utilities
mod test_module;

// modules that define tests
mod test_deps;

#[ctor::ctor]
fn init() {
	crate::init::init_for_tests();
}
