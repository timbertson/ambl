use anyhow::*;

// handy for type inference
pub fn result<T>(v: Result<T>) -> Result<T> {
	v
}

pub fn result_block<T, F: FnOnce() -> Result<T>>(f: F) -> Result<T> {
	f()
}
