use anyhow::*;
use ambl_macros::export;

#[test]
fn export_test() {
	#[export]
	fn fn_impl(s: String) -> Result<Vec<String>> {
		Ok(vec!("Hello".to_owned(), s))
	}
	
	let in_buf = "\"world!\"".to_owned();
	let mut out_ptr: *mut u8 = std::ptr::null_mut();
	let mut out_len: u32 = 0;
	
	amblffi_fn_impl(
		in_buf.as_ptr() as *const u8, in_buf.len() as u32,
		&mut out_ptr, &mut out_len);
	let expected = "{\"Ok\":[\"Hello\",\"world!\"]}";

	let actual = unsafe { String::from_raw_parts(
		out_ptr, out_len as usize, out_len as usize) };
	assert_eq!(actual, expected);
}
