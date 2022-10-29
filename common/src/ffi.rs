use anyhow::*;
use serde::Serialize;

// Used when writing to a sized ptr that the host provides
pub struct SizedPtrRef<'a> {
	ptr: &'a mut *mut u8,
	len: &'a mut u32
}
impl<'a> SizedPtrRef<'a> {
	pub fn wrap(ptr: &'a mut *mut u8, len: &'a mut u32) -> Self {
		Self { ptr, len }
	}

	pub fn write_and_leak(&mut self, bytes: Vec<u8>) -> Result<()> {
		*self.ptr = bytes.as_ptr() as *mut u8;
		// debug(&format!("wrote to addr {:?} value {:?}", self.ptr, *self.ptr));
		*self.len = bytes.len() as u32;
		// the bytes still reside in memory, the host will need to call trou_free to drop them
		std::mem::forget(bytes);
		Ok(())
	}
}

// Used when reading from a sized ptr (and to provide a destination for the host to write to)
pub struct SizedPtr {
	pub ptr: *mut u8,
	pub len: u32
}

impl SizedPtr {
	pub fn empty() -> Self {
		Self { ptr: 0 as *mut u8, len: 0 }
	}

	pub fn wrap(ptr: *const u8, len: u32) -> SizedPtr {
		Self { ptr: ptr as *mut u8, len }
	}
	
	// pub fn as_ref<'a>(&'a mut self) -> SizedPtrRef<'a> {
	// 	SizedPtrRef::wrap(&mut (self.ptr as *const u8), &mut self.len)
	// }

	pub unsafe fn to_str<'a>(&'a self) -> &'a str {
		let slice = std::slice::from_raw_parts::<'a>(self.ptr, self.len as usize);
		std::str::from_utf8(slice).unwrap()
	}
}

pub unsafe fn leak_opaque<T>(t: T) -> *const T {
	Box::leak(Box::new(t))
}

#[derive(Serialize)]
pub enum ResultFFI<T: Serialize> {
	Ok(T),
	Err(String),
}

impl<T: Serialize> From<Result<T>> for ResultFFI<T> {
	fn from(r: Result<T>) -> Self {
		match r {
			Result::Ok(t) => ResultFFI::Ok(t),
			Result::Err(e) => ResultFFI::Err(format!("{}", e)),
		}
	}
}
