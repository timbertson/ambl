use anyhow::*;

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

#[no_mangle]
pub extern "C" fn trou_alloc(len: u32) -> *mut u8 {
	let mut buf = Vec::with_capacity(len as usize);
	let ptr = buf.as_mut_ptr();
	std::mem::forget(buf);
	ptr
}

#[no_mangle]
pub extern "C" fn trou_free(ptr: *mut u8, len: u32) {
	let size = len as usize;
	let data = unsafe { Vec::from_raw_parts(ptr, size, size) };
	std::mem::drop(data);
}

pub unsafe fn leak_opaque<T>(t: T) -> *const T {
	Box::leak(Box::new(t))
}

// type BuildFn = Box<dyn FnOnce(&Ctx) -> Result<()>>;
// type TargetFn = Box<dyn FnOnce(&mut Ctx) -> Result<Vec<Target>>>;
pub type TargetsFFI = Vec<u8>;
