#![allow(dead_code, unused_variables)]

use anyhow::*;


pub mod api {

use anyhow::*;
	use serde::{Serialize, de::DeserializeOwned};

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
			debug(&format!("wrote to addr {:?} value {:?}", self.ptr, *self.ptr));
			*self.len = bytes.len() as u32;
			// the bytes still reside in memory, the host will need to call trou_free to drop them
			std::mem::forget(bytes);
			Ok(())
		}
	}
	
	// Used when reading from a sized ptr (and to provide a destination for the host to write to)
	struct SizedPtr {
		ptr: *mut u8,
		len: u32
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
	
	// #[repr(transparent)]
	// pub struct SizedPtr([u32; 2]);
	// impl SizedPtr {
	// 	pub fn empty() -> Self {
	// 		Self([0, 0])
	// 	}

	// 	pub fn wrap(ptr: *const u8, len: u32) -> Self {
	// 		Self([ptr as u32, len])
	// 	}

	// 	pub fn pointer(&self) -> *mut u8 {
	// 		self.0[0] as *mut u8
	// 	}

	// 	pub fn len(&self) -> u32 {
	// 		self.0[1]
	// 	}

	// 	pub unsafe fn to_str<'a>(&self) -> &'a str {
	// 		let slice = std::slice::from_raw_parts::<'a>(self.pointer(), self.len() as usize);
	// 		std::str::from_utf8(slice).unwrap()
	// 	}
		
	// 	pub fn write_and_leak(&mut self, bytes: Vec<u8>) -> Result<()> {
	// 		self.0[0] = bytes.as_ptr() as u32;
	// 		self.0[1] = bytes.len() as u32;
	// 		std::mem::forget(bytes);
	// 		Ok(())
	// 	}
	// }

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

	unsafe fn leak_opaque<T>(t: T) -> *const T {
		Box::leak(Box::new(t))
	}

	const PROTOCOL_VERSION: usize = 1;
	// type BuildFn = Box<dyn FnOnce(&Ctx) -> Result<()>>;
	// type TargetFn = Box<dyn FnOnce(&mut Ctx) -> Result<Vec<Target>>>;
	pub type TargetsFFI = Vec<u8>;
	
	pub struct BaseCtx {
	}
	impl BaseCtx {
		pub fn new() -> Self { Self {} }

		pub fn encode_targets(&mut self, t: Result<Vec<Target>>, mut out: SizedPtrRef) -> Result<()> {
			// TODO handle Err
			let ser: Vec<TargetSerialize> = t.unwrap().into_iter().map(|t| {
				TargetSerialize::convert(self, t)
			}).collect();
			let bytes = serde_json::to_vec(&ser).unwrap();
			debug(&serde_json::to_string(&ser).unwrap());
			out.write_and_leak(bytes)
		}

	}

	#[repr(C)]
	pub struct TargetCtx {
		target: String,
		// build_dir: String,
		// build_fns: Vec<BuildFn>,
		// target_fns: Vec<TargetFn>,
	}

	impl TargetCtx {
		pub fn new(target: String) -> Self {
			Self {
				target,
				// build_fns: Default::default(),
				// target_fns: Default::default(),
			}
		}

		pub fn target(&self) -> &str { todo!() }
		pub fn invoke<'de, C: Serialize, Response: DeserializeOwned>(call: C) -> Result<Response> {
			let buf = serde_json::to_vec(&call)?;
			let mut response = SizedPtr::empty();
			let response_str = unsafe {
				trou_invoke(buf.as_ptr(), buf.len() as u32, &mut response.ptr, &mut response.len);
				response.to_str()
			};
			// TODO error?
			Ok(serde_json::from_str(response_str)?)
		}

		// fn register_into<T>(dest: &mut Vec<T>, value: T) -> usize {
		// 	let idx = dest.len();
		// 	dest.push(value);
		// 	idx
		// }

		// fn register_build_fn(&mut self, f: BuildFn) -> usize {
		// 	Self::register_into(&mut self.build_fns, f)
		// }

		// fn register_target_fn(&mut self, f: TargetFn) -> usize {
		// 	Self::register_into(&mut self.target_fns, f)
		// }
		
		// invoke shortcuts

		// TODO: include / exclude filters, and distinguish file / dirs?
		pub fn listdir(&self, path: &str) -> Result<Vec<String>> {
			Self::invoke::<Listdir, Vec<String>>(Listdir{ path })
		}

		pub fn getenv(&self, key: &str) -> Option<String> {
			Self::invoke::<Getenv, Option<String>>(Getenv{ key }).unwrap()
		}

		pub fn build(&self, path: &str) -> Result<()> {
			Self::invoke::<Dependency, ()>(Dependency { path })
		}
	}

	#[derive(Serialize)]
	struct Listdir<'a> { path: &'a str }
	
	#[derive(Serialize)]
	struct Getenv<'a> { key: &'a str }
	
	#[derive(Serialize)]
	pub struct Dependency<'a> { path: &'a str }

	pub enum Target {
		Nested(Vec<Target>),
		Single(NamedTarget),
	}

	#[derive(Serialize)]
	pub enum TargetSerialize {
		Nested(Vec<TargetSerialize>),
		Single(NamedTargetSerialize),
	}

	pub struct NamedTarget {
		names: Vec<String>,
		build: BuildRule,
	}
	
	#[derive(Serialize)]
	pub struct NamedTargetSerialize {
		names: Vec<String>,
		build: BuildRuleSerialize,
	}
	
	pub struct Entrypoint {
		module: Option<String>,
		entrypoint: Option<String>,
		args: Option<String>,
	}

	pub enum BuildRule {
		Run(Entrypoint),
		Delegate(Entrypoint), // TODO it makes no sense to pass args to a delegate
	}

	#[derive(Serialize)]
	pub enum BuildRuleSerialize {
		WasmDelegate(String),
		Defer(usize),
		Direct(usize),
	}
	
	impl TargetSerialize {
		fn convert(ctx: &mut BaseCtx, base: Target) -> Self {
			match base {
				Target::Nested(t) => TargetSerialize::Nested(t.into_iter().map(|t| {
					TargetSerialize::convert(ctx, t)
				}).collect()),
				Target::Single(NamedTarget { names, build }) => TargetSerialize::Single(NamedTargetSerialize {
					names, build: BuildRuleSerialize::convert(ctx, build)
				}),
			}
		}
	}

	impl BuildRuleSerialize {
		fn convert(ctx: &mut BaseCtx, base: BuildRule) -> Self {
			todo!()
			// match base {
			// 	BuildRule::Direct(f) => BuildRuleSerialize::Direct(ctx.register_build_fn(f)),
			// 	BuildRule::Defer(f) => BuildRuleSerialize::Defer(ctx.register_target_fn(f)),
			// 	BuildRule::WasmDelegate(s) => BuildRuleSerialize::WasmDelegate(s),
			// }
		}
	}

	// host <-> guest API

	#[no_mangle]
	pub extern "C" fn trou_init_base_ctx() -> *const BaseCtx {
		unsafe { leak_opaque(BaseCtx::new()) }
	}

	#[no_mangle]
	pub extern "C" fn trou_init_target_ctx(target: *mut u8, len: u32) -> *const TargetCtx {
		unsafe { leak_opaque(TargetCtx::new(SizedPtr::wrap(target, len).to_str().to_owned())) }
	}

	#[no_mangle]
	pub extern "C" fn trou_deinit_base_ctx(ctx: &'static mut BaseCtx) {
		drop(unsafe { Box::from_raw(ctx) })
	}

	#[no_mangle]
	pub extern "C" fn trou_api_version() -> usize {
		PROTOCOL_VERSION
	}
	
	extern {
		fn trou_invoke(data: *const u8, len: u32, out: &mut *mut u8, out_len: &mut u32);
		fn trou_debug(data: *const u8, len: u32);
	}

	// target builder functions
	pub fn target<S: Into<String>, S2: Into<String>>(s: S, entrypoint: S2) -> Target {
		Target::Single(NamedTarget {
			names: vec!(s.into()),
			build: BuildRule::Run(Entrypoint { module: None, entrypoint: Some(entrypoint.into()), args: None, }),
		})
	}

	pub fn targets<S: Into<String>, S2: Into<String>>(s: Vec<S>, entrypoint: S2) -> Target {
		Target::Single(NamedTarget {
			names: s.into_iter().map(|s| s.into()).collect(),
			build: BuildRule::Run(Entrypoint { module: None, entrypoint: Some(entrypoint.into()), args: None, }),
		})
	}

	// pub fn lazy_targets<S: Into<String>>(entrypoint: S) -> Target {
	// 	Target::Single(NamedTarget {
	// 		names: Vec::new(),
	// 		build: BuildRule::Delegate(Entrypoint { module: None, entrypoint: Some(entrypoint.into()), args: None, }),
	// 	})
	// }

	// pub fn lazy<S: Into<String>, S2: Into<String>>(s: S, entrypoint: S) -> Target {
	// 	Target::Single(NamedTarget {
	// 		names: vec!(s.into()),
	// 		build: BuildRule::Delegate(Entrypoint { module: None, entrypoint: Some(entrypoint.into()), args: None, }),
	// 	})
	// }
	
	pub fn debug(s: &str) {
		unsafe { trou_debug(s.as_ptr(), s.len() as u32) };
	}

}

use api::*;

// TODO: boilerplate via macro / derive?
#[no_mangle]
pub extern "C" fn targets_ffi(c: &mut BaseCtx, out: &mut *mut u8, len: &mut u32) {
	debug("HI!");
	let inner = targets_inner(c);
	c.encode_targets(inner, SizedPtrRef::wrap(out, len)).unwrap()
}


pub extern "C" fn build_all(c: &mut TargetCtx) -> Result<()> {
	debug(&format!("Building {}", c.target()));
	c.build("a")?;
	Ok(())
}

// pub extern "C" fn platform_targets(c: &mut TargetCtx) -> Result<Vec<Target>> {
// 	Ok(vec!(
// 		targets(vec!("x86", "aarch64"), |c: &Ctx| {
// 			println!("you built: {}", c.target());
// 			Ok(())
// 		}),
// 	))
// }

pub fn targets_inner(_: &mut BaseCtx) -> Result<Vec<Target>> {
	Ok(vec!(
		target("all", "build_all"),
		targets(vec!("a", "b", "c"), "build_all"),

		// lazy("platform", |_: &mut Ctx| {
		// 	Ok(vec!(
		// 		targets(vec!("x86", "aarch64"), |c: &Ctx| {
		// 			println!("you built: {}", c.target());
		// 			Ok(())
		// 		}),
		// 	))
		// }),

		// lazy_targets(|_: &mut Ctx| {
		// 	Ok(vec!(
		// 		targets(vec!("d", "e", "f"), |c: &Ctx| {
		// 			println!("you built: {}", c.target());
		// 			Ok(())
		// 		}),
		// 	))
		// }),
	))
}


// declarative build API
pub struct SampleOpts {
	debug: bool,
}

// TODO wrapper which takes String and invokes with SampleBuild
pub fn sample_build(ctx: &mut TargetCtx, opts: SampleOpts) -> Result<()> {
	Ok(())
}
