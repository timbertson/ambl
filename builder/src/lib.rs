use anyhow::*;

pub mod api {
use anyhow::*;
use serde::Serialize;

	const PROTOCOL_VERSION: usize = 1;
	type BuildFn = Box<dyn FnOnce(&Ctx) -> Result<()>>;
	type TargetFn = Box<dyn FnOnce(&mut Ctx) -> Result<Vec<Target>>>;
	pub type TargetsFFI = Vec<u8>;

	
	#[repr(C)]
	pub struct Ctx {
		build_fns: Vec<BuildFn>,
		target_fns: Vec<TargetFn>,
	}

	impl Ctx {
		pub fn new() -> Self { Self { build_fns: Default::default(), target_fns: Default::default() } }

		pub fn target(&self) -> &str { todo!() }
		pub fn invoke<'a, C: Call<'a>>(call: C) -> Result<C::Response> { todo!() }

		pub fn encode_targets(&mut self, t: Result<Vec<Target>>) -> TargetsFFI {
			// TODO handle Err
			let ser: Vec<TargetSerialize> = t.unwrap().into_iter().map(|t| {
				TargetSerialize::convert(self, t)
			}).collect();
			serde_json::to_vec(&(PROTOCOL_VERSION, ser)).unwrap()
		}

		fn register_into<T>(dest: &mut Vec<T>, value: T) -> usize {
			let idx = dest.len();
			dest.push(value);
			idx
		}

		fn register_build_fn(&mut self, f: BuildFn) -> usize {
			Self::register_into(&mut self.build_fns, f)
		}

		fn register_target_fn(&mut self, f: TargetFn) -> usize {
			Self::register_into(&mut self.target_fns, f)
		}
	}

	pub trait Call<'a> {
		type Response;
		fn as_request(&self) -> Request<'a>;
	}

	// Serialized as [PROTOCOL_VERSION, [ENUM_NAME, CONTENTS], ...]
	pub enum Request<'a> {
		Dependency(Dependency<'a>),
		Checksum(ChecksumInput<'a>),
		EnvVar(&'a str),
	}
	

	pub struct Dependency<'a> {
		relative_path: &'a str,
	}

	pub enum ChecksumInput<'a> {
		Path(&'a str),
		Contents(&'a str),
	}
	
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
		name: String,
		build: BuildRule,
	}
	
	#[derive(Serialize)]
	pub struct NamedTargetSerialize {
		name: String,
		build: BuildRuleSerialize,
	}

	pub enum BuildRule {
		WasmDelegate(String),
		Defer(TargetFn),
		Direct(BuildFn),
	}

	#[derive(Serialize)]
	pub enum BuildRuleSerialize {
		WasmDelegate(String),
		Defer(usize),
		Direct(usize),
	}
	
	impl TargetSerialize {
		fn convert(ctx: &mut Ctx, base: Target) -> Self {
			match base {
				Target::Nested(t) => TargetSerialize::Nested(t.into_iter().map(|t| {
					TargetSerialize::convert(ctx, t)
				}).collect()),
				Target::Single(NamedTarget { name, build }) => TargetSerialize::Single(NamedTargetSerialize {
					name, build: BuildRuleSerialize::convert(ctx, build)
				}),
			}
		}
	}

	impl BuildRuleSerialize {
		fn convert(ctx: &mut Ctx, base: BuildRule) -> Self {
			match base {
				BuildRule::Direct(f) => BuildRuleSerialize::Direct(ctx.register_build_fn(f)),
				BuildRule::Defer(f) => BuildRuleSerialize::Defer(ctx.register_target_fn(f)),
				BuildRule::WasmDelegate(s) => BuildRuleSerialize::WasmDelegate(s),
			}
		}
	}

	#[no_mangle]
	pub extern "C" fn init() -> &'static Ctx {
		Box::leak(Box::new(Ctx::new()))
	}

	// TODO is this actually needed? We can probably just throw away the whole module instead
	#[no_mangle]
	pub extern "C" fn deinit(ctx: &'static mut Ctx) {
		drop(unsafe { Box::from_raw(ctx) })
	}

	#[no_mangle]
	pub extern "C" fn reset(c: &mut Ctx) {
		*c = Ctx::new()
	}
	
	// target builder functions
	pub fn target<S: Into<String>, F: FnOnce(&Ctx) -> Result<()> + 'static>(s: S, f: F) -> Target {
		Target::Single(NamedTarget {
			name: s.into(),
			build: BuildRule::Direct(Box::new(f)),
		})
	}

	// pub fn targets<S: Into<String>, F: FnOnce(&Ctx) -> Result<()> + 'static>(s: Vec<S>, f: F) -> Target {
	// 	Target::Nested(s.into_iter().map(|s| {
	// 	}));
	// 		name: s.into(),
	// 		build: BuildRule::Direct(Box::new(f)),
	// 	})
	// }
	
}

use api::*;

// TODO: boilerplate
pub fn targets(c: &mut Ctx) -> TargetsFFI {
	let inner = targets_inner(c);
	c.encode_targets(inner)
}

pub fn targets_inner(c: &mut Ctx) -> Result<Vec<Target>> {
	Ok(vec!(
		target("all", |_| {
			println!("HEYO!");
			Ok(())
		}),
		// api::targets(vec!("a", "b", "c"), |c: &Ctx| {
		// 	println!("you built: {}", c.target());
		// 	Ok(())
		// }),
	))
}

pub fn greet(name: &str) {
	println!("Hello, {}!", name);
}
