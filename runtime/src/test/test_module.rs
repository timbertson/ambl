use log::*;
use std::{ops::{Deref, DerefMut}, path::PathBuf};
use serde::{de::DeserializeOwned, Serialize};
use core::fmt;
use std::{collections::HashMap, ops::Index, env::{current_dir, self}, rc::Rc, default::Default, sync::{Arc, Mutex, atomic::{AtomicUsize, Ordering}}, fs, path::Path};

use anyhow::*;
use tempdir::TempDir;
use trou_common::{rule::{Target, Rule, dsl, FunctionSpec}, build::{DependencyRequest, FileDependency, DependencyResponse, FileDependencyType}, ctx::{TargetCtx, Invoker, BaseCtx}};
use wasmtime::Engine;

use crate::{project::{ActiveBuildToken, ProjectHandle, ProjectRef, Project, BuildReason, PostBuild, PostBuildFile}, persist::{PersistFile, Persist, PersistDependency, BuildRequest, ResolvedFnSpec}, module::BuildModule, sync::{Mutexed, MutexHandle}, err::result_block, path_util::{Scoped, Scope, CPath, Unscoped}};

type BuilderFn = Box<dyn Fn(&TestProject, &TargetCtx) -> Result<()> + Sync + Send>;

#[derive(Clone, Default)]
pub struct Log(Arc<Mutex<Vec<String>>>);

impl Log {
	pub fn reset(&self ) -> Log {
		Log(Arc::new(Mutex::new(self.0.lock().unwrap().drain(..).collect())))
	}
	
	pub fn record<S: ToString>(&self, s: S) {
		let s = s.to_string();
		debug!("Recording event: {}", &s);
		self.0.lock().unwrap().push(s);
	}
	
	pub fn is_empty(&self) -> bool { self.0.lock().unwrap().is_empty() }

	pub fn raw(&self) -> Vec<String> { self.0.lock().unwrap().clone() }
}

impl fmt::Debug for Log {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		self.0.lock().unwrap().fmt(f)
	}
}

impl<'a> PartialEq<Vec<&'a str>> for Log {
	fn eq(&self, other: &Vec<&'a str>) -> bool {
		Iterator::eq(
			self.0.lock().unwrap().iter(),
			other.iter(),
		)
	}
}

impl<'a> PartialEq<Vec<String>> for Log {
	fn eq(&self, other: &Vec<String>) -> bool {
		let refs: Vec<&str> = other.iter().map(|s| s.as_str()).collect();
		self.eq(&refs)
	}
}

#[derive(Clone)]
pub struct TestModule<'a> {
	pub name: String,
	pub scope: Option<String>,
	pub project: &'a TestProject<'a>,
	module_path: Unscoped,
	_rules: Vec<Rule>,
	rule_fn: Option<fn(&TestModule<'a>, &BaseCtx) -> Vec<Rule>>,
	builders: Arc<HashMap<String, BuilderFn>>,
}

pub const DEFAULT_BUILD_FN: &'static str = "build";

impl<'a> TestModule<'a> {
	pub fn new(project: &'a TestProject<'a>) -> Self {
		let name = project.next_module_name();
		Self {
			// NOTE: these get replaced by set_name below
			name: "".to_owned(),
			module_path: Unscoped::new("".to_owned()),

			project,
			scope: Default::default(),
			_rules: Default::default(),
			rule_fn: Default::default(),
			builders: Default::default()
		}.set_name(name)
	}
	
	pub fn builder<F: Fn(&TestProject, &TargetCtx) -> Result<()> + 'static + Sync + Send>(mut self, f: F) -> Self
	{
		Arc::get_mut(&mut self.builders).expect("builder()").insert(DEFAULT_BUILD_FN.to_string(), Box::new(f));
		self
	}

	pub fn set_name<S: ToString>(mut self, v: S) -> Self {
		self.name = v.to_string();
		self.module_path = Unscoped::new(v.to_string());
		self
	}

	pub fn set_scope<S: ToString>(mut self, v: S) -> Self {
		self.scope = Some(v.to_string());
		self
	}

	pub fn rule(mut self, v: Rule) -> Self {
		self._rules.push(v);
		self
	}

	pub fn rules(&self, ctx: &BaseCtx) -> Vec<Rule> {
		if let Some(ref f) = self.rule_fn {
			f(self, ctx)
		} else {
			self._rules.clone()
		}
	}
	
	pub fn default_build_fn(&self) -> FunctionSpec {
		dsl::build_fn(DEFAULT_BUILD_FN).module(&self.name)
	}

	pub fn rule_fn(mut self, f: fn(&TestModule<'a>, &BaseCtx) -> Vec<Rule>) -> Self {
		self.rule_fn = Some(f);
		self
	}
}

impl<'a> BuildModule for TestModule<'a> {
	type Compiled = Self;

	fn compile(engine: &Engine, path: &Unscoped) -> Result<Self::Compiled> {
		panic!("compilation requested for module {}", path)
	}

	fn load(engine: &Engine, module: &Self::Compiled, project: ProjectRef<Self>) -> Result<Self> {
		Ok(module.to_owned())
	}

	fn call<Ctx: serde::Serialize>(&mut self, f: &ResolvedFnSpec, arg: &Ctx, _unlocked_evidence: &ProjectHandle<Self>) -> Result<Vec<u8>> {
		if f.fn_name == "get_rules" {
			let mut ctx: BaseCtx = serde_json::from_slice(&serde_json::to_vec(arg)?)?;
			TestInvoker::wrap(self.project, &self.module_path, &f.scope, &mut ctx, |ctx| {
				Ok(self.rules(ctx))
			})
		} else {
			// go to and from JSON so I can have an owned version (and avoid unsafe casts)
			let mut ctx: TargetCtx = serde_json::from_slice(&serde_json::to_vec(arg)?)?;

			let path = ctx.target();
			let token = ctx.token;
			debug!("running builder for {} with token {:?}", path, token);
			let fn_name = &f.fn_name;
			let f_impl = self.builders.get(fn_name)
				.ok_or_else(|| anyhow!("no such builder {:?} in {:?}", fn_name, &self.builders.keys()))?;

			TestInvoker::wrap(self.project, &self.module_path, &f.scope, &mut ctx, |ctx| {
				f_impl(self.project, &*ctx)
			})?;
			Ok(serde_json::to_vec(&())?)
		}
	}
}

#[derive(Clone, Copy)]
struct BuildState<'a, 'b, 'c> {
	project: &'a TestProject<'a>,
	module: &'b Unscoped,
	scope: &'c Scope,
}

lazy_static::lazy_static! {
	static ref INVOKE_REFERENCES
		: Arc<Mutex<HashMap<ActiveBuildToken, BuildState<'static, 'static, 'static>>>>
		= Arc::new(Mutex::new(HashMap::new()));
}

#[derive(Clone)]
struct TestInvoker {
	token: ActiveBuildToken,
}
impl TestInvoker {
	fn wrap<'a, 'b, 'c, R: Serialize, C: AsRef<BaseCtx> + AsMut<BaseCtx>, F: FnOnce(&C) -> Result<R>>(
		project: &'a TestProject<'a>,
		module: &'b Unscoped,
		scope: &'c Scope,
		ctx: &mut C,
		f: F
	) -> Result<Vec<u8>> {
		let ctx_mut = ctx.as_mut();
		let token = ActiveBuildToken::from_raw(ctx_mut.token);
		ctx_mut._override_invoker(Box::new(TestInvoker { token }));
		
		let state = BuildState {
			module,
			project,
			scope,
		};

		// we can pretend it's 'static, because it never lasts in the map longer than 'a
		// in practice it's only needed while we call `f`
		let static_state = unsafe { std::mem::transmute::<
			BuildState<'_, '_, '_>,
			BuildState<'static, 'static, 'static>
		>(state) };

		let arc = Arc::clone(&INVOKE_REFERENCES);
		let mut map = arc.lock().unwrap();
		let old = map.insert(token, static_state);
		drop(map);

		debug!("invoker::wrap({:?})", token);
		let result = f(&*ctx);

		let mut map = arc.lock().unwrap();
		match old {
			Some(old) => { map.insert(token, old); },
			None => { map.remove(&token); },
		}
		Ok(serde_json::to_vec(&result?)?)
	}
}
impl Invoker for TestInvoker {
	fn invoke(&self, request: DependencyRequest) -> Result<DependencyResponse> {
		let arc = Arc::clone(&INVOKE_REFERENCES);
		let map = arc.lock().unwrap();
		let build_state = map.get(&self.token)
			.ok_or_else(|| format!("No invoke reference found for token {:?}", &self.token))
			.unwrap()
			.to_owned();
		let BuildState { project, module, scope } = build_state;
		let project = project.lock();
		let (build_request, post_build) = BuildRequest::from(request, Some(module), &scope)?;
		debug!("TestInvoker building {:?}", &build_request);
		let (project, dep) = Project::build(project, &build_request, &BuildReason::Dependency(self.token))?;
		dep.into_response(&project, &post_build)
	}
}

pub struct TestProject<'a> {
	// Project mutates rules at it loads them, so we store a pristine copy
	rules: Mutex<Vec<Rule>>,

	project: ProjectRef<TestModule<'a>>,
	handle: ProjectHandle<TestModule<'a>>,
	root: TempDir,
	log: Log,
	module_count: AtomicUsize,
	monotonic_clock: AtomicUsize,
}

const FAKE_FILE: PersistFile = PersistFile { mtime: 0, target: None };

impl<'a> TestProject<'a> {
	fn reset(&self) {
		let mut p = self.lock();
		let rules = self.rules.lock().unwrap();
		p.replace_rules(rules.clone());

		p.cache_mut().invalidate_if(|dep| {
			match dep.as_file() {
				Some(f) if f == &FAKE_FILE => false,
				_ => true,
			}
		});
	}

	fn new() -> Result<Self> {
		let root = TempDir::new("troutest")?;
		// silly mac has a /tmp symlink
		let root_abs = fs::canonicalize(root.path())?;
		let project = Project::new(CPath::new(root_abs.to_str().unwrap().to_owned()).into_absolute()?)?;

		let handle = project.handle();
		let module_count = AtomicUsize::new(0);
		let monotonic_clock = AtomicUsize::new(1);
		let log = Default::default();
		let rules = Default::default();
		let s = Self { project, handle, root, log, module_count, monotonic_clock, rules };
		s.lock().replace_rules(vec!());
		Ok(s)
	}

	pub fn in_tempdir<F, R>(f: F) -> Result<R>
		where for<'b> F: FnOnce(&'b TestProject<'b>) -> Result<R>
	{
		let tp = TestProject::new()?;
		env::set_current_dir(tp.root.path())?;
		debug!("TestProject running in {:?}", tp.root.path());

		f(&tp)
	}

	pub fn new_module(&'a self) -> TestModule<'a> {
		TestModule::<'a>::new(self)
	}

	fn lock<'b>(&'b self) -> Mutexed<'b, Project<TestModule<'a>>>
		where 'a : 'b // project liftime outlives lock lifetime
	{
		unsafe { self.handle.unsafe_lock("tests").expect("lock() failed") }
	}

	pub fn with_lock<'b, R, F: FnOnce(Mutexed<'b, Project<TestModule<'a>>>) -> R>(&'b self, f: F) -> R
		where 'a : 'b // project liftime outlives lock lifetime
	{
		f(self.lock())
	}
	
	fn next_module_name(&self) -> String {
		format!("mod-{}", self.module_count.fetch_add(1, Ordering::Relaxed))
	}
	
	// all-in one: adds an anonymous module for this rule, and pushes a rule
	pub fn target_builder<S: ToString, F: Fn(&TestProject, &TargetCtx) -> Result<()> + 'static + Sync + Send>
		(&'a self, target_name: S, f: F) -> &Self
	{
		let target_name = target_name.to_string();
		let m = self.new_module().builder(f);
		self
			.inject_rule(dsl::target(&target_name, dsl::build_via(&m.name, DEFAULT_BUILD_FN)))
			.inject_module(m)
	}
	
	// low-level module / rule modification
	pub fn inject_rule(&self, v: Rule) -> &Self {
		let mut r = self.rules.lock().unwrap();
		r.push(v);
		let mut p = self.lock();
		p.replace_rules(r.clone());
		drop(p);
		self
	}

	pub fn inject_rules_module(&self, m: TestModule<'a>) -> &Self {
		let mut mod_rule = dsl::module(&m.name);
		if let Some(ref scope) = m.scope {
			mod_rule = mod_rule.scope(scope);
		}
		self.inject_rule(dsl::include(mod_rule));
		self.inject_module(m)
	}

	pub fn inject_module(&self, v: TestModule<'a>) -> &Self {
		let mut p = self.lock();
		let s = v.name.to_owned();
		p.inject_module(&s, v);
		// mark the module file as fresh to skip having to write an actual file
		p.inject_cache(
			BuildRequest::FileDependency(Unscoped::new(s)),
			Persist::File(Some(FAKE_FILE.clone()))
		).expect("inject_module");
		drop(p);
		self
	}

	pub fn touch_fake<S: Into<String>>(&self, v: S) -> &Self {
		let mut p = self.lock();
		let stat = PersistFile {
			mtime: self.monotonic_clock.fetch_add(1, Ordering::Relaxed) as u128,
			target: None,
		};
		p.inject_cache(
			BuildRequest::FileDependency(Unscoped::new(v.into())),
			Persist::File(Some(stat))
		).expect("touch_fake");
		drop(p);
		self
	}

	pub fn build_file(&self, f: &str) -> Result<&Self> {
		self.build_file_as(f, FileDependencyType::Unit)?;
		Ok(self)
	}

	pub fn build_file_contents(&self, f: &str) -> Result<String> {
		self.build_file_as(f, FileDependencyType::Contents)?.try_into()
	}

	pub fn build_file_as(&self, f: &str, ret: FileDependencyType) -> Result<DependencyResponse> {
		let project = self.lock();
		println!("\n=== start build_file({})", f);
		let path = Unscoped::new(f.to_owned());
		let req = BuildRequest::FileDependency(path.clone());
		let post_build = PostBuild::FileDependency(PostBuildFile { path, ret });
		let (project, result) = Project::build(project, &req, &BuildReason::Explicit)
			.with_context(|| format!("Building {:?}", &req))?;

		let response = result.into_response(&project, &post_build);
		drop(project);
		println!("=== end build_file({})\n", f);

		// testcases run multiple builds, make sure we don't short-circuit between them
		self.reset();

		response
	}

	pub fn write_file<F: AsRef<Path>, S: AsRef<[u8]>>(&self, path: F, contents: S) -> Result<&Self> {
		debug!("Testcase is writing file {} within {}",
			path.as_ref().display(),
			env::current_dir()?.display()
		);
		fs::write(path, contents)?;
		Ok(self)
	}

	pub fn log(&self) -> Log {
		self.log.clone()
	}

	pub fn record<S: ToString>(&self, s: S) {
		self.log.record(s)
	}
}
