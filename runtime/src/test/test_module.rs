use log::*;
use core::fmt;
use std::{collections::HashMap, ops::Index, env::{current_dir, self}, rc::Rc, default::Default, sync::{Arc, Mutex}, fs, path::Path};

use anyhow::*;
use tempdir::TempDir;
use trou_common::{rule::{Target, Rule, dsl}, build::{DependencyRequest, FileDependency, DependencyResponse}, ctx::{TargetCtx, Invoker}};
use wasmtime::Engine;

use crate::{project::{ActiveBuildToken, ProjectHandle, ProjectRef, Project, BuildReason}, persist::{PersistFile, DependencyKey, Persist}, module::BuildModule, sync::{Mutexed, MutexHandle}, err::result_block};

type BuilderFn = Box<dyn Fn(&TestProject, TargetCtx) -> Result<()> + Sync + Send>;

#[derive(Clone)]
pub struct TestModule<'a> {
	project: &'a TestProject<'a>,
	rules: Vec<Rule>,
	builders: Arc<HashMap<String, BuilderFn>>,
}

impl<'a> TestModule<'a> {
	pub fn new(project: &'a TestProject<'a>) -> Self {
		Self { project, rules: Default::default(), builders: Default::default() }
	}
	
	pub fn builder<K: ToString, F: Fn(&TestProject, TargetCtx) -> Result<()> + 'static + Sync + Send>(mut self, k: K, f: F) -> Self
	{
		Arc::get_mut(&mut self.builders).expect("builder()").insert(k.to_string(), Box::new(f));
		self
	}

	pub fn rule(mut self, v: Rule) -> Self {
		self.rules.push(v);
		self
	}
}

#[derive(Clone, Default)]
pub struct Log(Arc<Mutex<Vec<String>>>);

impl Log {
	pub fn reset(&self ) -> Log {
		Log(Arc::new(Mutex::new(self.0.lock().unwrap().drain(..).collect())))
	}
	
	pub fn record<S: ToString>(&self, s: S) {
		self.0.lock().unwrap().push(s.to_string());
	}
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

impl<'a> BuildModule for TestModule<'a> {
	type Compiled = Self;

	fn compile(engine: &Engine, path: &str) -> Result<Self::Compiled> {
		panic!("compilation requested for module {}", path)
	}

	fn load(engine: &Engine, module: &Self::Compiled, project: ProjectRef<Self>) -> Result<Self> {
		Ok(module.to_owned())
	}

	fn get_rules(
		&mut self, config: &trou_common::rule::Config
	) -> Result<Vec<Rule>> {
		Ok(self.rules.to_owned())
	}

	fn call<Ctx: serde::Serialize>(&mut self, f: &trou_common::rule::FunctionSpec, arg: &Ctx) -> Result<Vec<u8>> {
		todo!()
	}

	fn run_builder(
		&mut self,
		token: ActiveBuildToken,
		path: &str,
		builder: &Target,
		_unlocked_evidence: &ProjectHandle<Self>
	) -> Result<()> {
		debug!("running builder for {} with token {:?}", path, token);
		let fn_name = &builder.build.fn_name;
		let f = self.builders.get(fn_name)
			.ok_or_else(|| anyhow!("no such builder: {}", fn_name))?;
		let mut ctx = TargetCtx::new(path.to_owned(), None, token.raw());

		ctx._override_invoker(Box::new(TestInvoker { token }));
		TestInvoker::wrap(self.project, token, || {
			f(self.project, ctx)
		})
	}
}

lazy_static::lazy_static! {
	static ref INVOKE_REFERENCES: Arc<Mutex<HashMap<ActiveBuildToken, &'static TestProject<'static>>>> = Arc::new(Mutex::new(HashMap::new()));
}

struct TestInvoker {
	token: ActiveBuildToken,
}
impl TestInvoker {
	fn wrap<'a, R, F: FnOnce() -> R>(
		project: &'a TestProject<'a>,
		token: ActiveBuildToken,
		f: F
	) -> R {
		// we can pretend it's 'static, because we remove it from the map before 'a ends
		let static_project = unsafe { std::mem::transmute::<
			&'a TestProject<'a>,
			&'static TestProject<'static>
		>(project) };

		let arc = Arc::clone(&INVOKE_REFERENCES);
		let mut map = arc.lock().unwrap();
		let old = map.insert(token, static_project);
		drop(map);

		debug!("invoker::wrap({:?})", token);
		let result = f();

		let mut map = arc.lock().unwrap();
		match old {
			Some(old) => { map.insert(token, old); },
			None => { map.remove(&token); },
		}
		result
	}
}
impl Invoker for TestInvoker {
	fn invoke(&self, request: DependencyRequest) -> Result<DependencyResponse> {
		let arc = Arc::clone(&INVOKE_REFERENCES);
		let map = arc.lock().unwrap();
		let project = map.get(&self.token).ok_or_else(|| format!("No invoke reference found for token {:?}", &self.token)).unwrap().lock();
		drop(map);

		debug!("TestInvoker building {:?}", &request);
		let dep = Project::build(project, &request, &BuildReason::Dependency(self.token))?.1;
		dep.into_response(&request)
	}
}

pub struct TestProject<'a> {
	project: ProjectRef<TestModule<'a>>,
	handle: ProjectHandle<TestModule<'a>>,
	root: TempDir,
	log: Log,
}

const FAKE_FILE: PersistFile = PersistFile { mtime: 0 };

impl<'a> TestProject<'a> {
	fn new() -> Result<Self> {
		let project = Project::new()?;
		let handle = project.handle();
		let root = TempDir::new("troutest")?;
		let s = Self { project, handle, root, log: Default::default() };
		s.lock().replace_rules(vec!());
		Ok(s)
	}

	pub fn in_tempdir<F, R>(f: F) -> R
		where for<'b> F: FnOnce(&'b TestProject<'b>) -> R
	{
		result_block(|| {
			let tp = TestProject::new()?;

			let original_cwd = env::current_dir()?;
			env::set_current_dir(tp.root.path())?;
			debug!("TestProject running in {:?}", tp.root.path());

			let ret = f(&tp);

			env::set_current_dir(original_cwd)?;
			Ok(ret)
		}).unwrap()
	}

	pub fn new_module(&'a self) -> TestModule<'a> {
		TestModule::<'a>::new(self)
	}

	fn lock<'b>(&'b self) -> Mutexed<'b, Project<TestModule<'a>>>
		where 'a : 'b // project liftime outlives lock lifetime
	{
		unsafe { self.handle.unsafe_lock("tests").expect("lock() failed") }
	}
	
	// all-in one: adds an anonymous module for this rule, and pushes a rule
	pub fn target_builder<S: ToString, F: Fn(&TestProject, TargetCtx) -> Result<()> + 'static + Sync + Send>(&'a self, target_name: S, f: F) -> &Self {
		// including the current module size guarantees a unique module name
		// let p = self.lock();
		let mod_name = format!("anon-{}", self.lock().module_len());
		let fn_name = "build";
		let target_name = target_name.to_string();
		let m = self.new_module().builder(fn_name, f);
		self
			.push_raw_rule(dsl::target(&target_name, dsl::build_via(&mod_name, fn_name)))
			.inject_raw_module(mod_name, m)
	}
	
	// low-level module / rule modification
	pub fn push_raw_rule(&self, v: Rule) -> &Self {
		let mut p = self.lock();
		p.push_rule(v);
		drop(p);
		self
	}

	pub fn inject_raw_module<K: ToString>(&self, k: K, v: TestModule<'a>) -> &Self {
		let mut p = self.lock();
		let s = k.to_string();
		p.inject_module(k, v);
		// mark the module file as fresh to skip having to write an actual file
		p.inject_cache(
			DependencyKey::FileDependency(s),
			Persist::File(Some(FAKE_FILE.clone()))
		).expect("inject_module");
		drop(p);
		self
	}

	pub fn build_file_res(&self, f: &str) -> Result<&Self> {
		// testcases run multiple builds, make sure we don't short-circuit between them
		let mut project = self.lock();
		project.cache_mut().invalidate_if(|dep| {
			match dep.as_file() {
				Some(f) if f == &FAKE_FILE => false,
				_ => true,
			}
		});
		Project::build(
			project,
			&DependencyRequest::FileDependency(FileDependency::new(f.to_owned())),
			&BuildReason::Explicit)?;
		Ok(self)
	}

	pub fn build_file(&self, f: &str) -> &Self {
		self.build_file_res(f).expect("build_file")
	}

	pub fn write_file<F: AsRef<Path>, S: AsRef<[u8]>>(&self, path: F, contents: S) -> &Self {
		debug!("Testcase is writing file {} within {}",
			path.as_ref().display(),
			env::current_dir().unwrap().display()
		);
		fs::write(path, contents).unwrap();
		self
	}

	pub fn log(&self) -> Log {
		self.log.clone()
	}
	
	pub fn record<S: ToString>(&self, s: S) {
		self.log.record(s)
	}
}