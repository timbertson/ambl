use ambl_common::build::{InvokeAction, ReadFile, FileSource};
use ambl_common::ffi::ResultFFI;
use log::*;
use std::fs::OpenOptions;
use std::os::unix;
use std::{ops::{Deref, DerefMut}, path::PathBuf};
use serde::{de::DeserializeOwned, Serialize};
use core::fmt;
use std::{collections::HashMap, ops::Index, env::{current_dir, self}, rc::Rc, default::Default, sync::{Arc, Mutex, atomic::{AtomicUsize, Ordering}}, fs, path::Path};

use anyhow::*;
use tempdir::TempDir;
use ambl_common::{rule::{Target, Rule, dsl, FunctionSpec}, build::{DependencyRequest, InvokeResponse, Invoke}, ctx::{TargetCtx, Invoker, BaseCtx}};
use wasmtime::Engine;

use crate::build::{BuildReason, TargetContext, Forced};
use crate::build_request::{ResolvedFnSpec, BuildRequest};
use crate::ctx::Ctx;
use crate::project::{ActiveBuildToken, ProjectHandle, ProjectRef, Project, Implicits};
use crate::persist::{PersistFile, BuildResult, BuildResultWithDeps, FileStat, Mtime, PersistChecksum};
use crate::module::BuildModule;
use crate::sync::{Mutexed, MutexHandle};
use crate::err::result_block;
use crate::path_util::{Embedded, Embed, CPath, Unembedded};
use crate::invoke;
use crate::ui::{Ui, Writer};

type BuilderFn = fn(&TestProject, &TargetCtx) -> Result<()>;
type CtxFn = fn(&TestProject, &BaseCtx) -> Result<()>;

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
	pub mount: Option<String>,
	pub project: &'a TestProject<'a>,
	module_path: Unembedded,
	_rules: Vec<Rule>,
	rule_fn: Option<fn(&TestModule<'a>, &BaseCtx) -> Result<Vec<Rule>>>,
	builders: Arc<HashMap<String, BuilderFn>>,
	anon_fns: Arc<HashMap<String, CtxFn>>,
}

pub const DEFAULT_BUILD_FN: &'static str = "build";

impl<'a> TestModule<'a> {
	pub fn new(project: &'a TestProject<'a>) -> Self {
		let name = project.next_module_name();
		Self {
			// NOTE: these get replaced by set_name below
			name: "".to_owned(),
			module_path: Unembedded(CPath::new_nonvirtual("".to_owned())),

			project,
			scope: Default::default(),
			mount: Default::default(),
			_rules: Default::default(),
			rule_fn: Default::default(),
			builders: Default::default(),
			anon_fns: Default::default()
		}.set_name(name)
	}
	
	pub fn builder(mut self, f: BuilderFn) -> Self
	{
		Arc::get_mut(&mut self.builders).expect("builders").insert(DEFAULT_BUILD_FN.to_string(), f);
		self
	}

	pub fn wasm_fn<S: Into<String>>(mut self, s: S, f: CtxFn) -> Self {
		Arc::get_mut(&mut self.anon_fns).expect("anon_fns").insert(s.into(), f);
		self
	}

	pub fn set_name<S: ToString>(mut self, v: S) -> Self {
		self.name = v.to_string();
		self.module_path = Unembedded(CPath::new_nonvirtual(v.to_string()));
		self
	}

	pub fn set_scope<S: ToString>(mut self, v: S) -> Self {
		self.scope = Some(v.to_string());
		self
	}

	pub fn set_mount<S: ToString>(mut self, v: S) -> Self {
		self.mount = Some(v.to_string());
		self
	}

	pub fn rule(mut self, v: Rule) -> Self {
		self._rules.push(v);
		self
	}

	pub fn rules(&self, ctx: &BaseCtx) -> Result<Vec<Rule>> {
		if let Some(ref f) = self.rule_fn {
			f(self, ctx)
		} else {
			Ok(self._rules.clone())
		}
	}
	
	pub fn default_build_fn(&self) -> FunctionSpec {
		dsl::module(&self.name).function(DEFAULT_BUILD_FN)
	}

	pub fn rule_fn(mut self, f: fn(&TestModule<'a>, &BaseCtx) -> Result<Vec<Rule>>) -> Self {
		self.rule_fn = Some(f);
		self
	}
}

impl<'a> BuildModule for TestModule<'a> {
	type Compiled = Self;

	fn compile(engine: &Engine, path: &Unembedded) -> Result<Self::Compiled> {
		panic!("compilation requested for module {}", path)
	}

	fn load(engine: &Engine, module: &Self::Compiled, project: ProjectRef<Self>) -> Result<Self> {
		Ok(module.to_owned())
	}

	fn build(&mut self, implicits: &Implicits, f: &ResolvedFnSpec, arg: &Ctx, _unlocked_evidence: &ProjectHandle<Self>) -> Result<String> {
		let fn_name = &f.fn_name;
		match arg {
			Ctx::Base(base_ctx) => {
				let ctx: BaseCtx = serde_json::from_str(&arg.json_string()?)?;
				if fn_name == "get_rules" {
					TestInvoker::wrap(self.project, implicits, &self.module_path, &f.embed, &mut Ctx::Base(ctx), |ctx| {
						self.rules(ctx.into_base_mut())
					})
				} else if let Some(f_impl) = self.anon_fns.get(fn_name) {
					TestInvoker::wrap(self.project, implicits, &self.module_path, &f.embed, &mut Ctx::Base(ctx), |ctx| {
						f_impl(self.project, ctx.into_base_mut())
					})
				} else {
					Err(anyhow!("No anon function defined: {}\nanons: {:?}",
						fn_name, &self.anon_fns.keys()
					))
				}
			},
			Ctx::Target(target_ctx, tmp_dest_path) => {

				// go to and from JSON so I can have an owned version (and avoid unsafe casts)
				let serialized = arg.json_string()?;
				let target_ctx: TargetCtx = serde_json::from_slice(&serialized.as_bytes())
					.with_context(|| format!("deserializing {:?}", &serialized))?;
				let target_name = target_ctx.target().to_owned();
				let token = target_ctx.token;
				let mut ctx = Ctx::Target(target_ctx, tmp_dest_path.clone());

				if let Some(f_impl) = self.builders.get(fn_name) {
					debug!("running builder for {} with token {:?}", target_name, token);

					TestInvoker::wrap(self.project, implicits, &self.module_path, &f.embed, &mut ctx, |ctx| {
						f_impl(self.project, ctx.into_target_mut()?)
					})
				} else if let Some(f_impl) = self.anon_fns.get(fn_name) {
					TestInvoker::wrap(self.project, implicits, &self.module_path, &f.embed, &mut ctx, |ctx| {
						f_impl(self.project, ctx.into_target_mut()?)
					})
				} else {
					Err(anyhow!("No builder / anon function defined: {}\nbuilders: {:?}, anons: {:?}",
						fn_name, &self.builders.keys(), &self.anon_fns.keys()
					))
				}
			}
		}
	}
}

#[derive(Clone, Copy)]
struct BuildState<'a, 'b, 'c> {
	project: &'a TestProject<'a>,
	module: &'b Unembedded,
	target_context: &'c TargetContext,
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
	fn wrap<'a, 'b, 'c, R: Serialize, F: FnOnce(&mut Ctx) -> Result<R>>(
		project: &'a TestProject<'a>,
		implicits: &'b Implicits,
		module: &'b Unembedded,
		embed: &'c Embed,
		ctx: &mut Ctx,
		f: F
	) -> Result<String> {
		let token = ActiveBuildToken::from_raw(ctx.token());
		
		let invoker = Box::new(TestInvoker { token });
		let mut dest_tmp_path = None;
		match ctx {
			Ctx::Base(ref mut ctx) => {
				ctx._override_invoker(invoker);
			},
			Ctx::Target(ref mut ctx, path) => {
				dest_tmp_path = Some(path.to_owned());
				ctx.as_mut()._override_invoker(invoker);
			},
		}
		
		let target_context = TargetContext { embed: embed.clone(), implicits: implicits.clone(), dest_tmp_path };
		
		let state = BuildState {
			module,
			project,
			target_context: &target_context,
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
		let result = f(ctx);

		let mut map = arc.lock().unwrap();
		match old {
			Some(old) => { map.insert(token, old); },
			None => { map.remove(&token); },
		}
		let serialized = ResultFFI::serialize(result);
		debug!("invoker::return( {} )", &serialized);
		Ok(serialized)
	}
}
impl Invoker for TestInvoker {
	fn invoke(&self, request: Invoke) -> Result<InvokeResponse> {
		let arc = Arc::clone(&INVOKE_REFERENCES);
		let map = arc.lock().unwrap();
		let build_state = map.get(&self.token)
			.ok_or_else(|| format!("No invoke reference found for token {:?}", &self.token))
			.unwrap()
			.to_owned();
		drop(map);
		let BuildState { project, module, target_context } = build_state;
		invoke::perform(project.lock(), target_context, module, self.token, request)
	}
}

pub struct TestProject<'a> {
	// Project mutates rules at it loads them, so we store a pristine copy
	rules: Mutex<Vec<Rule>>,

	project: ProjectRef<TestModule<'a>>,
	token: ActiveBuildToken,
	handle: ProjectHandle<TestModule<'a>>,
	root: TempDir,
	log: Log,
	module_count: AtomicUsize,
	monotonic_clock: AtomicUsize,
}

const FAKE_FILE: PersistFile = PersistFile {
	stat: FileStat::File(Mtime(0)),
	target: None,
	checksum: PersistChecksum::Disabled,
};

impl<'a> TestProject<'a> {
	fn reset(&self) {
		let mut p = self.lock();
		let rules = self.rules.lock().unwrap();
		p.replace_rules(rules.clone());

		p.cache_mut().invalidate_if(|dep| {
			match dep.record.result {
				BuildResult::File(ref f) if f == &FAKE_FILE => false,
				_ => true,
			}
		});
	}

	fn new() -> Result<Self> {
		let root = TempDir::new("ambltest")?;
		let ui = Ui::test();
		// silly mac has a /tmp symlink
		let root_abs = fs::canonicalize(root.path())?;
		let project = Project::new(CPath::from_path_nonvirtual(root_abs)?.into_absolute()?, ui.writer())?;
		let token = ActiveBuildToken::generate();

		let handle = project.handle();
		let module_count = AtomicUsize::new(0);
		let monotonic_clock = AtomicUsize::new(1);
		let log = Default::default();
		let rules = Default::default();
		let s = Self { project, handle, token, root, log, module_count, monotonic_clock, rules };
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
		unsafe { self.handle.unsafe_lock_shared("tests").expect("lock() failed") }
	}

	pub fn with_lock<'b, R, F: FnOnce(Mutexed<'b, Project<TestModule<'a>>>) -> R>(&'b self, f: F) -> R
		where 'a : 'b // project liftime outlives lock lifetime
	{
		f(self.lock())
	}
	
	fn next_module_name(&self) -> String {
		format!("mod-{}.wasm", self.module_count.fetch_add(1, Ordering::Relaxed))
	}
	
	// all-in one: adds an anonymous module for this rule, and pushes a rule
	pub fn target_builder<S: ToString>
		(&'a self, target_name: S, f: BuilderFn) -> &Self
	{
		let target_name = target_name.to_string();
		let m = self.new_module().builder(f);
		self
			.inject_rule(dsl::target(&target_name, dsl::module(&m.name).function(DEFAULT_BUILD_FN)))
			.inject_module(m)
	}

	pub fn target_builder_module<S1: ToString>
		(&'a self, m: TestModule<'a>, target_name: S1, f: BuilderFn) -> &Self
	{
		let target_name = target_name.to_string();
		self.inject_rules_module(m
			.rule(dsl::target(&target_name, FunctionSpec::from(DEFAULT_BUILD_FN)))
			.builder(f)
		)
	}

	// low-level module / rule modification
	pub fn inject_rule<R: Into<Rule>>(&self, v: R) -> &Self {
		let mut r = self.rules.lock().unwrap();
		r.push(v.into());
		let mut p = self.lock();
		p.replace_rules(r.clone());
		drop(p);
		self
	}

	// injects a module and includes it in the root ruleset
	pub fn inject_rules_module(&self, m: TestModule<'a>) -> &Self {
		let rule = match m.mount {
			None => Self::include_for_test_module(&m),
			Some(ref mount) => dsl::mount(mount),
		};
		self.inject_rule(rule);
		self.inject_module(m)
	}
	
	fn include_for_test_module(m: &TestModule) -> Rule {
		let mod_dsl = dsl::module(&m.name);
		match m.scope {
			None => dsl::include(mod_dsl),
			Some(ref scope) => dsl::include(mod_dsl.scope(scope)),
		}
	}

	pub fn inject_module(&self, v: TestModule<'a>) -> &Self {
		let mut p = self.lock();
		let mut full_path = v.name.to_owned();
		if let Some(mount) = v.mount.as_ref() {
			let simple_path = v.name.to_owned();
			full_path = format!("{}/{}", mount, full_path);
			// mounted modules must be loaded via a yaml entrypoint:
			let yaml_path = format!("{}/ambl.yaml", mount);
			let rules = vec!(Self::include_for_test_module(&v));
			let contents = serde_yaml::to_string(&rules).expect("serialize rules");
			self.write_file(yaml_path, contents).expect("write ambl.yaml");
		}

		p.inject_module(&full_path, v);
		// mark the module file as fresh to avoid loading a real .wasm file
		p.inject_cache(
			BuildRequest::FileDependency(Unembedded(CPath::new_nonvirtual(full_path))),
			BuildResultWithDeps::simple(BuildResult::File(FAKE_FILE.clone()))
		).expect("inject_module");
		drop(p);
		self
	}

	pub fn touch_fake<S: Into<String>>(&self, v: S) -> &Self {
		let mut p = self.lock();
		let stat = PersistFile {
			stat: FileStat::File(Mtime(self.monotonic_clock.fetch_add(1, Ordering::Relaxed) as u128)),
			target: None,
			checksum: PersistChecksum::Disabled,
		};
		p.inject_cache(
			BuildRequest::FileDependency(Unembedded(CPath::new_nonvirtual(v.into()))),
			BuildResultWithDeps::simple(BuildResult::File(stat))
		).expect("touch_fake");
		drop(p);
		self
	}

	pub fn build_file(&self, f: &str) -> Result<&Self> {
		self.build_dep(&DependencyRequest::FileDependency(f.into()))?;
		Ok(self)
	}

	pub fn build_file_contents(&self, f: &str) -> Result<String> {
		self.invoke_full(&Invoke::Action(InvokeAction::ReadFile(ReadFile {
			source_root: FileSource::Target(f.into()),
			source_suffix: None,
		})))?.try_into()
	}

	pub fn build_dep(&self, req: &DependencyRequest) -> Result<InvokeResponse> {
		let build_request = BuildRequest::from(req.to_owned(), None, &Embed::root(), None)?;
		self.build_full(&build_request, &BuildReason::Explicit(Forced(false)))
	}

	pub fn build_full(&self, req: &BuildRequest, reason: &BuildReason) -> Result<InvokeResponse> {
		let project = self.lock();
		let (project, result) = Project::build(project, &Implicits::none(), req, reason)
			.with_context(|| format!("Building {:?}", req))?;

		let response = result.into_response();
		drop(project);

		// testcases run multiple builds, make sure we don't short-circuit between them
		self.reset();

		response
	}

	fn invoke_full(&self, req: &Invoke) -> Result<InvokeResponse> {
		let module = Unembedded(CPath::new_nonvirtual("_fake_root_module".to_owned()));
		let target_context = TargetContext {
			embed: Embed::root(),
			implicits: Default::default(),
			dest_tmp_path: None,
		};
		let result = invoke::perform(self.lock(), &target_context, &module, self.token, req.to_owned())
			.with_context(|| format!("Building {:?}", req));

		// testcases run multiple builds, make sure we don't short-circuit between them
		self.reset();

		result
	}

	pub fn write_file<F: AsRef<Path>, S: AsRef<[u8]>>(&self, path: F, contents: S) -> Result<&Self> {
		debug!("Testcase is writing file {} within {}",
			path.as_ref().display(),
			env::current_dir()?.display()
		);
		let pb = path.as_ref().to_owned();
		fs::create_dir_all(pb.parent().unwrap())?;
		let path = path.as_ref();
		fs::write(path, contents).with_context(|| format!("Writing file {:?}", path))?;
		Ok(self)
	}
	
	pub fn lexists<F: AsRef<Path>>(&self, path: F) -> Result<bool> {
		Ok(crate::path_util::lexists(path.as_ref()).with_context(|| format!("Checking file presence {:?}", path.as_ref()))?)
	}

	pub fn read_file<F: AsRef<Path>>(&self, path: F) -> Result<String> {
		let string = fs::read_to_string(path.as_ref()).with_context(|| format!("Reading file {:?}", path.as_ref()))?;
		Ok(string.trim_end_matches('\n').to_owned())
	}

	pub fn mkdirp<F: AsRef<Path>>(&self, path: F) -> Result<&Self> {
		debug!("Testcase: mkdirp {} within {}",
			path.as_ref().display(),
			env::current_dir()?.display()
		);
		let pb = path.as_ref().to_owned();
		fs::create_dir_all(pb)?;
		Ok(self)
	}

	pub fn write_symlink<P1: AsRef<Path>, P2: AsRef<Path>>(&self, path: P1, dest: P2) -> Result<&Self> {
		debug!("Testcase is writing symlink {} -> {}",
			path.as_ref().display(),
			dest.as_ref().display(),
		);
		let pb = path.as_ref().to_owned();
		fs::create_dir_all(pb.parent().unwrap())?;
		let path = path.as_ref();
		let dest = dest.as_ref();
		unix::fs::symlink(dest, path).with_context(|| format!("Symlinking {:?} -> {:?}", path, dest))?;
		Ok(self)
	}

	pub fn log(&self) -> Log {
		self.log.clone()
	}

	pub fn record<S: ToString>(&self, s: S) {
		self.log.record(s)
	}
}
