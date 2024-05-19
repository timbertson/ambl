use anyhow::*;
use serde::{Serialize, Deserialize};
use std::hash::Hash;
use std::marker::PhantomData;

use crate::ctx::BaseCtx;

// Newtype wrapper so we can implement Hash for it
#[repr(transparent)]
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub struct Config(pub Option<serde_json::Value>);
impl Config {
	pub fn value(&self) -> &Option<serde_json::Value> {
		&self.0
	}
}

impl Hash for Config {
	fn hash<H: std::hash::Hasher>(&self, _state: &mut H) {
		// This is an extremely weak hash, because a nice one would require the ability
		// to iterate Value object properties in a sorted order and that's not worth it.
	}
}
impl Default for Config {
	fn default() -> Self {
		Self(Default::default())
	}
}

// used for embedding a self-contained project at a specific path
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub struct Mount {
	#[serde(default)]
	path: String,
}

impl Mount {
	pub fn get_path(&self) -> &str {
		self.path.as_str()
	}
}

impl Into<Mount> for &str {
	fn into(self) -> Mount {
		self.to_owned().into()
	}
}

impl Into<Mount> for String {
	fn into(self) -> Mount {
		Mount { path: self }
	}
}

impl Into<Rule> for Mount {
	fn into(self) -> Rule {
		Rule::Mount(self)
	}
}

// used for delegating target definitions to another module, with an optional scope
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub struct Include {
	#[serde(default)]
	module: Option<String>,

	#[serde(default)]
	scope: Option<String>,

	#[serde(default)]
	config: Config,

	#[serde(rename = "fn", default)]
	fn_name: Option<String>,
}

impl Include {
	pub fn config<C: Serialize>(mut self, c: C) -> Result<Self> {
		self.config = Config(Some(serde_json::to_value(c)?));
		Ok(self)
	}

	pub fn scope<S: Into<String>>(mut self, s: S) -> Self {
		self.scope = Some(s.into());
		self
	}

	pub fn function<S: Into<String>>(mut self, s: S) -> Self {
		self.fn_name = Some(s.into());
		self
	}

	pub fn get_scope(& self) -> &Option<String> {
		&self.scope
	}

	pub fn get_module(& self) -> &Option<String> {
		&self.module
	}

	pub fn get_fn_name(& self) -> &Option<String> {
		&self.fn_name
	}

	pub fn get_config(& self) -> &Config {
		&self.config
	}
}

// Things that can be included:
// - str / string, names a module
// - function symbol, names a symbol in the current module
// - function spec, e.g. rule_fn(foo).config(bar)
impl Into<Include> for &str {
	fn into(self) -> Include {
		self.to_owned().into()
	}
}

impl Into<Include> for String {
	fn into(self) -> Include {
		Include {
			module: Some(self),
			scope: Default::default(),
			config: Default::default(),
			fn_name: Default::default(),
		}
	}
}

impl Into<Include> for Module {
	fn into(self) -> Include {
		self.0.into()
	}
}

// a raw symbol, like rule_fn(foo_rules)
impl Into<Include> for TypedFnSymbol<BaseCtx, Result<Vec<Rule>>> {
	fn into(self) -> Include {
		Include {
			module: Default::default(),
			scope: Default::default(),
			config: Default::default(),
			fn_name: Some(self.symbol),
		}
	}
}

impl Into<Include> for FunctionSpec {
	fn into(self) -> Include {
		let FunctionSpec {
			fn_name,
			module,
			config,
		} = self;
		Include {
			module: module,
			scope: Default::default(),
			config,
			fn_name: Some(fn_name),
		}
	}
}

impl Into<Rule> for Include {
	fn into(self) -> Rule {
		Rule::Include(self)
	}
}

// Raw function spec, used in runtime (serialized over JSON so it can't be type safe)
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub struct FunctionSpec {
	#[serde(rename = "fn", default)]
	pub fn_name: String,

	#[serde(default)]
	pub module: Option<String>,

	#[serde(default)]
	pub config: Config,
}

impl FunctionSpec {
	pub fn config<C: Serialize>(mut self, c: C) -> Result<Self> {
		self.config = Config(Some(serde_json::to_value(c)?));
		Ok(self)
	}
	
	pub fn scope<S: Into<String>>(self, s: S) -> Include {
		let Self {
			fn_name,
			module,
			config,
		} = self;
		Include {
			fn_name: Some(fn_name),
			scope: Some(s.into()),
			module,
			config,
		}
	}
}

impl<S: ToString> From<S> for FunctionSpec {
	fn from(value: S) -> Self {
		FunctionSpec {
			fn_name: value.to_string(),
			module: Default::default(),
			config: Default::default(),
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub struct EnvLookup {
	pub key: String,
	pub find: String,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub enum Rule {
	Target(Target),
	Mount(Mount),
	Include(Include),
	Sandbox(Sandbox),
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub struct Target {
	// TODO deserializer which accepts `name` shorthand
	pub names: Vec<String>,

	#[serde(flatten)]
	pub build: FunctionSpec,
}

impl Into<Rule> for Target {
	fn into(self) -> Rule {
		Rule::Target(self)
	}
}

// TODO not sure if these should really be rules, but it's expedient for now
#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub enum Sandbox {
	Nix,
	AllowEnv(Vec<String>),
}

impl Into<Rule> for Sandbox {
	fn into(self) -> Rule {
		Rule::Sandbox(self)
	}
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct NestedRule {
	#[serde(default)]
	pub scope: Option<String>,

	pub rules: Vec<Rule>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TypedFnSymbol<I, O> {
	symbol: String,
	_impl: PhantomData<fn(I) -> O>
}
impl<I,O> TypedFnSymbol<I, O> {
	pub fn unsafe_from_string(symbol: String, _impl: fn(I) -> O) -> TypedFnSymbol<I, O> {
		TypedFnSymbol {
			symbol,
			_impl: PhantomData
		}
	}

	// implement the same methods as FunctionSpec
	pub fn config<C: Serialize>(self, c: C) -> Result<FunctionSpec> {
		Into::<FunctionSpec>::into(self).config(c)
	}
}

impl<I,O> Into<FunctionSpec> for TypedFnSymbol<I,O> {
	fn into(self) -> FunctionSpec {
		FunctionSpec {
			fn_name: self.symbol,
			module: None,
			config: Default::default(),
		}
	}
}

pub struct FnSymbol(String);

// module can be used either for Include or to build a FunctionSpec
pub struct Module(String);

impl Module {
	pub fn function<S: Into<String>>(self, name: S) -> FunctionSpec {
		FunctionSpec {
			fn_name: name.into(),
			module: Some(self.0),
			config: Default::default(),
		}
	}
	
	pub fn scope<S: Into<String>>(self, scope: S) -> Include {
		Include {
			module: Some(self.0),
			scope: Some(scope.into()),
			config: Default::default(),
			fn_name: Default::default(),
		}
	}
}

pub mod dsl {
use crate::build::{Command, FilesetDependency, GenCommand, Stdio};
	use super::*;

	// Rule builder functions. Strictly these are just part of the API,
	// but they live here for locality with the instance methods
	pub fn target<S: Into<String>, F: Into<FunctionSpec>>(s: S, entrypoint: F) -> Rule {
		Rule::Target(Target {
			names: vec!(s.into()),
			build: entrypoint.into(),
		})
	}

	pub fn targets<S: Into<String>, F: Into<FunctionSpec>>(s: Vec<S>, entrypoint: FunctionSpec) -> Rule {
		Rule::Target(Target {
			names: s.into_iter().map(|s| s.into()).collect(),
			build: entrypoint.into(),
		})
	}

	pub fn rule<R: Into<Rule>>(r: R) -> Rule {
		r.into()
	}

	pub fn include<S: Into<Include>>(incl: S) -> Rule {
		Rule::Include(incl.into())
	}

	pub fn module<S: Into<String>>(path: S) -> Module {
		Module(path.into())
	}
	
	pub fn mount<S: Into<String>>(path: S) -> Rule {
		Rule::Mount(Mount { path: path.into() })
	}

	pub fn cmd<S: Into<String>>(exe: S) -> Command {
		Command::from(GenCommand::<String, String, Stdio> {
			exe: exe.into(),
			args: Default::default(),
			env: Default::default(),
			env_inherit: Default::default(),
			impure_share_paths: Default::default(),
			output: Default::default(),
			input: Default::default(),
		})
	}

	pub fn fileset<S: Into<String>>(root: S) -> FilesetDependency {
		FilesetDependency {
			root: root.into(),
			dirs: Default::default(),
			files: Default::default(),
		}
	}
	
	pub mod sandbox {
		use super::*;
		pub fn nix_compat() -> Rule {
			rule(Sandbox::Nix)
		}

		pub fn allow_env<S: Into<String>>(s: S) -> Rule {
			rule(Sandbox::AllowEnv(vec!(s.into())))
		}

		pub fn allow_envs<S: Into<String>, V: Iterator<Item=S>>(v: V) -> Rule {
			rule(Sandbox::AllowEnv(v.map(Into::into).collect()))
		}
	}
}
