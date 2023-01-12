use ambl_common::rule::EnvLookup;
use log::*;
use ambl_common::build::{FileSelection, FilesetDependency};
use std::{collections::HashMap, fs, time::UNIX_EPOCH, io, borrow::Borrow, fmt::Display, path::{Path, PathBuf}};

use anyhow::*;
use serde::{Serialize, de::DeserializeOwned, Deserialize};
use ambl_common::{build::{DependencyRequest, InvokeResponse, FileDependency, FileDependencyType, Command, GenCommand}, rule::{FunctionSpec, Config}};

use crate::project::{ProjectRef, Project, ProjectHandle};
use crate::path_util::{Simple, Scope, Scoped, CPath, Unscoped, ResolveModule};
use crate::module::BuildModule;

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
// Like DependencyRequest but with all variants fully resolved
// (i.e. relative to project root, not to whatever scope the request came from)
pub enum BuildRequest {
	FileDependency(Unscoped),
	WasmCall(ResolvedFnSpec<'static>),
	EnvVar(String),
	EnvLookup(EnvLookup),
	Fileset(ResolvedFilesetDependency),
	Execute(GenCommand<Unscoped>),
	Universe,
}

impl BuildRequest {
	pub fn from<'a>(req: DependencyRequest, source_module: Option<&Unscoped>, scope: &'a Scope<'a>) -> Result<(Self, PostBuild)> {
		Ok(match req {
			DependencyRequest::FileDependency(v) => {
				let FileDependency { path, ret } = v;
				let path = Unscoped::from_string(path, scope);
				let ret = PostBuildFile { path: path.clone(), ret };
				(Self::FileDependency(path), PostBuild::FileDependency(ret))
			},
			DependencyRequest::WasmCall(v) => (
				Self::WasmCall(ResolvedFnSpec::from_explicit_fn_name(v, source_module, scope.clone())?),
				PostBuild::Unit
			),
			DependencyRequest::EnvVar(v) => (Self::EnvVar(v), PostBuild::Unit),
			DependencyRequest::EnvLookup(v) => (Self::EnvLookup(v), PostBuild::Unit),
			DependencyRequest::Fileset(v) => {
				let FilesetDependency { root, dirs, files } = v;
				let root = Unscoped::from_string(root, scope);
				(Self::Fileset(ResolvedFilesetDependency{ root, dirs, files }), PostBuild::Unit)
			},
			DependencyRequest::Execute(v) => {
				let gen_str : GenCommand<String> = v.into();
				let mut gen = gen_str.convert(|s| {
					Unscoped::from_string(s, scope)
				});
				// If there is a scope, make sure it's used for the default CWD
				let override_cwd = match (scope.as_simple(), &gen.cwd) {
					(Some(scope), None) => {
						gen.cwd = Some(scope.clone().into());
					},
					_ => ()
				};
				(Self::Execute(gen), PostBuild::Unit)
			},
			DependencyRequest::Universe => (Self::Universe, PostBuild::Unit),
		})
	}
}

// Information not needed in the build itself, but used to formuate a response
pub enum PostBuild {
	FileDependency(PostBuildFile),
	Unit,
}

pub struct PostBuildFile {
	pub path: Unscoped,
	pub ret: FileDependencyType,
}


#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct ResolvedFnSpec<'a> {
	pub fn_name: String,
	// we track the full path from the project, since that's how modules are keyed
	pub full_module: Unscoped,
	// but we also track the scope of this call, since that affects
	// the results
	pub scope: Scope<'a>,
	pub config: Config,
}

impl<'a> ResolvedFnSpec<'a> {
	pub fn from_explicit_fn_name(f: FunctionSpec, source_module: Option<&Unscoped>, scope: Scope<'a>) -> Result<Self> {
		let FunctionSpec { fn_name, module, config } = f;
		let fn_name = fn_name.ok_or_else(|| anyhow!("Function spec is missing a function name"))?;
		let explicit_cpath = module.map(CPath::new);
		let full_module = ResolveModule {
			source_module,
			explicit_path: explicit_cpath.as_ref().map(|p| Scoped::new(scope.copy(), p)),
		}.resolve()?;
		Ok(Self {
			scope,
			fn_name,
			full_module,
			config,
		})
	}
}


#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct ResolvedFilesetDependency {
	pub root: Unscoped,
	pub dirs: Vec<FileSelection>,
	pub files: Vec<FileSelection>,
}

pub struct CompiledGlobs {
	pub dirs: Vec<FileSelectionGlob>,
	pub files: Vec<FileSelectionGlob>,
}

#[derive(Debug)]
pub enum FileSelectionGlob {
	IncludeGlob(glob::Pattern),
	ExcludeGlob(glob::Pattern),
}
impl FileSelectionGlob {
	pub fn is_include(&self) -> bool {
		match self {
			FileSelectionGlob::IncludeGlob(_) => true,
			FileSelectionGlob::ExcludeGlob(_) => false,
		}
	}
}

impl ResolvedFilesetDependency {
	fn compile_one(sel: &FileSelection) -> Result<FileSelectionGlob> {
		Ok(match sel {
			FileSelection::IncludeGlob(p) => FileSelectionGlob::IncludeGlob(glob::Pattern::new(p)?),
			FileSelection::ExcludeGlob(p) => FileSelectionGlob::ExcludeGlob(glob::Pattern::new(p)?),
		})
	}

	fn compile_many(sels: &Vec<FileSelection>) -> Result<Vec<FileSelectionGlob>> {
		sels.iter().map(Self::compile_one).collect()
	}

	pub fn compile(&self) -> Result<CompiledGlobs> {
		Ok(CompiledGlobs {
			dirs: Self::compile_many(&self.dirs)?,
			files: Self::compile_many(&self.files)?,
		})
	}
}
