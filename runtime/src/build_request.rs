use ambl_api::{ImpureShare, Stderr, Stdio, Stdout};
use ambl_common::rule::EnvLookup;
use log::*;
use ambl_common::build::{FileSelection, FilesetDependency};
use std::fmt;
use std::{collections::HashMap, fs, time::UNIX_EPOCH, io, borrow::Borrow, fmt::Display, path::{Path, PathBuf}};

use anyhow::*;
use serde::{Serialize, de::DeserializeOwned, Deserialize};
use ambl_common::{build::{DependencyRequest, InvokeResponse, Command, GenCommand}, rule::{FunctionSpec, Config}};

use crate::project::{ProjectRef, Project, ProjectHandle};
use crate::path_util::{Simple, Embed, Embedded, CPath, Unembedded, ResolveModule};
use crate::module::BuildModule;
use crate::build::TargetContext;

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
// Like DependencyRequest but with all variants fully resolved
// (i.e. relative to project root, not to whatever embed the request came from)
pub enum BuildRequest {
	FileDependency(Unembedded),
	FileExistence(Unembedded),
	WasmCall(ResolvedFnSpec<'static>),
	EnvVar(String),
	EnvKeys(String),
	EnvLookup(EnvLookup),
	Fileset(ResolvedFilesetDependency),
	Execute(ResolvedCommand),
	Universe,
}

impl BuildRequest {
	pub fn from(
		req: DependencyRequest,
		source_module: Option<&Unembedded>,
		embed: &Embed,
		dest_tmp_path: Option<&Unembedded>,
	) -> Result<Self> {
		Ok(match req {
			DependencyRequest::FileDependency(path) => {
				let path = Unembedded::from_string(path, embed);
				Self::FileDependency(path)
			},
			DependencyRequest::FileExistence(path) => {
				let path = Unembedded::from_string(path, embed);
				Self::FileExistence(path)
			},
			DependencyRequest::WasmCall(v) =>
				Self::WasmCall(ResolvedFnSpec::from_explicit_fn_name(v, source_module, embed.clone())?),
			DependencyRequest::EnvVar(v) => Self::EnvVar(v),
			DependencyRequest::EnvKeys(v) => Self::EnvKeys(v),
			DependencyRequest::EnvLookup(v) => Self::EnvLookup(v),
			DependencyRequest::Fileset(v) => {
				let FilesetDependency { root, dirs, files } = v;
				let root = Unembedded::from_string(root, embed);
				Self::Fileset(ResolvedFilesetDependency{ root, dirs, files })
			},
			DependencyRequest::Execute(v) => {
				Self::Execute(ResolvedCommand::new(v.into(), embed.clone(), dest_tmp_path)?)
			},
			DependencyRequest::Universe => Self::Universe,
		})
	}
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct ResolvedFnSpec<'a> {
	pub fn_name: String,
	// we track the full path from the project, since that's how modules are keyed
	pub full_module: Unembedded,
	// but we also track the embed of this call, since that affects
	// the results
	pub embed: Embed<'a>,
	pub config: Config,
}

impl<'a> ResolvedFnSpec<'a> {
	pub fn from_explicit_fn_name(f: FunctionSpec, source_module: Option<&Unembedded>, embed: Embed<'a>) -> Result<Self> {
		let FunctionSpec { fn_name, module, config } = f;
		let explicit_cpath = module.map(|m| CPath::new(m, &embed));
		let full_module = ResolveModule {
			source_module,
			explicit_path: explicit_cpath.as_ref().map(|p| Embedded::new(embed.copy(), p)),
		}.resolve().with_context(|| format!("resolving call to function {}", &fn_name))?;
		Ok(Self {
			embed,
			fn_name,
			full_module,
			config,
		})
	}
}

// like ambl_api::Stdout except WriteDest embeds the actual destination path
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum ResolvedStdout {
	Return,
	Inherit,
	WriteDest(Unembedded),
	Ignore,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct ResolvedStdio {
	pub stdout: ResolvedStdout,
	pub stderr: Stderr,
}

impl ResolvedStdio {
	fn new(stdio: Stdio, dest_path: Option<&Unembedded>) -> Result<Self> {
		let Stdio { stdout, stderr} = stdio;
		let stdout = match stdout {
			Stdout::WriteDest => {
				let dest_path = dest_path.ok_or_else(|| anyhow!("WriteDest used outside a target"))?;
				ResolvedStdout::WriteDest(dest_path.to_owned())
			},
			Stdout::Return => ResolvedStdout::Return,
			Stdout::Inherit => ResolvedStdout::Inherit,
			Stdout::Ignore => ResolvedStdout::Ignore,
		};
		Ok(Self { stdout, stderr })
	}
}

#[derive(Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum Exe {
	FromPath(String),
	Local(Unembedded),
}

impl Display for Exe {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Exe::FromPath(p) => Display::fmt(p, f),
			Exe::Local(p) => Display::fmt(p, f),
		}
	}
}

impl fmt::Debug for Exe {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Exe::FromPath(p) => fmt::Debug::fmt(p, f),
			Exe::Local(p) => fmt::Debug::fmt(p, f),
		}
	}
}

#[derive(Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct ResolvedCommand {
	pub embed: Embed<'static>,
	pub cmd: GenCommand<Exe, Unembedded, ResolvedStdio>,
}

impl ResolvedCommand {
	pub fn new(cmd_str: GenCommand<String, String, Stdio>, embed: Embed<'static>, target_dest: Option<&Unembedded>) -> Result<Self> {
		let string_to_path = |s| Unembedded::from_string(s, &embed);
		let GenCommand { exe, args, env, env_inherit, impure_share_paths, output, input } = cmd_str;

		// use `./` for exes in the current directory
		let exe = if exe.contains('/') {
			Exe::Local(string_to_path(exe))
		} else {
			Exe::FromPath(exe)
		};

		let output = ResolvedStdio::new(output, target_dest)?;

		let impure_share_paths : Vec<ImpureShare<Unembedded>> = impure_share_paths
			.into_iter()
			.map(|impure_share| impure_share.map(&string_to_path))
			.collect();

		let cmd = GenCommand {
			exe, args, env,
			env_inherit,
			impure_share_paths,
			output, input,
		};

		Ok(Self { embed, cmd })
	}
}

impl fmt::Debug for ResolvedCommand {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		self.cmd.fmt(f)
	}
}


#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct ResolvedFilesetDependency {
	pub root: Unembedded,
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
