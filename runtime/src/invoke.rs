use std::{fs, io};
use std::path::{PathBuf, Path};
use std::sync::Arc;

use anyhow::*;
use log::*;
use ambl_common::build::{InvokeResponse, InvokeAction, Invoke, WriteDest, FileSource, DependencyRequest, ChecksumConfig};

use crate::build::{BuildReason, BuildResponse, TargetContext};
use crate::build_request::BuildRequest;
use crate::err::result_block;
use crate::module::BuildModule;
use crate::path_util::{lexists, Unscoped, CPath, Scope, Simple, Scoped, self, string_of_pathbuf};
use crate::persist::BuildResult;
use crate::project::{Project, ActiveBuildToken, ProjectMutex, ProjectMutexPair, Implicits};
use crate::sync::MutexHandle;

pub fn perform<'a, M: BuildModule>(
	project: ProjectMutex<'a, M>,
	target_context: &TargetContext,
	module_path: &Unscoped,
	token: ActiveBuildToken,
	request: Invoke,
) -> Result<InvokeResponse> {
	let response = match request {
		Invoke::Action(action) => {
			crate::debug::shell_on_failure(perform_invoke(project, target_context, module_path, token, &action))
		},
		Invoke::Dependency(request) => {
			let build_request = BuildRequest::from(request, Some(module_path), &target_context.scope)?;
			let (project, persist) = Project::build(project, &target_context.implicits, &build_request, &BuildReason::Dependency(token))?;
			persist.into_response()
		},
	};
	debug!("invoke response: {:?}", &response);
	response
}

fn perform_invoke<M: BuildModule>(
	mut project: ProjectMutex<M>,
	target_context: &TargetContext,
	module_path: &Unscoped,
	token: ActiveBuildToken,
	action: &InvokeAction,
) -> Result<InvokeResponse> {
	result_block(|| {
		let scope = &target_context.scope;
		let dest_tmp = |project: &Project<M>, target: &String| {
			let target = Scoped::new(scope.copy(), Simple::try_from(target.to_owned(), scope)?);
			Project::tmp_path(project, &target)
		};
		match action {
			InvokeAction::WriteDest(f) => {
				let path = dest_tmp(&project, &f.target)?;
				debug!("Writing output file {} for target {}", &path, &f.target);
				let dest_pb = PathBuf::from(path.as_path());
				if let Some(parent) = dest_pb.parent() {
					fs::create_dir_all(parent)?;
				}
				if !f.replace && lexists(path.as_path())? {
					return Err(anyhow!("Attempted to replace existing file {}", &path))
				}
				fs::write(path.as_path(), &f.contents)
					.with_context(|| format!("Writing file {}", &path))?;
				if f.contents.is_empty() {
					// targets with output might want to disable checksums, but targets with empty
					// outpt never make sense to checksum
					project.configure_checksum(token, ChecksumConfig::Disabled);
				}
				Ok(InvokeResponse::Unit)
			},

			InvokeAction::ConfigureChecksum(enabled) => {
				let config = if *enabled { ChecksumConfig::Enabled } else { ChecksumConfig::Disabled };
				project.configure_checksum(token, config);
				Ok(InvokeResponse::Unit)
			},

			InvokeAction::CopyFile(f) => {
				let (project, src) = built_source_path(project, target_context, module_path, token, &f.source_root, Some(&f.source_suffix))?;
				let dest = dest_tmp(&project, &f.dest_target)?;
				debug!("Copying file {} -> {}", src.display(), &dest);
				if !lexists(&src)? {
					return Err(anyhow!("Copy source doesn't exist: {}", src.display()));
				};
				fs::copy(&src, dest.as_path())
					.with_context(|| format!("Copying file {} -> {}", src.display(), &dest))?;
				Ok(InvokeResponse::Unit)
			},

			InvokeAction::ReadFile(f) => {
				let (project, src) = built_source_path(project, target_context, module_path, token, &f.source_root, f.source_suffix.as_deref())?;
				debug!("Reading file {}", src.display());
				let bytes = fs::read(&src)
					.with_context(|| format!("Reading file {}", src.display()))?;
				Ok(InvokeResponse::Bytes(bytes))
			},

			InvokeAction::GetPath(f) => {
				let (project, src) = built_source_path(project, target_context, module_path, token, &f.source_root, f.source_suffix.as_deref())?;
				Ok(InvokeResponse::Str(string_of_pathbuf(src)))
			},
		}
	}).with_context(|| format!("Invoking action: {:?}", action))
}

fn built_source_path<'a, M: BuildModule>(
	mut project: ProjectMutex<'a, M>,
	target_context: &TargetContext,
	module_path: &Unscoped,
	token: ActiveBuildToken,
	src: &FileSource,
	suffix: Option<&str>
)
	-> Result<ProjectMutexPair<'a, M, PathBuf>>
{
	let scope = &target_context.scope;
	let mut src = match src {
		FileSource::Target(path) => {
			let request = DependencyRequest::FileDependency(path.to_owned());
			let build_request = BuildRequest::from(request, Some(module_path), scope)?;
			let (project_ret, persist) = Project::build(project, &target_context.implicits, &build_request, &BuildReason::Dependency(token))?;
			project = project_ret;
			let target = match persist.result.result {
				BuildResult::File(t) => t.target,
				_ => None,
			};
			let unscoped = match target {
				Some(t) => project.dest_path(&Scoped::root(t))?,
				None => Unscoped::from_string(path.to_owned(), scope),
			};
			unscoped.0.into()
		},
		FileSource::Tempdir(tempdir) => {
			let temp_root: &Path = project.get_tempdir(token, *tempdir)?;
			let mut ret = PathBuf::from(temp_root);
			scope.push_to(&mut ret);
			ret
		}
	};

	if let Some(suffix) = suffix {
		src.push(suffix.to_owned())
	}
	Ok((project, src))
}
