use std::fs;
use std::path::{PathBuf, Path};
use std::sync::Arc;

use anyhow::*;
use log::*;
use ambl_common::build::{InvokeResponse, InvokeAction, Invoke, WriteDest, FileSource};

use crate::build::BuildReason;
use crate::build_request::BuildRequest;
use crate::err::result_block;
use crate::module::BuildModule;
use crate::path_util::{lexists, Unscoped, CPath, Scope, Simple, Scoped};
use crate::project::{Project, ActiveBuildToken, ProjectMutex};
use crate::sync::MutexHandle;

pub fn perform<'a, M: BuildModule>(
	request: Invoke,
	token: ActiveBuildToken,
	module_path: &Unscoped,
	scope: &Scope,
	project: ProjectMutex<'a, M>
) -> Result<InvokeResponse> {
	match request {
		Invoke::Action(ref action) => {
			crate::debug::shell_on_failure(perform_invoke(action, token, scope, project))
		},
		Invoke::Dependency(request) => {
			let (build_request, post_build) = BuildRequest::from(request, Some(module_path), scope)?;
			let (project, persist) = Project::build(project, &build_request, &BuildReason::Dependency(token))?;
			persist.into_response(&project, &post_build)
		},
	}
}

fn perform_invoke<M: BuildModule>(
	action: &InvokeAction,
	token: ActiveBuildToken,
	scope: &Scope,
	project: ProjectMutex<M>
) -> Result<InvokeResponse> {
	result_block(|| {
		let dest_tmp = |project: &Project<M>, target: &String| {
			let target = Scoped::new(scope.copy(), Simple::try_from(target.to_owned())?);
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
			},
			InvokeAction::CopyFile(f) => {
				match &f.source_root {
					FileSource::Target(t) => todo!("Copy file from target"),
					FileSource::Tempdir(tempdir) => {
						let temp_root: &Path = project.get_tempdir(token, *tempdir)?;
						let mut src = PathBuf::from(temp_root);
						src.push(Unscoped::from_string(f.source_suffix.clone(), scope).0);
						let dest = dest_tmp(&project, &f.dest_target)?;
						debug!("Copying file {} -> {}", src.display(), &dest);
						if !lexists(&src)? {
							return Err(anyhow!("Copy source doesn't exist: {}", src.display()));
						};
						fs::copy(&src, dest.as_path())
							.with_context(|| format!("Copying file {} -> {}", src.display(), &dest))?;
					},
				}
			},
		}
		Ok(InvokeResponse::Unit)
	}).with_context(|| format!("Invoking action: {:?}", action))
}
