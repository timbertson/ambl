use std::fs;
use std::path::PathBuf;
use std::sync::Arc;

use anyhow::*;
use log::*;
use ambl_common::build::{InvokeResponse, InvokeAction, Invoke};

use crate::build::BuildReason;
use crate::build_request::BuildRequest;
use crate::err::result_block;
use crate::module::BuildModule;
use crate::path_util::{lexists, Unscoped, Scope, Scoped, CPath};
use crate::project::{Project, ActiveBuildToken};
use crate::sync::MutexHandle;

pub fn perform2<M: BuildModule>(
	request: Invoke,
	token: ActiveBuildToken,
	module_arc: &Arc<Unscoped>,
	scope: &Scope,
	mut project_handle: MutexHandle<Project<M>>
) -> Result<InvokeResponse> {
	match request {
		Invoke::Action(action) => {
			let expanded = action.map(|p: String| {
				Unscoped::from_string(p, &scope)
			});
			crate::debug::shell_on_failure(perform(&expanded))
		},
		Invoke::Dependency(request) => {
			let (build_request, post_build) = BuildRequest::from(request, Some(module_arc), scope)?;
			let project = project_handle.lock("ambl_invoke")?;
			let (project, persist) = Project::build(project, &build_request, &BuildReason::Dependency(token))?;
			persist.into_response(&project, &post_build)
		},
	}
}

fn perform(action: &InvokeAction<Unscoped>) -> Result<InvokeResponse> {
	result_block(|| {
		match action {
			InvokeAction::WriteFile(f) => {
				debug!("Writing file {}", &f.path);
				let dest_pb = PathBuf::from(f.as_path());
				if let Some(parent) = dest_pb.parent() {
					fs::create_dir_all(parent)?;
				}
				fs::write(f.as_path(), &f.contents)
					.with_context(|| format!("Writing file {}", &f.path))?;
			},
			InvokeAction::CopyFile(f) => {
				debug!("Copying file {} -> {}", &f.src, &f.dest);
				let dest_pb = PathBuf::from(f.dest_path());
				if let Some(parent) = dest_pb.parent() {
					fs::create_dir_all(parent)?;
				}
				if !lexists(&f.src)? {
					return Err(anyhow!("Copy source doesn't exist: {}", &f.src));
				};
				fs::copy(&f.src, &f.dest)
					.with_context(|| format!("Copying file {} -> {}", &f.src, &f.dest))?;
			},
		}
		Ok(InvokeResponse::Unit)
	}).with_context(|| format!("Invoking action: {:?}", action))
}
