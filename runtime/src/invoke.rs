use std::{fs, io};
use std::path::{PathBuf, Path};
use std::sync::Arc;

use anyhow::*;
use log::*;
use ambl_common::build::{InvokeResponse, InvokeAction, Invoke, WriteDest, FileSource, DependencyRequest, ChecksumConfig};
use superconsole::output;

use crate::build::{BuildReason, BuildResponse, OutputMode, TargetContext};
use crate::build_request::BuildRequest;
use crate::err::result_block;
use crate::module::BuildModule;
use crate::path_util::{lexists, Unembedded, CPath, Embed, Simple, Embedded, self, string_of_pathbuf};
use crate::persist::BuildResult;
use crate::project::{Project, ActiveBuildToken, ProjectMutex, ProjectMutexPair, Implicits};
use crate::sync::MutexHandle;

pub fn perform<'a, M: BuildModule>(
	project: ProjectMutex<'a, M>,
	target_context: &TargetContext,
	module_path: &Unembedded,
	token: ActiveBuildToken,
	request: Invoke,
) -> Result<InvokeResponse> {
	let response = match request {
		Invoke::Action(action) => {
			crate::debug::shell_on_failure(perform_invoke(project, target_context, module_path, token, &action))
		},
		Invoke::Dependency(request) => {
			let build_request = BuildRequest::from(request, Some(module_path), &target_context.embed, target_context.dest_tmp_path.as_ref())?;
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
	module_path: &Unembedded,
	token: ActiveBuildToken,
	action: &InvokeAction,
) -> Result<InvokeResponse> {
	result_block(|| {
		let embed = &target_context.embed;
		match action {
			InvokeAction::WriteDest(f) => {
				let WriteDest { contents, replace, output_name } = f;
				let mut path = target_context.dest_tmp_path()?.as_path().to_owned();
				if !f.replace && lexists(&path)? {
					return Err(anyhow!("Attempted to replace existing file {}", path.display()))
				}

				match target_context.output_mode {
					Some(OutputMode::Multiple) => {
						// if you use write_dest() on a multi-output target, we write e.g. "foo/foo" instead
						// of just "foo".
						let output_name = output_name.clone()
							.or_else(|| path.file_name().map(|name| path_util::str_of_os(name).to_owned()))
							.ok_or_else(|| anyhow!("couldn't get default output name for multi-output target"))?;
						path_util::fsopt(&path, fs::create_dir_all(&path))?;
						path.push(output_name);
					},
					None | Some(OutputMode::Single) => {
						if let Some(output_name) = output_name {
							return Err(anyhow!("Attempted to write output named {} in {}, but this is not a multi-output target",
								output_name, path.display()));
						}
						if contents.is_empty() {
							// targets with output might want to disable checksums, but targets
							// with a single empty output never make sense to checksum
							project.configure_checksum(token, ChecksumConfig::Disabled);
						}
					},
				}

				debug!("Writing output file {}", path.display());
				fs::write(path.as_path(), &f.contents)
					.with_context(|| format!("Writing file {}", path.display()))?;
				Ok(InvokeResponse::Unit)
			},

			InvokeAction::ConfigureChecksum(enabled) => {
				let config = if *enabled { ChecksumConfig::Enabled } else { ChecksumConfig::Disabled };
				project.configure_checksum(token, config);
				Ok(InvokeResponse::Unit)
			},

			InvokeAction::CopyFile(f) => {
				let (project, src) = built_source_path(project, target_context, module_path, token, &f.source_root, Some(&f.source_suffix))?;
				let dest = target_context.dest_tmp_path()?;
				debug!("Copying file {} -> {}", src.display(), dest);
				if !lexists(&src)? {
					return Err(anyhow!("Copy source doesn't exist: {}", src.display()));
				};
				fs::copy(&src, dest.as_path())
					.with_context(|| format!("Copying file {} -> {}", src.display(), dest))?;
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
	module_path: &Unembedded,
	token: ActiveBuildToken,
	src: &FileSource,
	suffix: Option<&str>
)
	-> Result<ProjectMutexPair<'a, M, PathBuf>>
{
	let embed = &target_context.embed;
	let mut src = match src {
		FileSource::Target(path) => {
			let request = DependencyRequest::FileDependency(path.to_owned());
			let build_request = BuildRequest::from(request, Some(module_path), embed, target_context.dest_tmp_path.as_ref())?;
			let (project_ret, persist) = Project::build(project, &target_context.implicits, &build_request, &BuildReason::Dependency(token))?;
			project = project_ret;
			let target = match persist.result.result {
				BuildResult::File(t) => t.target,
				_ => None,
			};
			let unembedded = match target {
				Some(t) => project.dest_path(&Embedded::root(t))?,
				None => Unembedded::from_string(path.to_owned(), embed),
			};
			unembedded.0.into()
		},
		FileSource::Tempdir(tempdir) => {
			let temp_root: &Path = project.get_tempdir(token, *tempdir)?;
			let mut ret = PathBuf::from(temp_root);
			embed.push_mount_to(&mut ret);
			ret
		}
	};

	if let Some(suffix) = suffix {
		src.push(suffix.to_owned())
	}
	Ok((project, src))
}
