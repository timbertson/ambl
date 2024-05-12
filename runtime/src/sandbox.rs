use log::*;
use os_pipe::{PipeReader, PipeWriter};
use tempdir::TempDir;
use std::collections::HashMap;
use std::ffi::{OsStr, OsString};
use std::fmt;
use std::io::{Read, Stdin, self, Write, BufRead, BufReader};
use std::os::fd::FromRawFd;
use std::path::Path;
use std::process::Child;
use std::str::FromStr;
use std::thread::{JoinHandle, Thread, self};
use std::{process::{self, Command, Stdio}, env::current_dir, os::unix::fs::symlink, path::PathBuf, fs, collections::HashSet};

use anyhow::*;
use ambl_common::build::{self, GenCommand, InvokeResponse, ChecksumConfig, ImpureShare};

use crate::build::BuildReason;
use crate::build_request::BuildRequest;
use crate::persist::{DepSet, BuildResult};
use crate::project::{Project, ProjectMutexPair, ProjectSandbox, Implicits};
use crate::sync::{Mutexed, MutexHandle};
use crate::path_util::{lexists, Absolute, CPath, External, Scope, Scoped, Simple, Unscoped};
use crate::err::result_block;
use crate::module::BuildModule;
use crate::{DependencyRequest, ui};

pub struct InternalCommand<'a> {
	scope: &'a Scope<'a>,
	mount_depth: u32,
	cmd: GenCommand<Unscoped>,
}
impl<'a> InternalCommand<'a> {
	pub fn wrap(cmd_str: GenCommand<String>, scope: &'a Scope, mount_depth: u32) -> Self {
		Self {
			scope,
			mount_depth,
			cmd: cmd_str.convert(|s| {
				Unscoped::from_string(s, scope)
			}),
		}
	}
}

impl<'a> fmt::Debug for InternalCommand<'a> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		self.cmd.fmt(f)
	}
}

pub struct Sandbox;

struct Roots {
	tmp: Absolute,
	project: Absolute,
}

impl Sandbox {
	fn install_symlink(roots: &Roots, rel: &Simple, dest_rel: Option<&Simple>) -> Result<()> {
		result_block(|| {
			let link = roots.tmp.join(rel);
			let target = roots.project.join(dest_rel.unwrap_or(rel));
			Self::_link(link, target)
		}).with_context(|| format!("Installing symlink {:?} (in root {:?})", rel, roots.tmp))
	}
	
	fn auto_promote(roots: &Roots, paths: Vec<&Unscoped>) -> Result<()> {
		for unscoped_path in paths {
			let rel_path = &unscoped_path.0;
			result_block(|| {
				let tmp_path = roots.tmp.join(rel_path);
				if lexists(tmp_path.as_path())? {
					let dest_path = roots.project.join(rel_path);
					debug!("promoting newly-created path: {}", rel_path);
					fs::rename(tmp_path.as_path(), dest_path.as_path())?;
				} else {
					debug!("path not created; not promoting: {}", rel_path);
				}
				Ok(())
			}).with_context(|| format!("auto-promoting path {}", rel_path))?;
		}
		Ok(())
	}

	fn share_path<'a, 'b>(
		roots: & Roots,
		auto_promote: &mut Vec<&'b Unscoped>,
		source: &'a ImpureShare<Unscoped>
	) -> Result<()>
		// source path reference must live at least as long as the
		// references we store in auto_promote
		where 'a : 'b
	{
		result_block(|| {
			let cpath = &source.value().0;
			let target: Absolute = roots.project.join(cpath);
			let should_link = if !lexists(target.as_path())? {
				match source {
					ImpureShare::Dir(_) => {
						fs::create_dir_all(target.as_path())?;
						true
					},
					ImpureShare::File(t) => {
						// don't auto-create the file. Instead, add this file as an output and
						// promote it at the end of the command if it's been created
						auto_promote.push(t);
						false
					},
				}
			} else {
				true
			};
			if should_link {
				let link = roots.tmp.join(cpath);
				Self::_link(link, target)
			} else {
				Ok(())
			}
		}).with_context(|| format!("Linking shared dir {:?} (in root {:?})", source, roots.tmp))
	}
	
	fn _link<P: AsRef<Path> + fmt::Debug>(link: Absolute, target: P) -> Result<()> {
		debug!("Linking {:?} -> {:?}", &link, &target);
		let link: PathBuf = link.into();
		let parent = link.parent().expect("link.parent()");
		debug!("Creating parent directory {:?}", &parent);
		fs::create_dir_all(&parent)?;
		symlink(target.as_ref(), &link)
			.with_context(|| format!("fs::symlink({:?}, {:?})", target, &link))?;
		Ok(())
	}

	fn collect_paths<'a, 'b, M: BuildModule>(
		project: &'a Project<M>,
		dest: &mut HashMap<Simple, Option<Simple>>,
		key: &'b BuildRequest,
		result: &'b BuildResult,
	) -> Result<()> where 'a : 'b {
		match key {
			BuildRequest::FileDependency(cpath) => {
				if let Result::Ok(rel) = cpath.to_owned().0.into_simple() {
					if dest.contains_key(&rel) {
						return Ok(());
					}
					match result {
						BuildResult::File(file) => {
							if let Some(output) = file.target.to_owned() {
								// don't use `target`, use the shadow path within .ambl/out/`target`
								let unscoped = project.dest_path(&Scoped::root(output))?;
								let simple = unscoped.0.into_simple()?;
								debug!("Adding target to sandbox: {} ({})", &rel, &simple);
								dest.insert(rel, Some(simple));
							} else {
								// normal file
								debug!("Adding file to sandbox: {}", &rel);
								dest.insert(rel, None);
							}

							let persist = project.lookup(&key)?
								.ok_or_else(|| anyhow!("Couldn't find result in build cache for: {:?}", key))?
								.raw();
							if let Some(deps) = &persist.deps {
								// don't include transitive deps if there is a checksum,
								// as that may be impure
								match deps.checksum {
									ChecksumConfig::Enabled => (),
									ChecksumConfig::Disabled => {
										for (key, result) in deps.iter() {
											Self::collect_paths(project, dest, key, &result.result)?;
										}
									}
								}
							}
						},
						_ => (),
					}
				} else {
					// TODO proper sandboxing would allow us to shadow all paths, but currently we can
					// only shadow paths within the workspace
					()
				}
			},
			_ => (), // not a file or target dependency
		}
		Ok(())
	}

	pub fn run<'a, 'b, M: BuildModule>(
		mut project: Mutexed<'a, Project<M>>,
		implicits: &Implicits,
		command: &'b InternalCommand,
		reason: &BuildReason,
	) -> Result<(Mutexed<'a, Project<M>>, TempDir, InvokeResponse)> {
		let dep_set = reason.parent().and_then(|t| project.get_deps(t)).unwrap_or(DepSet::empty_static()).clone();
		let mut rel_paths = Default::default();
		for (dep, result) in dep_set.deps.iter() {
			Self::collect_paths(&project, &mut rel_paths, dep, &result.result)
				.context("initializing command sandbox")?;
		}
		
		let InternalCommand { scope, mount_depth, cmd } = command;
		let GenCommand { exe, args, env, env_inherit, output, input, impure_share_paths } = cmd;

		let scope_dest = scope.as_simple();

		let mut cmd = Command::new(&exe.0.as_path());

		cmd.env_clear();
		
		let mut env_inherit: HashSet<String> = HashSet::from_iter(env_inherit.into_iter().map(ToOwned::to_owned));

		// inherit from project sandbox configuration
		{
			if implicits.sandbox.nix {
				let request = BuildRequest::EnvKeys("NIX_*".to_owned());
				let (project_ret, value) = Project::<M>::build(project, implicits, &request, reason)?;
				project = project_ret;
				match value.result.result {
					BuildResult::EnvKeys(keys) => {
						for key in keys {
							env_inherit.insert(key.to_owned());
						}
					},
					_ => panic!("impossible result from Envvar request"),
				}
			}
			
			for key in implicits.sandbox.allow_env.iter() {
				env_inherit.insert(key.to_owned());
			}
		}

		// Make sure we have minimal requisite environment, add these in if they're not already included:
		// TODO scope to osx / linux / etc
		// TODO make PATH minimal if nix is used?
		for key in ["PATH", "HOME", "LD_DYLD_PATH", "LD_LIBRARY_PATH"] {
			env_inherit.insert(key.to_owned());
		}

		// populate the inherited env values
		for k in env_inherit {
			let request = BuildRequest::EnvVar(k.to_owned());
			let (project_ret, value) = Project::<M>::build(project, implicits, &request, reason)?;
			match value.result.result {
				BuildResult::Env(value) => {
					if let Some(v) = value {
						cmd.env(k, v);
					}
				},
				_ => panic!("impossible result from Envvar request"),
			}
			project = project_ret;
		}
		for (k, v) in env {
			cmd.env(k, v);
		}
		
		let ui_writer = project.writer().to_owned();
		let (tmp, response) = project.unlocked_block(|project_handle| {
			let tmp = tempdir::TempDir::new("ambl")?;
			debug!("created command sandbox {:?}", &tmp);
			let roots = Roots {
				tmp: CPath::try_from(tmp.path().to_owned())?.into_absolute()?,
				project: CPath::try_from(current_dir()?)?.into_absolute()?,
			};
			
			cmd.args(args);
			
			cmd.stdin(match input {
				build::Stdin::Inherit => Stdio::inherit(),
				build::Stdin::Value(v) => todo!(),
				build::Stdin::Null => Stdio::null(),
			});
			
			use build::{Stdout, Stderr};
			

			let mut capture_pipe = None;
			let mut proxy_pipe = None;
			
			let writeable_end = |store: &mut Option<(PipeReader, PipeWriter)>| {
				if let Some((_, ref write)) = store {
					write.try_clone()
				} else {
					let (read, write) = os_pipe::pipe()?;
					let result = write.try_clone();
					*store = Some((read, write));
					result
				}
			};

			cmd.stdout(match output.stdout {
				Stdout::Return => writeable_end(&mut capture_pipe)?.into(),
				Stdout::Inherit => writeable_end(&mut proxy_pipe)?.into(),
				Stdout::Ignore => Stdio::null(),
			});
			
			cmd.stderr(match (&output.stderr, &output.stdout) {
				// inherit & inherit means merge
				(Stderr::Merge, Stdout::Inherit) | (Stderr::Inherit, Stdout::Inherit) =>
					writeable_end(&mut proxy_pipe)?.into(),

				(Stderr::Merge, Stdout::Return) => writeable_end(&mut capture_pipe)?.into(),
				(Stderr::Merge, Stdout::Ignore) | (Stderr::Ignore, _) => Stdio::null(),

				(Stderr::Inherit, _) => writeable_end(&mut proxy_pipe)?.into(),
			});

			let response = result_block(|| {
			
				// TODO: actual sandboxing.
				// Use something like user namespacing to shadow
				// all directories the command shouldn't have access to

				debug!("Installing {} symlinks", rel_paths.len());
				for (k, v) in rel_paths {
					Self::install_symlink(&roots, &k, v.as_ref())?;
				}

				// TODO: figure out how an explicitly passed in cwd would interact
				// with mount path
				let cwd: Absolute = match scope.as_simple() {
					Some(scope) => roots.tmp.join(scope),
					None => roots.tmp.clone(),
				};

				std::fs::create_dir_all(&cwd)?;
				cmd.current_dir(&cwd);

				{ // install @scope in cwd
					let scope_link = cwd.join(&CPath::new("@scope".to_owned()));
					Self::_link(scope_link, scope_dest.map(|simple| simple.as_ref())
						.unwrap_or(CPath::Cwd.as_ref()))?;
				}

				{ // install @root in cwd, as a sequence of `..` components
					let root_link = roots.tmp.join(&CPath::new("@root".to_owned()));
					let mut root_dest = PathBuf::from_str(".")?;
					for _ in 0..(*mount_depth) {
						root_dest.push("..");
					}
					Self::_link(root_link, root_dest)?;
				}

				{ // install .ambl/tmp at the project root, so the command can populate the output path
					let ambl_cpath = CPath::new(".ambl/tmp".to_owned());
					let root_link = roots.tmp.join(&ambl_cpath);
					let root_dest = roots.project.join(&ambl_cpath);
					Self::_link(root_link, root_dest)?;
				}

				debug!("Installing {} symlinks to impure_shared_paths", impure_share_paths.len());
				let mut auto_promote_paths: Vec<&Unscoped> = Vec::new();
				for path in impure_share_paths {
					Self::share_path(&roots, &mut auto_promote_paths, path)?;
				}

				if log_enabled!(log::Level::Debug) {
					debug!("+ {:?}", &cmd);
				} else {
					info!("+ {:?}", &exe);
				}

				let mut proc = cmd.spawn()?;

				// cmd holds write handles in stdout/err values, drop them
				drop(cmd);
				
				let mut join_handles = Vec::new();
				if let Some((reader, writer)) = proxy_pipe.take() {
					drop(writer);
					join_handles.push(spawn_output_proxy(ui_writer, reader)?);
				}

				let response = match capture_pipe.take() {
					Some((mut reader, writer)) => {
						drop(writer);
						let mut s: Vec<u8> = Vec::new();
						reader.read_to_end(&mut s)?;
						drop(reader);
						InvokeResponse::Bytes(s)
					},
					None => InvokeResponse::Unit,
				};
				// wait for all output pipes to drain
				drop(join_handles);

				let result = proc.wait()?;
				debug!("- {:?}", &result);
				// std::thread::sleep(std::time::Duration::from_millis(500));
				if result.success() {
					Self::auto_promote(&roots, auto_promote_paths)?;
					Ok(response)
				} else {
					Err(anyhow!("Command `{}` failed (exit status: {:?})", exe, &result.code()))
				}
			});

			let response = crate::debug::shell_on_failure(response)?;
			Ok((tmp, response))
		})?;
		Ok((project, tmp, response))
	}
}

#[derive(Clone, Copy)]
enum OutputStream {
	Stdout,
	Stderr,
}

fn spawn_output_proxy(ui_writer: ui::Writer, reader: PipeReader) -> Result<JoinHandle<()>> {
	let result = thread::Builder::new()
		.spawn(move || {
			let mut line_reader = BufReader::new(reader);
			let mut buf = String::with_capacity(1024);
			loop {
				let len = line_reader.read_line(&mut buf).unwrap_or_else(|e| {
					error!("failed reading data from child process: {}", e);
					0
				});
				if len == 0 {
					break;
				}
				let line_only = buf.trim_end_matches('\n');
				ui_writer.emit(ui::Event::Line(&line_only)).expect("emitting output");
				buf.clear();
			}
	});
	Ok(result?)
}
