use log::*;
use tempdir::TempDir;
use std::collections::HashMap;
use std::ffi::{OsStr, OsString};
use std::io::{Read, Stdin, self, Write, BufRead, BufReader};
use std::os::fd::FromRawFd;
use std::process::Child;
use std::thread::{JoinHandle, Thread, self};
use std::{process::{self, Command, Stdio}, env::current_dir, os::unix::fs::symlink, path::PathBuf, fs, collections::HashSet};

use anyhow::*;
use ambl_common::build::{self, GenCommand, InvokeResponse, ChecksumConfig};

use crate::build::BuildReason;
use crate::build_request::BuildRequest;
use crate::persist::{DepSet, BuildResult};
use crate::project::{Project, ProjectMutexPair, ProjectSandbox, Implicits};
use crate::sync::{Mutexed, MutexHandle};
use crate::path_util::{External, Absolute, Scoped, CPath, Simple, Unscoped, lexists};
use crate::err::result_block;
use crate::module::BuildModule;
use crate::{DependencyRequest, ui};

pub struct Sandbox {
}

struct Roots {
	tmp: Absolute,
	project: Absolute,
}

impl Sandbox {
	fn install_symlink(roots: &Roots, rel: &Simple, dest_rel: Option<&Simple>) -> Result<()> {
		result_block(|| {
			let link = roots.tmp.join(rel);
			let target = roots.project.join(dest_rel.unwrap_or(rel));
			Self::_link(roots, link, target)
		}).with_context(|| format!("Installing symlink {:?} (in root {:?})", rel, roots.tmp))
	}

	fn share_dir(roots: &Roots, source: &Unscoped) -> Result<()> {
		result_block(|| {
			let target: Absolute = roots.project.join(&source.0);
			if !lexists(target.as_path())? {
				fs::create_dir_all(target.as_path())?;
			}
			let link = roots.tmp.join(&source.0);
			Self::_link(roots, link, target)
		}).with_context(|| format!("Linking shared dir {:?} (in root {:?})", source, roots.tmp))
	}
	
	fn _link(roots: &Roots, link: Absolute, target: Absolute) -> Result<()> {
		debug!("Linking {:?} -> {:?}", &link, &target);
		let link: PathBuf = link.into();
		let parent = link.parent().expect("link.parent()");
		debug!("Creating parent directory {:?}", &parent);
		fs::create_dir_all(&parent)?;
		symlink(target.as_path(), &link)
			.with_context(|| format!("fs::symlink({:?}, {:?})", target.as_path(), &link))?;
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

	pub fn run<'a, M: BuildModule>(
		mut project: Mutexed<'a, Project<M>>,
		implicits: &Implicits,
		command: &GenCommand<Unscoped>,
		reason: &BuildReason,
	) -> Result<(Mutexed<'a, Project<M>>, TempDir, InvokeResponse)> {
		let dep_set = reason.parent().and_then(|t| project.get_deps(t)).unwrap_or(DepSet::empty_static()).clone();
		let mut rel_paths = Default::default();
		for (dep, result) in dep_set.deps.iter() {
			Self::collect_paths(&project, &mut rel_paths, dep, &result.result)
				.context("initializing command sandbox")?;
		}
		
		let GenCommand { exe, args, cwd, env, env_inherit, output, input, impure_share_dirs } = command;
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
		
		let writer = project.writer().to_owned();
		let (tmp, response) = project.unlocked_block(|project_handle| {
			let tmp = tempdir::TempDir::new("ambl")?;
			debug!("created command sandbox {:?}", &tmp);
			let roots = Roots {
				tmp: CPath::try_from(tmp.path().to_owned())?.into_absolute()?,
				project: CPath::try_from(current_dir()?)?.into_absolute()?,
			};
			
			cmd.args(args);
			
			let mut output_proxy = OutputProxy::new();

			cmd.stdin(match input {
				build::Stdin::Inherit => Stdio::inherit(),
				build::Stdin::Value(v) => todo!(),
				build::Stdin::Null => Stdio::null(),
			});

			cmd.stdout(match output.stdout {
				build::Stdout::Collect => Stdio::piped(),
				build::Stdout::Inherit => {
					output_proxy.source.replace(OutputStream::Stdout);
					Stdio::piped()
				},
				build::Stdout::Ignore => Stdio::null(),
			});
			
			cmd.stderr(match output.stderr {
				build::Stderr::Merge => {
					unsafe { Stdio::from_raw_fd(1) }
				},
				build::Stderr::Inherit => {
					if output_proxy.source.is_some() {
						// inherit and inherit, that means Merge...
						unsafe { Stdio::from_raw_fd(1) }
					} else {
						output_proxy.source.replace(OutputStream::Stderr);
						Stdio::piped()
					}
				},
				build::Stderr::Ignore => Stdio::null(),
			});
			
			// TODO: actual sandboxing.
			// Use something like user namespacing to shadow
			// all directories the command shouldn't have access to

			debug!("Installing {} symlinks", rel_paths.len());
			for (k, v) in rel_paths {
				Self::install_symlink(&roots, &k, v.as_ref())?;
			}

			debug!("Installing {} symlinks to impure_shared_dirs", impure_share_dirs.len());
			for dir in impure_share_dirs {
				Self::share_dir(&roots, dir)?;
			}

			// build up actual CWD from temp + scope + explicit CWD
			let mut full_cwd: PathBuf = roots.tmp.to_owned().into();
			if let Some(cwd) = cwd {
				// TODO warn if CWD is absolute?
				full_cwd.push(&cwd.0);
			}
			cmd.current_dir(&full_cwd);

			if log_enabled!(log::Level::Debug) {
				debug!("+ {:?}", &cmd);
			} else {
				info!("+ {:?}", &exe);
			}

			let response = result_block(|| {
				std::fs::create_dir_all(&full_cwd)?;
				let mut proc = cmd.spawn()?;
				
				let otuput_proxy_handle = output_proxy.spawn(writer, &mut proc)?;
				let response = match output.stdout {
					build::Stdout::Collect => {
						let mut s: Vec<u8> = Vec::new();
						proc.stdout.take().expect("stdout pipe").read_to_end(&mut s)?;
						InvokeResponse::Bytes(s)
					},
					_ => InvokeResponse::Unit,
				};
				drop(otuput_proxy_handle);
				let result = proc.wait()?;
				debug!("- {:?}: {:?}", &cmd, &result);
				std::thread::sleep(std::time::Duration::from_millis(500));
				if result.success() {
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

#[derive(Default)]
struct OutputProxy {
	source: Option<OutputStream>,
	thread: Option<JoinHandle<()>>,
}

impl OutputProxy {
	fn new() -> Self { Default::default() }
	
	fn read_thread<P: Read + Send + 'static>(&self, writer: ui::Writer, p: P) -> io::Result<JoinHandle<()>> {
		thread::Builder::new()
			.spawn(move || {
				let mut reader = BufReader::new(p);
				let mut buf = String::with_capacity(1024);
				loop {
					let len = reader.read_line(&mut buf).unwrap_or_else(|e| {
						error!("failed reading data from child process: {}", e);
						0
					});
					if len == 0 {
						break;
					}
					writer.emit(ui::Event::Line(&buf));
					buf.clear();
				}
		})
	}

	fn spawn(&mut self, writer: ui::Writer, proc: &mut Child) -> Result<()> {
		match self.source {
			Some(source) => {
				let spawned = match source {
					OutputStream::Stdout => self.read_thread(writer, proc.stdout.take().expect("child output pipe")),
					OutputStream::Stderr => self.read_thread(writer, proc.stderr.take().expect("child output pipe")),
				};
				self.thread = Some(spawned?);
				Ok(())
			},
			None => Ok(()),
		}
	}
}
impl Drop for OutputProxy {
	fn drop(&mut self) {
		if let Some(thread) = self.thread.take() {
			thread.join().expect("thread join on Drop");
		}
	}
}

