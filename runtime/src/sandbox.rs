use log::*;
use std::{process::{self, Command, Stdio}, env::current_dir, os::unix::fs::symlink, path::PathBuf, fs, collections::HashSet};

use anyhow::*;
use trou_common::build;

use crate::{persist::{DepSet, PersistDependency, Persist}, project::{Project, BuildReason}, sync::{Mutexed, MutexHandle}, path::{AnyPath, Relative, Absolute}, err::result_block};
use crate::DependencyRequest;

pub struct Sandbox {
}

struct Roots {
	tmp: Absolute,
	project: Absolute,
}

impl Sandbox {
	fn install_symlink(roots: &Roots, rel: &Relative) -> Result<()> {
		result_block(|| {
			let link = roots.tmp.join(rel);
			let target = roots.project.join(rel);
			let link: PathBuf = link.into();
			let target: PathBuf = target.into();
			debug!("Linking {:?} -> {:?}", &link, &target);
			let parent = link.parent().expect("link.parent()");
			debug!("Creating parent directory {:?}", &parent);
			fs::create_dir_all(&parent)?;
			symlink(&target, &link)?;
			Ok(())
		}).with_context(|| format!("Installing symlink {:?} (in root {:?})", rel, roots.tmp))
	}
	
	fn collect_paths<'a>(
		mut project: Mutexed<'a, Project>,
		dest: &mut HashSet<Relative>,
		req: DependencyRequest,
	) -> Result<Mutexed<'a, Project>> {
		match req {
			DependencyRequest::FileDependency(ref f) => {
				let rel = AnyPath::relative(f.to_owned())?;
				if dest.contains(&rel) {
					return Ok(project);
				}
				let persist = project.lookup(&req)?
					.ok_or_else(|| anyhow!("Couldn't find result in build cache for: {:?}", req))?;
				match persist {
					Persist::File(Some(_)) => {
						dest.insert(rel);
					},
					Persist::Target(target) => {
						// TODO don't include transitive deps if there is a checksum
						for (req, _) in target.deps.deps.clone().into_iter() {
							// TODO wish we didn't need req.to_owned here
							project = Self::collect_paths(project, dest, req.to_owned())?;
						}
					},
					_ => (),
				}
			},
			_ => (), // not a file or target dependency
		}
		Ok(project)
	}

	pub fn run<'a>(mut project: Mutexed<'a, Project>,
		command: &build::Command,
		reason: &BuildReason,
	) -> Result<Mutexed<'a, Project>> {
		let dep_set = reason.parent().and_then(|t| project.get_deps(t)).unwrap_or(DepSet::empty_static()).clone();
		let mut rel_paths = Default::default();
		for (dep, state) in dep_set.deps.into_iter() {
			project = Self::collect_paths(project, &mut rel_paths, dep.to_owned())
				.context("initializing command sandbox")?;
		}

		let build::Command { exe, args, cwd, env, env_inherit, output, input } = command;
		let mut cmd = Command::new(exe);

		cmd.env_clear();
		for k in env_inherit {
			let (project_ret, value) = Project::build(project, &DependencyRequest::EnvVar(k.to_owned()), reason)?;
			match value {
				PersistDependency::Env(value) => {
					cmd.env(k, value.0);
				},
				_ => panic!("impossible result from Envvar request"),
			}
			project = project_ret;
		}
		for (k, v) in env {
			cmd.env(k, v);
		}
		
		project.unlocked_block(|project_handle| {
			let tmp = tempdir::TempDir::new("trou")?;
			debug!("created command sandbox {:?}", &tmp);
			let roots = Roots {
				tmp: AnyPath::path(tmp.path())?.into_absolute()?,
				project: AnyPath::path(current_dir()?)?.into_absolute()?,
			};
			
			cmd.args(args);

			cmd.stdin(match input {
				build::Stdin::Inherit => Stdio::inherit(),
				build::Stdin::Value(v) => todo!(),
				build::Stdin::Null => Stdio::null(),
			});

			cmd.stdout(match output.stdout {
				build::Stdout::String => todo!(),
				build::Stdout::Inherit => Stdio::inherit(),
				build::Stdout::Ignore => Stdio::null(),
				build::Stdout::WriteTo(_) => todo!(),
				build::Stdout::AppendTo(_) => todo!(),
			});
			
			// TODO: actual sandboxing.
			// Use something like user namespacing to shadow
			// all directories the command shoulnd't have access to

			debug!("Installing {} symlinks", rel_paths.len());
			for rel in rel_paths {
				Self::install_symlink(&roots, &rel)?;
			}

			cmd.current_dir::<PathBuf>(if let Some(cwd) = cwd {
				let rel = AnyPath::relative(cwd.to_owned())?;
				roots.tmp.join(&rel).into()
			} else {
				roots.tmp.into()
			});

			info!("+ {:?}", &cmd);

			let result = cmd.status()
				.with_context(|| format!("Unable to launch command: {:?}", &cmd))?;
			tmp.close()?;
			if result.success() {
				Ok(())
			} else {
				Err(anyhow!("Command `{}` failed (exit status: {:?})", exe, &result.code()))
			}
		})?;
		Ok(project)
	}
}
