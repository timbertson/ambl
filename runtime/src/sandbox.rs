use log::*;
use std::{process::{self, Command, Stdio}, env::current_dir, os::unix::fs::symlink, path::PathBuf, fs, collections::HashSet};

use anyhow::*;
use trou_common::build::{self, FileDependency, GenCommand};

use crate::build::BuildReason;
use crate::build_request::BuildRequest;
use crate::persist::{DepSet, BuildResult};
use crate::project::{Project};
use crate::sync::{Mutexed, MutexHandle};
use crate::path_util::{External, Absolute, Scope, Scoped, CPath, Simple, Unscoped};
use crate::err::result_block;
use crate::module::BuildModule;
use crate::DependencyRequest;

pub struct Sandbox {
}

struct Roots {
	tmp: Absolute,
	project: Absolute,
}

impl Sandbox {
	fn install_symlink(roots: &Roots, rel: &Simple) -> Result<()> {
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
	
	fn collect_paths<'a, 'b, M: BuildModule>(
		project: &'a Project<M>,
		dest: &mut HashSet<Simple>,
		key: &'b BuildRequest,
	) -> Result<()> where 'a : 'b {
		match key {
			BuildRequest::FileDependency(cpath) => {
				if let Result::Ok(rel) = cpath.to_owned().0.into_simple() {
					if dest.contains(&rel) {
						return Ok(());
					}
					let persist = project.lookup(&key)?
						.ok_or_else(|| anyhow!("Couldn't find result in build cache for: {:?}", key))?
						.raw();
					match &persist.result {
						BuildResult::File(Some(_)) => {
							dest.insert(rel);
						},
						BuildResult::Target(file) => {
							if let Some(output) = file.target.to_owned() {
								// don't use `rel`, use the shadow path within .trou/out
								dest.insert(project.dest_path(&Scoped::root(output))?);
							}
							// TODO don't include transitive deps if there is a checksum
							for (key, _) in persist.require_deps()?.iter() {
								Self::collect_paths(project, dest, key)?;
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

	pub fn run<'a, M: BuildModule>(mut project: Mutexed<'a, Project<M>>,
		command: &GenCommand<Unscoped>,
		reason: &BuildReason,
	) -> Result<Mutexed<'a, Project<M>>> {
		let dep_set = reason.parent().and_then(|t| project.get_deps(t)).unwrap_or(DepSet::empty_static()).clone();
		let mut rel_paths = Default::default();
		for (dep, state) in dep_set.deps.iter() {
			Self::collect_paths(&project, &mut rel_paths, dep)
				.context("initializing command sandbox")?;
		}
		
		let GenCommand { exe, args, cwd, env, env_inherit, output, input } = command;
		let mut cmd = Command::new(&exe.0.as_path());

		cmd.env_clear();
		for k in env_inherit {
			let request = BuildRequest::EnvVar(k.to_owned());
			let (project_ret, value) = Project::<M>::build(project, &request, reason)?;
			match value {
				BuildResult::Env(value) => {
					cmd.env(k, value);
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
				tmp: CPath::try_from(tmp.path().to_owned())?.into_absolute()?,
				project: CPath::try_from(current_dir()?)?.into_absolute()?,
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
				Self::install_symlink(&roots, &rel.to_owned().into())?;
			}

			// build up actual CWD from temp + scope + explicit CWD
			let mut full_cwd: PathBuf = roots.tmp.to_owned().into();
			if let Some(cwd) = cwd {
				// TODO warn if CWD is absolute?
				full_cwd.push(&cwd.0);
			}
			cmd.current_dir(full_cwd);

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
