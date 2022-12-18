use log::*;
use std::{process::{self, Command, Stdio}, env::current_dir, os::unix::fs::symlink, path::PathBuf, fs, collections::HashSet};

use anyhow::*;
use trou_common::build::{self, FileDependency};

use crate::{persist::{DepSet, PersistDependency, Persist, DependencyKey, FileDependencyKey}, project::{Project, BuildReason}, sync::{Mutexed, MutexHandle}, path_util::{AnyPath, Relative, Absolute, Normalized, Scope, Scoped}, err::result_block, module::BuildModule};
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
	
	fn collect_paths<'a, 'b, M: BuildModule>(
		project: &'a Project<M>,
		dest: &mut HashSet<&'b Normalized>,
		key: &'b DependencyKey,
	) -> Result<()> where 'a : 'b {
		match key {
			DependencyKey::FileDependency(FileDependencyKey::Complex(_)) => {
				// TODO proper sandboxing would allow us to shadow all paths, but currently we can
				// only shadow paths within the workspace
			},
			DependencyKey::FileDependency(FileDependencyKey::Simple(ref rel)) => {
				if dest.contains(rel) {
					return Ok(());
				}
				let persist = project.lookup(&key)?
					.ok_or_else(|| anyhow!("Couldn't find result in build cache for: {:?}", key))?
					.raw();
				match persist {
					Persist::File(Some(_)) => {
						dest.insert(rel);
					},
					Persist::Target(target) => {
						// TODO don't include transitive deps if there is a checksum
						for (key, _) in target.deps.deps.iter() {
							Self::collect_paths(project, dest, key)?;
						}
					},
					_ => (),
				}
			},
			_ => (), // not a file or target dependency
		}
		Ok(())
	}

	pub fn run<'a, M: BuildModule>(mut project: Mutexed<'a, Project<M>>,
		command: &Scoped<&build::Command>,
		reason: &BuildReason,
	) -> Result<Mutexed<'a, Project<M>>> {
		let dep_set = reason.parent().and_then(|t| project.get_deps(t)).unwrap_or(DepSet::empty_static()).clone();
		let mut rel_paths = Default::default();
		for (dep, state) in dep_set.deps.iter() {
			Self::collect_paths(&project, &mut rel_paths, dep)
				.context("initializing command sandbox")?;
		}
		
		// relieve borrow on project
		let rel_paths: Vec<Normalized> = rel_paths.into_iter().map(|ptr| ptr.to_owned()).collect();

		let build::Command { exe, args, cwd, env, env_inherit, output, input } = command.value;
		let mut cmd = Command::new(exe);

		cmd.env_clear();
		for k in env_inherit {
			let dependency_request = DependencyRequest::EnvVar(k.to_owned());
			let request = command.with_value(&dependency_request);
			let (project_ret, value) = Project::<M>::build(project, request, reason)?;
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
				tmp: AnyPath::path(tmp.path().to_owned()).into_absolute()?,
				project: AnyPath::path(current_dir()?).into_absolute()?,
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
			if let Some(scope) = command.scope.into_normalized() {
				full_cwd.push::<&str>(scope.as_ref());
			}
			if let Some(cwd) = cwd {
				// TODO warn if CWD is absolute?
				full_cwd.push(cwd);
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
