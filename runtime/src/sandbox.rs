use log::*;
use std::process::{self, Command, Stdio};

use anyhow::*;
use trou_common::build;

use crate::{persist::{DepSet, PersistDependency}, project::{Project, BuildReason}, sync::Mutexed};

pub struct Sandbox {
}

impl Sandbox {
	pub fn run<'a>(project: Mutexed<'a, Project>,
		command: &build::Command,
		reason: &BuildReason,
	) -> Result<Mutexed<'a, Project>> {
		let dep_set = reason.parent().and_then(|t| project.get_deps(t)).unwrap_or(DepSet::empty_static());
		let build::Command { exe, args, cwd, env, output, input } = command;
		let mut cmd = Command::new(exe);
		cmd.args(args);
		if let Some(cwd) = cwd {
			cmd.current_dir(cwd);
		}
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

		info!("+ {:?}", &cmd);
		let result = cmd.status()?;
		if result.success() {
			Ok(project)
		} else {
			Err(anyhow!("Command `{}` failed (exit status: {:?})", exe, &result.code()))
		}
	}
}
