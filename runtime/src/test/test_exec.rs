use std::{fs, sync::{Arc, Mutex}};
use ambl_common::build::Stdout;
use ambl_common::ctx::TargetCtx;
use ambl_common::rule::dsl;
use serial_test::serial;

use anyhow::*;
use crate::build::TargetContext;
use crate::{module::*, test::test_module::{TestModule, TestProject, Log}, project::Project};
use ambl_common::{rule::dsl::*, build::{DependencyRequest}};
use super::util::*;

use super::test_module::DEFAULT_BUILD_FN;

#[test]
#[serial]
fn run_can_only_see_dependencies() -> Result<()> {
	TestProject::in_tempdir(|p: &TestProject| {
		p.write_file("behind_checksum", "1")?;
		p.write_file("behind_mtime", "1")?;
		p.write_file("plain_file_dep", "1")?;
		p.write_file("not_a_dep", "1")?;
		p.target_builder("checksum", |p, c| {
			c.build("behind_checksum")?;
			c.write_dest("constant")
		});

		p.target_builder("mtime", |p, c| {
			c.build("behind_mtime")?;
			c.write_dest("constant")?;
			c.disable_checksum()
		});

		p.target_builder("a", |p, c| {
			c.build("mtime")?;
			c.build("checksum")?;
			c.build("plain_file_dep")?;
			let files = c.run(cmd("ls").arg("-1").stdout(Stdout::Return))?.into_string()?;
			let mut lines: Vec<&str> = files.lines().filter(|l| !l.ends_with(".wasm")).collect();
			lines.sort();
			for line in lines {
				p.record(line);
			}
			c.empty_dest()
		}).build_file("a")?;

		eq!(p.log(), vec!(
			"behind_mtime", // transitive is exposed because mtime is not checksummed
			"checksum",
			"mtime",
			"plain_file_dep",
		));
		Ok(())
	})
}


#[test]
#[serial]
fn run_is_influenced_by_implicit_sandbox_config() -> Result<()> {
	TestProject::in_tempdir(|p: &TestProject| {
		let builder = |p: &TestProject, c: &TargetCtx| {
			c.write_dest(
				c.run_output(cmd("bash").arg("-euc").arg("echo ${NIX_FAKE_ENV:-UNSET}"))?
			)
		};

		p.target_builder("fake-env", builder);
		
		p.inject_rules_module(p.new_module().rule_fn(|m, ctx| {
			Ok(vec!(
				target("nix-fake-env", m.default_build_fn()),
				sandbox::nix_compat(),
			))
		}).builder(builder));

		p.inject_rules_module(p.new_module().rule_fn(|m, ctx| {
			Ok(vec!(
				target("allow-fake-env", m.default_build_fn()),
				sandbox::allow_env("NIX_FAKE_ENV"),
			))
		}).builder(builder));

		std::env::set_var("NIX_FAKE_ENV", "test-value");

		eq!("UNSET", p.build_file_contents("fake-env")?);
		eq!("test-value", p.build_file_contents("nix-fake-env")?);
		eq!("test-value", p.build_file_contents("allow-fake-env")?);

		Ok(())
	})
}
