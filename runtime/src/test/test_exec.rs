use std::{fs, sync::{Arc, Mutex}};
use ambl_common::build::Stdout;
use serial_test::serial;

use anyhow::*;
use crate::{module::*, test::test_module::{TestModule, TestProject, Log}, project::Project};
use ambl_common::{rule::dsl::*, build::{DependencyRequest, FileDependency}};
use super::util::*;

use super::test_module::DEFAULT_BUILD_FN;

#[test]
#[serial]
fn run_can_only_see_dependencies() -> Result<()> {
	TestProject::in_tempdir(|p: &TestProject| {
		p.write_file("dep_1", "1")?;
		p.write_file("dep_2", "1")?;
		p.write_file("not_a_dep", "1")?;
		p.target_builder("a", |p, c| {
			c.build("dep_1")?;
			c.build("dep_2")?;
			let output = c.run(cmd("ls").arg("-1").stdout(Stdout::String))?.into_string()?;
			let mut lines: Vec<&str> = output.lines().filter(|l| !l.ends_with(".wasm")).collect();
			lines.sort();
			for line in lines {
				p.record(line);
			}
			Ok(())
		}).build_file("a")?;

		eq!(p.log(), vec!("dep_1", "dep_2"));
		Ok(())
	})
}
