use std::{fs, sync::{Arc, Mutex}};
use ambl_common::build::Stdout;
use serial_test::serial;

use anyhow::*;
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
			let files = c.run(cmd("ls").arg("-1").stdout(Stdout::String))?.into_string()?;
			
			// TODO should .ambl/out be symlinked directly into the tempdir?
			let targets = c.run(cmd("ls").arg("-1").arg(".ambl/out").stdout(Stdout::String))?.into_string()?;
			let mut lines: Vec<&str> = files.lines().filter(|l| !l.ends_with(".wasm")).chain(targets.lines()).collect();
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
