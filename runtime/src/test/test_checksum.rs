use log::*;
use std::{fs, sync::{Arc, Mutex}};
use ambl_common::rule::dsl;
use serial_test::serial;

use anyhow::*;
use crate::build_request::BuildRequest;
use crate::{module::*, test::test_module::{TestModule, TestProject, Log}, project::Project};
use ambl_common::{rule::dsl::*, build::{DependencyRequest}};
use super::util::*;

use super::test_module::DEFAULT_BUILD_FN;

#[test]
#[serial]
fn skip_unchanged_plain_file() -> Result<()> {
	TestProject::in_tempdir(|p: &TestProject| {
		p.target_builder("a", |p, c| {
			c.build("dep")?;
			p.record("build a");
			c.empty_dest()
		});

		p.write_file("dep", "123")?;
		p.build_file("a")?;
		
		p.write_file("dep", "123")?;
		p.build_file("a")?;

		eq!(p.log(), vec!("build a"));
		Ok(())
	})
}

#[test]
#[serial]
fn skip_unchanged_target() -> Result<()> {
	TestProject::in_tempdir(|p: &TestProject| {
		p.target_builder("dep", |p, c| {
			// also tests that we rebuild if checksums are disabled
			c.always_rebuild()?;
			p.record("build dep");
			c.write_dest("123")
		});

		p.target_builder("a", |p, c| {
			c.build("dep")?;
			p.record("build a");
			c.empty_dest()
		});

		p.build_file("a")?;
		eq!(p.log(), vec!("build dep", "build a"));
		
		p.build_file("a")?;
		eq!(p.log(), vec!("build dep", "build a", "build dep"));
		Ok(())
	})
}

#[ignore]
#[test]
#[serial]
fn broken_symlink_is_dirty_only_if_it_changes_destination() -> Result<()> {
	TestProject::in_tempdir(|p: &TestProject| {
		p.target_builder("a", |p, c| {
			c.build("dep")?;
			p.record("build a");
			c.empty_dest()
		});

		p.target_builder("dep", |p, c| {
			let dest = c.read_file("_dest")?;
			c.run(cmd("ln").arg("-sfn").arg(&dest).arg(c.output_path_str()))?;
			p.record(format!("build dep ({})", &dest));
			c.empty_dest()
		});

		p.write_file("_dest", "dest1")?;
		p.build_file("a")?;
		eq!(p.log().reset(), vec!("build dep (dest1)", "build a"));

		// this will always write a new symlink with a new mtime, but same dest
		p.build_file("a")?;
		eq!(p.log().reset(), vec!("build dep (dest1)"));

		p.write_file("_dest", "dest2")?;
		p.build_file("a")?;
		eq!(p.log(), vec!("build dep (dest2)", "build a"));
		Ok(())
	})
}
