use std::{fs, sync::{Arc, Mutex}};
use serial_test::serial;

use anyhow::*;
use crate::{module::*, test::test_module::{TestModule, TestProject}, project::{Project, BuildReason}};
use trou_common::{rule::dsl::*, build::{DependencyRequest, FileDependency}};

#[test]
#[serial]
fn simple_build() {
	TestProject::in_tempdir(|p: &TestProject| {
		p.target_builder("a", |p, _| {
			Ok(p.record("built!"))
		})
		.build_file("a");

		assert_eq!(p.log(), vec!("built!"));
	});
}

#[test]
#[serial]
fn rebuild_only_if_dep_changes() {
	TestProject::in_tempdir(|p: &TestProject| {
		p.target_builder("a", |p, c| {
			let contents = c.read_file("dep")?;
			Ok(p.record(format!("built from: {}", contents)))
		});
		p.write_file("dep", "1");

		p.build_file("a");
		p.build_file("a");
		
		println!("");

		p.write_file("dep", "2");
		p.build_file("a");
		p.build_file("a");

		assert_eq!(p.log(), vec!("built from: 1", "built from: 2"));
	});
}
