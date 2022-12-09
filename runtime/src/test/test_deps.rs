use std::{fs, sync::{Arc, Mutex}};
use serial_test::serial;

use anyhow::*;
use crate::{module::*, test::test_module::{TestModule, TestProject}, project::{Project, BuildReason}};
use trou_common::{rule::dsl::*, build::{DependencyRequest, FileDependency}};

use super::test_module::DEFAULT_BUILD_FN;

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

// fn rule_change_causes_rebuild() {
// 	// TODO how to insert a rule module
// 	TestProject::in_tempdir(|p: &TestProject| {
// 		p.target_builder("a", |p, c| {
// 			let contents = c.read_file("dep")?;
// 			Ok(p.record(format!("built from: {}", contents)))
// 		});
// 		p.write_file("dep", "1");

// 		p.build_file("a");
// 		p.build_file("a");
		
// 		println!("");

// 		p.write_file("dep", "2");
// 		p.build_file("a");
// 		p.build_file("a");

// 		assert_eq!(p.log(), vec!("built from: 1", "built from: 2"));
// }

// fn equivalent_rule_skips_rebuild() {
// }

#[test]
#[serial]
fn unchanged_rule_module_is_not_evaluated() {
	TestProject::in_tempdir(|p: &TestProject| {
		let m = p.new_module().rule_fn(|m, ctx| {
			m.project.record("get_rules");
			vec!(target("a", m.default_build_fn()))
		}).builder(|p, ctx| {
			p.record("build");
			Ok(())
		});
		p.inject_rules_module(m);
		
		p.build_file("a");
		p.build_file("a");
		assert_eq!(p.log(), vec!("get_rules", "build"));
	});
}
