use std::{fs, sync::{Arc, Mutex}};

use anyhow::*;
use crate::{module::*, test::test_module::{TestModule, TestProject}, project::{Project, BuildReason}};
use trou_common::{rule::dsl::*, build::{DependencyRequest, FileDependency}};

#[test]
fn test_simple_build() {
	TestProject::in_tempdir(|p: &TestProject| {
		p.target_builder("a", |p| {
			p.record("built!");
			Ok(())
		})
		.build_file("a");

		assert_eq!(p.log(), vec!("built!"));
	});
}
