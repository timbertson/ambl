use std::{fs, sync::{Arc, Mutex}};
use serial_test::serial;

use anyhow::*;
use crate::{module::*, test::test_module::{TestModule, TestProject}, project::{Project, BuildReason}};
use trou_common::{rule::dsl::{*, self}, build::{DependencyRequest, FileDependency}};
use super::util::*;

use super::test_module::DEFAULT_BUILD_FN;

#[test]
#[serial]
fn test_paths_of_nested_module() -> Result<()> {
	TestProject::in_tempdir(|p| {
		let nested_build_m = p.new_module().set_name("nested-build").builder(|p, ctx| {
			ctx.build("../root")?;
			p.record(format!(
				"nested build of {} with dest {}",
				ctx.target(),
				ctx.dest().display(),
			));
			fs::write(ctx.dest(), "nested!")?;
			Ok(())
		});
		
		p.target_builder("root", |p, ctx| {
			p.record(format!(
				"root build of {} with dest {}",
				ctx.target(),
				ctx.dest().display()
			));
			Ok(())
		});

		let nested_rule_m = p.new_module().rule_fn(|m, ctx| {
			vec!(target("a", build_fn("build").module("nested-build")))
		});

		p.inject_rule(include(module(&nested_rule_m.name).scope("subdir")));
		p.inject_module(nested_rule_m);
		p.inject_module(nested_build_m);

		p.build_file("subdir/a")?;
		let cwd = std::env::current_dir()?;
		eq!(p.log(), vec!(
			format!("root build of root with dest {}/.trou/tmp/root", cwd.display()),
			format!("nested build of a with dest {}/.trou/tmp/subdir/a", cwd.display())
		));

		eq!(fs::read_to_string(format!("{}/.trou/out/subdir/a", cwd.display()))?, "nested!");
		eq!(fs::read_to_string(format!("{}/.trou/out/root", cwd.display()))?, "");

		Ok(())
	})
}
