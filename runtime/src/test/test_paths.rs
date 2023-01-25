use std::{fs, sync::{Arc, Mutex}};
use serial_test::serial;

use anyhow::*;
use crate::{module::*, test::test_module::{TestModule, TestProject}, project::Project};
use ambl_common::{rule::dsl::{*, self}, build::{DependencyRequest}};
use super::util::*;

use super::test_module::DEFAULT_BUILD_FN;

#[test]
#[serial]
fn test_paths_of_nested_module() -> Result<()> {
	TestProject::in_tempdir(|p| {
		// Nested build is a module at the root, but which will be invoked from `subdir/` scope.
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
		
		// add a simple root builder for nested-build to call
		p.target_builder("root", |p, ctx| {
			p.record(format!(
				"root build of {} with dest {}",
				ctx.target(),
				ctx.dest().display()
			));
			ctx.empty_dest()
		});

		// define a target (which we'll scope under `subdir/`) which is built by nested-build
		let nested_rule_m = p.new_module().rule_fn(|m, ctx| {
			Ok(vec!(target("a", function("build").module("../nested-build"))))
		});

		p.inject_rule(include(module(&nested_rule_m.name).scope("subdir")));
		p.inject_module(nested_rule_m);
		p.inject_module(nested_build_m);

		p.build_file("subdir/a")?;
		let cwd = std::env::current_dir()?;
		eq!(p.log(), vec!(
			format!("root build of root with dest .ambl/tmp/root"),
			format!("nested build of a with dest .ambl/tmp/subdir/a")
		));

		eq!(fs::read_to_string(cwd.join(".ambl/out/subdir/a"))?, "nested!");
		eq!(fs::read_to_string(cwd.join(".ambl/out/root"))?, "");

		Ok(())
	})
}

#[test]
#[serial]
fn test_module_which_is_itself_a_target() -> Result<()> {
	TestProject::in_tempdir(|p| {
		p.target_builder("my.wasm", |p, ctx| {
			p.record("build wasm");
			ctx.write_dest("(not used...)")
		});

		p.inject_rule(target("a", function("build").module("my.wasm")));
		
		// this isn't going via the FS but we're making sure it gets loaded from the
		// target path (within .ambl/out)
		let implementation = p.new_module().set_name(".ambl/out/my.wasm").builder(|p, ctx| {
			p.record(format!("build {}", ctx.target()));
			ctx.empty_dest()
		});
		p.inject_module(implementation);

		p.build_file("a")?;
		eq!(p.log(), vec!(
			format!("build wasm"),
			format!("build a")
		));
		Ok(())
	})
}
