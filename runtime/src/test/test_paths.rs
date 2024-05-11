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
				ctx.dest_path().display(),
			));
			fs::write(ctx.dest_path(), "nested!")?;
			Ok(())
		});
		
		// add a simple root builder for nested-build to call
		p.target_builder("root", |p, ctx| {
			p.record(format!(
				"root build of {} with dest {}",
				ctx.target(),
				ctx.dest_path().display()
			));
			ctx.empty_dest()
		});

		// define a target (which we'll scope under `subdir/`) which is built by nested-build
		let nested_rule_m = p.new_module().rule_fn(|m, ctx| {
			Ok(vec!(target("a", module("../nested-build").function("build"))))
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

		p.inject_rule(target("a", module("my.wasm").function("build")));
		
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


#[test]
#[serial]
fn test_vitual_paths() -> Result<()> {
	TestProject::in_tempdir(|p| {
		p.target_builder("target", |p, ctx| {
			p.record(ctx.read_file("file")?);
			p.record(ctx.read_file("@scope/file")?);
			p.record(ctx.read_file("@root/file")?);
			
			ctx.write_dest(
				ctx.run_output(cmd("bash").arg("-euc").arg("ls -1 ../ && cat file @scope/file @root/file"))?
			)
		});
		
		p.write_file("mount/file", "mounted")?;
		p.write_file("mount/scope/file", "mounted and scoped")?;
		p.write_file("file", "root file")?;

		// TODO inject at mount _and_ scope
		
		p.build_file("target")?;
		eq!(p.log(), vec!(
			format!("build wasm"),
			format!("build a")
		));
		eq!(
			p.read_file(".ambl/out/target")?,
			"mount\nmounted\nmounted and scoped\nroot file"
		);
		Ok(())
	})
}
