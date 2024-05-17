use std::{fs, sync::{Arc, Mutex}};
use ambl_api::Stderr;
use serial_test::serial;

use anyhow::*;
use crate::{module::*, test::test_module::{TestModule, TestProject}, project::Project};
use ambl_common::{rule::dsl::{*, self}, build::DependencyRequest};
use super::util::*;

use super::test_module::DEFAULT_BUILD_FN;

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
fn test_virtual_paths() -> Result<()> {
	TestProject::in_tempdir(|p| {
		p.target_builder_module(p.new_module().set_mount("mount").set_scope("scope"), "target", |p, ctx| {
			p.record(ctx.read_file("file")?);
			p.record(ctx.read_file("@scope/file")?);
			p.record(ctx.read_file("@root/file")?);
			
			ctx.write_dest(
				ctx.run_output(cmd("bash").arg("-uxc")
					.arg("ls -1 ../; cat file; cat @scope/file; cat @root/file")
					.stderr(Stderr::Merge))?
			)
		});
		
		p.write_file("mount/file", "mounted\n")?;
		p.write_file("mount/scope/file", "mounted and scoped\n")?;
		p.write_file("file", "root file\n")?;

		p.build_file("mount/scope/target")?;
		eq!(p.log(), vec!(
			"mounted\n",
			"mounted and scoped\n",
			"root file\n",
		));
		let built_contents = p.read_file(".ambl/out/mount/scope/target")?;
		eq!(
			built_contents.split('\n').collect::<Vec<_>>(),
			vec!(
				"+ ls -1 ../",
				"file",
				"mount",

				"+ cat file",
				"mounted",

				"+ cat @scope/file",
				"mounted and scoped",

				"+ cat @root/file",
				"root file",
			)
		);
		Ok(())
	})
}
