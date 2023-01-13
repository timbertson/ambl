use log::*;
use std::{fs, sync::{Arc, Mutex}};
use ambl_common::rule::dsl;
use serial_test::serial;

use anyhow::*;
use crate::build_request::BuildRequest;
use crate::{module::*, test::test_module::{TestModule, TestProject, Log}, project::Project};
use ambl_common::{rule::dsl::*, build::{DependencyRequest, FileDependency}};
use super::util::*;

use super::test_module::DEFAULT_BUILD_FN;

#[test]
#[serial]
fn simple_build() -> Result<()> {
	TestProject::in_tempdir(|p: &TestProject| {
		p.target_builder("a", |p, c| {
			p.record("built!");
			c.write_dest("output")
		})
		.build_file("a")?;

		eq!(p.log(), vec!("built!"));
		eq!(fs::read_to_string(".ambl/out/a")?, "output".to_owned());
		Ok(())
	})
}

#[test]
#[serial]
fn rebuild_if_dep_changes() -> Result<()> {
	TestProject::in_tempdir(|p: &TestProject| {
		p.target_builder("a", |p, c| {
			let contents = c.read_file("dep")?;
			p.record(format!("built from: {}", contents));
			c.empty_dest()
		});
		p.write_file("dep", "1")?;

		p.build_file("a")?;
		p.build_file("a")?;
		eq!(p.log().reset(), vec!("built from: 1"));
		
		println!("");

		p.write_file("dep", "2")?;
		p.build_file("a")?;
		p.build_file("a")?;

		eq!(p.log().reset(), vec!("built from: 2"));
		Ok(())
	})
}

#[test]
#[serial]
fn unchanged_rule_module_is_not_evaluated() -> Result<()> {
	TestProject::in_tempdir(|p: &TestProject| {
		let m = p.new_module().rule_fn(|m, ctx| {
			m.project.record("get_rules");
			Ok(vec!(target("a", m.default_build_fn())))
		}).builder(|p, ctx| {
			p.record("build");
			ctx.empty_dest()
		});
		p.inject_rules_module(m);
		
		p.build_file("a")?;
		p.build_file("a")?;
		eq!(p.log(), vec!("get_rules", "build"));
		Ok(())
	})
}

#[test]
#[serial]
fn rule_change_causes_rebuild() -> Result<()> {
	TestProject::in_tempdir(|p: &TestProject| {
		let build_m = p.new_module().set_name("builder").builder(|p, ctx| {
			p.record("build");
			ctx.empty_dest()
		});

		let rule_m = p.new_module().rule_fn(|m, ctx| {
			m.project.record("get_rules");
			Ok(vec!(target("a", function("build").module("builder"))))
		});
		let m_name = rule_m.name.to_owned();
		p.inject_rules_module(rule_m);
		p.inject_module(build_m);

		p.build_file("a")?;

		eq!(p.log().reset(), vec!("get_rules", "build"));

		p.touch_fake("builder");
		p.build_file("a")?;
		eq!(p.log(), vec!("build"));
		Ok(())
	})
}

#[test]
#[serial]
fn equivalent_rule_module_does_not_cause_rebuild() -> Result<()> {
	TestProject::in_tempdir(|p: &TestProject| {
		let build_m = p.new_module().set_name("builder").builder(|p, ctx| {
			p.record("build");
			ctx.empty_dest()
		});

		let rule_m = p.new_module().rule_fn(|m, ctx| {
			m.project.record("get_rules");
			Ok(vec!(target("a", function("build").module("builder"))))
		});
		let m_name = rule_m.name.to_owned();
		p.inject_rules_module(rule_m);
		p.inject_module(build_m);

		p.build_file("a")?;

		eq!(p.log().reset(), vec!("get_rules", "build"));

		p.touch_fake(&m_name);
		p.build_file("a")?;
		eq!(p.log(), vec!("get_rules"));
		Ok(())
	})
}

#[test]
#[serial]
fn rebuild_on_transitive_dep_change() -> Result<()> {
	TestProject::in_tempdir(|p: &TestProject| {
		p.target_builder("b", |p, c| {
			let contents = c.read_file("a")?;
			p.record("build b");
			fs::write(c.dest(), format!("{} -> b", contents))?;
			Ok(())
		});
		p.target_builder("c", |p, c| {
			let contents = c.read_file("b")?;
			p.record("build c");
			fs::write(c.dest(), format!("{} -> c", contents))?;
			Ok(())
		});
		p.write_file("a", "1")?;

		eq!(p.build_file_contents("c")?, "1 -> b -> c");
		eq!(p.log().reset(), vec!("build b", "build c"));

		p.build_file("c")?;
		assert_prop!(p.log().reset(), Log::is_empty);

		p.write_file("a", "2")?;
		eq!(p.build_file_contents("c")?, "2 -> b -> c");
		eq!(p.log().reset(), vec!("build b", "build c"));
		
		Ok(())
	})
}

#[test]
#[serial]
fn rebuild_on_fileset_change() -> Result<()> {
	TestProject::in_tempdir(|p: &TestProject| {
		p.target_builder("list", |p, c| {
			let files: Vec<String> = c.list_fileset(fileset(".").include_files("*.txt"))?;
			p.record("build");
			fs::write(c.dest(), format!("{}", files.join("\n")))?;
			Ok(())
		});
		p.write_file("a.txt", "1")?;
		
		// dotfiles are ignored implicitly, as long as they don't match an explicit rule
		p.write_file(".dir/b.txt", "1")?;

		eq!(p.build_file_contents("list")?, "./a.txt");
		eq!(p.log().reset(), vec!("build"));

		// changing file contents doesn't matter
		p.write_file("a.txt", "2")?;
		assert_prop!(p.log().reset(), Log::is_empty);

		p.write_file("subdir/b.txt", "1")?;
		eq!(p.build_file_contents("list")?, "./a.txt\n./subdir/b.txt");
		
		Ok(())
	})
}

#[test]
#[serial]
fn test_does_not_cache_target_failures() -> Result<()> {
	TestProject::in_tempdir(|p: &TestProject| {
		p.target_builder("file", |p, c| {
			if p.log().is_empty() {
				p.record("fail");
				Err(anyhow!("Initial failure"))
			} else {
				p.record("succeed");
				c.empty_dest()
			}
		});

		eq!(p.build_file("file").is_err(), true);
		eq!(p.log(), vec!("fail"));

		p.build_file("file")?;
		eq!(p.log(), vec!("fail", "succeed"));
		Ok(())
	})
}

#[test]
#[serial]
fn test_does_not_cache_wasm_call_failures() -> Result<()> {
	TestProject::in_tempdir(|p: &TestProject| {
		let module = p.new_module().wasm_fn("fn", |p, c| {
			if p.log().is_empty() {
				p.record("fail");
				Err(anyhow!("Initial failure"))
			} else {
				p.record("succeed");
				Ok(())
			}
		});
		let req = DependencyRequest::WasmCall(dsl::function("fn").module(&module.name));

		p.inject_module(module);

		let initial_build = p.build_dep(&req);
		debug!("initial build: {:?}", initial_build);
		eq!(initial_build.is_err(), true);
		eq!(p.log(), vec!("fail"));

		p.build_dep(&req)?;
		eq!(p.log(), vec!("fail", "succeed"));

		Ok(())
	})
}
