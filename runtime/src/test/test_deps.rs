use std::{fs, sync::{Arc, Mutex}};
use serial_test::serial;

use anyhow::*;
use crate::{module::*, test::test_module::{TestModule, TestProject}, project::{Project, BuildReason}};
use trou_common::{rule::dsl::*, build::{DependencyRequest, FileDependency}};
use super::util::*;

use super::test_module::DEFAULT_BUILD_FN;

#[test]
#[serial]
fn simple_build() -> Result<()> {
	TestProject::in_tempdir(|p: &TestProject| {
		p.target_builder("a", |p, _| {
			Ok(p.record("built!"))
		})
		.build_file("a")?;

		eq!(p.log(), vec!("built!"));
		Ok(())
	})
}

#[test]
#[serial]
fn rebuild_if_dep_changes() -> Result<()> {
	TestProject::in_tempdir(|p: &TestProject| {
		p.target_builder("a", |p, c| {
			let contents = c.read_file("dep")?;
			Ok(p.record(format!("built from: {}", contents)))
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
			vec!(target("a", m.default_build_fn()))
		}).builder(|p, ctx| {
			p.record("build");
			Ok(())
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
			Ok(p.record("build"))
		});

		let rule_m = p.new_module().rule_fn(|m, ctx| {
			m.project.record("get_rules");
			vec!(target("a", build_fn("build").module("builder")))
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
			Ok(p.record("build"))
		});

		let rule_m = p.new_module().rule_fn(|m, ctx| {
			m.project.record("get_rules");
			vec!(target("a", build_fn("build").module("builder")))
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
			fs::write(c.path(), format!("{} -> b", contents))?;
			Ok(())
		});
		p.target_builder("c", |p, c| {
			let contents = c.read_file("b")?;
			p.record("build c");
			fs::write(c.path(), format!("{} -> c", contents))?;
			Ok(())
		});
		p.write_file("a", "1")?;

		eq!(p.build_file_contents("c")?, "1 -> b -> c");
		eq!(p.log().reset(), vec!("build b", "build c"));

		p.build_file("c")?;
		eq!(p.log().reset(), vec!());


		p.write_file("a", "2")?;
		eq!(p.build_file_contents("c")?, "2 -> b -> c");
		eq!(p.log().reset(), vec!("build b", "build c"));
		
		Ok(())
	})
}
