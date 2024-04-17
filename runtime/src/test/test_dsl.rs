use anyhow::*;
use ambl_api::*;

pub fn foo_build(ctx: TargetCtx) -> Result<()> {
	todo!()
}

pub fn foo_rules(ctx: BaseCtx) -> Result<Vec<Rule>> {
	todo!()
}

#[test]
fn test_ways_of_constructing_rules() -> Result<()> {
	let rules: Vec<Rule> = vec!(
		// target DSL
		target("foo", target_fn!(foo_build)),
		target("foo", target_fn!(foo_build).config("test")?),
		target("foo", module("mymod").function("build_fn")),
		target("foo", module("mymod").function("build_fn").config("test")?),

		// include rules DSL
		include("mymod.wasm"),
		include(module("mymod.wasm")),
		include(module("mymod.wasm").function("rules_fn")),
		include(module("mymod.wasm").function("rules_fn").config("test")?),
		include(module("mymod.wasm").function("rules_fn").config("test")?.scope("scope")),
		include(module("mymod.wasm").scope("scope").function("rules_fn").config("test")?),

		include(rule_fn!(foo_rules)),
	);
	Ok(())
}
