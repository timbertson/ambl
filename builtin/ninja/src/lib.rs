mod target;
mod eval;

#[ambl_api::export]
pub mod build {
	use std::collections::HashMap;
	use std::default::Default;
	use ninja_build_syntax as ninja;

	use anyhow::{anyhow, *};
	use serde::{Deserialize, Serialize};
	use ambl_api::{FunctionSpec, TargetCtx, *};
	use log::*;
	use crate::target::OwnedValue;

use crate::eval;
use crate::target::{TargetConfig, UnownedValue};

	#[derive(Serialize, Deserialize)]
	struct Config {
		ninja_path: String
	}

	impl Default for Config {
		fn default() -> Self {
			return Self { ninja_path: "build.ninja".to_owned() }
		}
	}
	
	#[derive(Debug, Default)]
	struct NinjaRules<'a> {
		rules: HashMap<String, ninja::Rule<'a>>,
		builds: Vec<ninja::Build<'a>>,
		bindings: HashMap<String, UnownedValue<'a>>,
	}
	
	impl<'a> NinjaRules<'a> {
		fn parse(bytes: &'a[u8]) -> Result<Self> {
			use ninja::Statement;
			let mut result: Self = Default::default();
			for stmt in ninja::parse(bytes) {
				let stmt = stmt?;
				match stmt {
					Statement::Rule(stmt) => {
						result.rules.insert(stmt.name.0.to_owned(), stmt);
					},
					Statement::Build(stmt) => {
						result.builds.push(stmt);
					},

					Statement::Binding(stmt) => {
						result.bindings.insert(stmt.name.0.to_owned(), stmt.value.into());
					},

					Statement::Default(_) => todo!("Default"),
					Statement::Include(_) => todo!("Include"),
					Statement::Pool(_) => todo!("Pool"),
					Statement::Comment(_) => (),
				}
			}
			Ok(result)
		}
	}
	
	pub fn execute(ctx: TargetCtx) -> Result<()> {
		crate::target::execute(ctx)
	}

	pub fn get_rules(ctx: BaseCtx) -> Result<Vec<Rule>> {
		let config = ctx.parse_config::<Config>()?;
		let file_contents = ctx.read_file_bytes(&config.ninja_path)?;
		let ninja_rules = NinjaRules::parse(&file_contents)?;
		warn!("{:?}", &ninja_rules);
		
		let mut result = Vec::new();
		let builder: FunctionSpec = target_fn!(execute).into();
		// let mut targets: Vec<TargetConfig> = Default::default();
		// let bindings = eval::leaf(&ninja_rules.bindings);

		for build in ninja_rules.builds {
			for output in build.outputs { // TODO inefficient to redo this work per output
				let output_str = String::from_utf8(eval::evaluate(&ninja_rules.bindings, &UnownedValue::from(output))?)?;
				let rule_name = build.rule.0;
				let rule = ninja_rules.rules.get(rule_name).ok_or_else(|| anyhow!("can't find build rule {}", rule_name))?;
				let mut bindings: HashMap<String, OwnedValue> = ninja_rules.bindings.iter()
					.map(|(k,v)| (k.to_owned(), v.into()))
					.collect(); // TODO inefficient, repeated each target. Prefer to partially evaluate?

				// add bindings from this build declaration
				for binding in build.bindings.iter() {
					bindings.insert(binding.name.0.to_owned(), (&binding.value).into());
				}

				let target_config = TargetConfig {
					rule: rule.into(),
					bindings: bindings,
				};
				result.push(target(output_str, builder.clone().config(target_config)?));
			}
		}
		Ok(result)
	}
}
