mod target;
mod eval;

#[ambl_api::export]
pub mod build {
	use std::collections::{BTreeMap, HashMap};
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
		info!("{:?}", &ninja_rules);
		
		let builder: FunctionSpec = target_fn!(execute).into();

		let eval_static = |val| {
			Ok(String::from_utf8(eval::evaluate(&ninja_rules.bindings, &UnownedValue::from(val))?)?)
		};
		
		let mut target_map: BTreeMap<String, TargetConfig> = Default::default();

		for build in ninja_rules.builds {
			let ninja::Build {
				outputs,
				implicit_outputs: _,
				rule,
				inputs,
				implicit_inputs,
				order_only_inputs,
				bindings
			} = build;

			let inputs: Vec<String> = inputs.into_iter().map(eval_static).collect::<Result<_>>()?;
			let implicit_inputs: Vec<String> =
				implicit_inputs.into_iter().chain(order_only_inputs.into_iter())
				.map(eval_static).collect::<Result<_>>()?;

			for output in outputs { // TODO inefficient to redo this work per output
				let output_str = eval_static(output)?;

				let rule_name = rule.0;
				let rule = ninja_rules.rules.get(rule_name).ok_or_else(|| anyhow!("can't find build rule {}", rule_name))?;
				let mut owned_bindings: HashMap<String, OwnedValue> = ninja_rules.bindings.iter()
					.map(|(k,v)| (k.to_owned(), v.into()))
					.collect(); // TODO inefficient, repeated each target. Prefer to partially evaluate?

				// add bindings from this build declaration
				for binding in bindings.iter() {
					owned_bindings.insert(binding.name.0.to_owned(), (&binding.value).into());
				}
				
				let target_config = TargetConfig {
					rule: rule.into(),
					bindings: owned_bindings,
					inputs: inputs.clone(), implicit_inputs: implicit_inputs.clone()
				};
				target_map.insert(output_str, target_config);
			}
		}
		
		// now we've defined all the targets, add @scope prefixes to all internally-defined targets
		let all_targets: Vec<String> = target_map.keys().cloned().collect();
		let scope_defined_targets = |vec: &mut Vec<String>| {
			for item in vec.iter_mut() {
				if all_targets.contains(item) {
					item.insert_str(0, "@scope/");
				}
			}
		};
		for config in target_map.values_mut() {
			scope_defined_targets(&mut config.inputs);
			scope_defined_targets(&mut config.implicit_inputs);
		}
		
		target_map.into_iter().map(|(name, config)| {
			Ok(target(name, builder.clone().config(config)?))
		}).collect()
	}
}
