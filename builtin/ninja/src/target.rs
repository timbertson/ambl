use std::collections::HashMap;
use std::fmt;
use ninja_build_syntax::{self as ninja};

use anyhow::{anyhow, *};
use serde::{Deserialize, Serialize};
use ambl_api::{TargetCtx, *};
use log::*;

use crate::eval::{self, Lookup, LookupResult};

// For convenience, we currently use owned versions of all ninja
// structs. We can't use references for raw bytes
// when using JSON serialization. TODO we can likely
// use Cow for most owned strings

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct OwnedIdentifier(String);
impl<'a> From<&'a ninja::Identifier<'a>> for OwnedIdentifier {
	fn from(value: &'a ninja::Identifier<'a>) -> Self {
		Self(value.0.into())
	}
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum OwnedValuePiece {
	Reference(OwnedIdentifier),
	Plain(Vec<u8>),
}
impl<'a> From<&'a ninja::ValuePiece<'a>> for OwnedValuePiece {
	fn from(value: &'a ninja::ValuePiece<'a>) -> Self {
		match value {
			ninja::ValuePiece::Plain(x) => Self::Plain(x.to_vec()),
			ninja::ValuePiece::Reference(x) => Self::Reference(OwnedIdentifier((*x).to_owned())),
		}
	}
}
impl<'a> Into<ninja::ValuePiece<'a>> for &'a OwnedValuePiece {
	fn into(self) -> ninja::ValuePiece<'a> {
		match self {
			OwnedValuePiece::Plain(ref v) => ninja::ValuePiece::Plain(v.as_slice()),
			OwnedValuePiece::Reference(ref v) => ninja::ValuePiece::Reference(v.0.as_str()),
		}
	}
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct OwnedValue(pub Vec<OwnedValuePiece>);
impl OwnedValue {
	pub fn reference(r: String) -> Self {
		Self(vec!(OwnedValuePiece::Reference(OwnedIdentifier(r))))
	}
}
impl<'a> From<&'a ninja::Value<'a>> for OwnedValue {
	fn from(value: &'a ninja::Value<'a>) -> Self {
		Self(value.into_iter().map(|v| v.into()).collect())
	}
}

impl<'a> From<&'a UnownedValue<'a>> for OwnedValue {
	fn from(value: &'a UnownedValue<'a>) -> Self {
		Self(value.0.iter().map(|v| v.into()).collect())
	}
}

impl<'a> Into<UnownedValue<'a>> for &'a OwnedValue {
	fn into(self) -> UnownedValue<'a> {
		UnownedValue::from(self.0.iter().map(|x| x.into()))
	}
}

// ninja crate has a private vec, not sure if that'a intentional
#[derive(Clone, Debug)]
pub struct UnownedValue<'a>(pub Vec<ninja::ValuePiece<'a>>);
impl<'a> UnownedValue<'a> {
	pub fn reference(r: &'a str) -> Self {
		Self(vec!(ninja::ValuePiece::Reference(r)))
	}
}

impl<'a, I: IntoIterator<Item = ninja::ValuePiece<'a>>> From<I> for UnownedValue<'a> {
	fn from(value: I) -> Self {
		UnownedValue(value.into_iter().collect())
	}
}

// #[derive(Clone, Debug, Serialize, Deserialize)]
// pub struct OwnedBinding {
// 	pub name: OwnedIdentifier,
// 	pub value: OwnedValue,
// }

// impl<'a> From<&'a ninja::Binding<'a>> for OwnedBinding {
// 	fn from(value: &'a ninja::Binding<'a>) -> Self {
// 		let ninja::Binding { name, value } = value;
// 		Self {
// 			name: name.into(),
// 			value: value.into(),
// 		}
// 	}
// }

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct OwnedRule {
	pub name: OwnedIdentifier,
	pub bindings: HashMap<String, OwnedValue>,
}

impl<'a> From<&'a ninja::Rule<'a>> for OwnedRule {
	fn from(value: &'a ninja::Rule<'a>) -> Self {
		let ninja::Rule { name, bindings } = value;
		OwnedRule {
			name: name.into(),
			bindings: bindings.iter().map(|b| (b.name.0.to_owned(), (&b.value).into())).collect(),
		}
	}
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct TargetConfig {
	pub bindings: HashMap<String, OwnedValue>,
	pub rule: OwnedRule,
}

#[derive(Clone, Copy)]
pub struct RuleBindings<'a> {
	config: &'a TargetConfig,
	ctx: &'a TargetCtx,
}

impl<'a> fmt::Debug for RuleBindings<'a> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.debug_struct("RuleBindings")
			.field("config_keys", &self.config.bindings.keys())
			.field("rule_keys", &self.config.rule.bindings.keys())
			.finish()
	}
}

impl<'a> Lookup for RuleBindings<'a> {
	fn lookup(&self, key: &str) -> Option<Result<eval::LookupResult>> {
		self.config.bindings.lookup(key).or_else(||
			self.config.rule.bindings.lookup(key).or_else(|| {
				match key {
					"in" => Some(Ok(LookupResult::Simple("TODO".to_owned()))),
					"out" => {
						let dest_path = self.ctx.dest_path_str();
						// TODO: remove when paths.md is fully implemented
						let hack_path = format!("@root/{}", dest_path);
						Some(Ok(LookupResult::Simple(hack_path)))
					},
					_ => None,
				}
			})
		)
	}
}

pub fn execute(ctx: TargetCtx) -> Result<()> {
	let config_raw = ctx.config.as_ref().ok_or_else(|| anyhow!("config required"))?;
	let config = TargetConfig::deserialize(config_raw)?;
	let bindings = RuleBindings {
		config: &config,
		ctx: &ctx,
	};
	warn!("config: {:?}", config);

	// TODO should we be splicing this?
	let bash_command = eval::evaluate(bindings, &UnownedValue::reference("command"))?;
	ctx.run(cmd("bash").arg("-euxc").arg(String::from_utf8(bash_command)?))?;
	Ok(())
	// ctx.write_dest("TEST")
}
