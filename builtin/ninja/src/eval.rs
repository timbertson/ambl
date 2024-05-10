use std::collections::HashMap;
use std::default::Default;
use std::fmt::Debug;
use ninja::ValuePiece;
use ninja_build_syntax::{self as ninja};

use anyhow::{anyhow, *};

use crate::target::{OwnedValue, UnownedValue};

// #[derive(Clone, Copy)]
// pub struct Child<'a, T: Clone> {
// 	value: &'a T,
// 	parent: Inherited<'a, T>,
// }

// // #[derive(Clone, Copy)]
// #[derive(Clone)]
// pub enum Inherited<'a, T: Clone> {
// 	Leaf(&'a T),
// 	Child(&'a T, &'a Inherited<'a, T>),
// }

// impl<'a, T: Clone> Inherited<'a, T> {
// 	pub fn child(&'a self, t: &'a T) -> Inherited<'a, T> {
// 		Inherited::Child(t, self)
// 	}
// }

// pub fn leaf<'a, T : Clone>(t: &'a T) -> Inherited<'a, T> {
// 	Inherited::Leaf(t)
// }

// impl<'a, T: Clone> Inherited<'a, T> {
// 	fn iter(&'a self) -> InheritIter<'a, T> {
// 		InheritIter(Some(*self))
// 	}
// }

pub enum LookupResult<'a> {
	Simple(&'a str),
	Complex(UnownedValue<'a>),
}

pub trait Lookup : Debug {
	fn lookup(&self, key: &str) -> Option<Result<LookupResult>>;
}

impl<'a> Lookup for &'a HashMap<String, UnownedValue<'a>> {
	fn lookup(&self, key: &str) -> Option<Result<LookupResult>> {
		self.get(key).map(|v| Ok(LookupResult::Complex(v.clone())))
	}
}

impl<'a> Lookup for &'a HashMap<String, OwnedValue> {
	fn lookup(&self, key: &str) -> Option<Result<LookupResult>> {
		// TODO: mapping over the value just to avoid a private access seems silly
		self.get(key).map(|v| Ok(LookupResult::Complex(v.into())))
	}
}

impl<'a> Lookup for HashMap<String, OwnedValue> {
	fn lookup(&self, key: &str) -> Option<Result<LookupResult>> {
		// TODO: mapping over the value just to avoid a private access seems silly
		self.get(key).map(|v| Ok(LookupResult::Complex(v.into())))
	}
}

// impl<'a> Lookup for Inherited<'a, HashMap<String, ninja::Value<'a>>> {
// 	fn lookup(&self, key: &str) -> Option<LookupResult> {
// 		match self {
// 			Inherited::Leaf(l) => l.lookup(key),
// 			Inherited::Child(child, parent) => child.lookup(key).or_else(|| parent.lookup(key)),
// 		}
// 	}
// }

// TODO: why does T have to be clone for this impl? Inherited doesn't hold a T, it holds a reference to a T
// impl<'a, T: Clone> Copy for Inherited<'a, T> {}

// struct InheritIter<'a, T: Clone>(Option<Inherited<'a, T>>);

// impl<'a, T: Clone> Iterator for InheritIter<'a, T> {
// 	type Item = &'a T;
	
// 	fn next(&mut self) -> Option<Self::Item> {
// 		match self.0.take() {
// 			None => None,
// 			Some(next) => {
// 				match next {
// 					Inherited::Leaf(t) => Some(t),
// 					Inherited::Child(t, parent) => {
// 						self.0 = Some(*parent);
// 						Some(t)
// 					},
// 				}
// 			},
// 		}
// 	}
// }

/*
The full lookup order for a variable expanded in a build block (or the rule is uses) is:
- Special built-in variables ($in, $out).
- Build-level variables from the build block.
- Rule-level variables from the rule block (i.e. $command).
		(Note from the above discussion on expansion that these are expanded "late", and may make use of in-scope bindings like $in.)
- File-level variables from the file that the build line was in.
- Variables from the file that included that file using the subninja keyword.
*/
pub fn evaluate<'a, L: Lookup + Copy>(
	bindings: L,
	value: &'a UnownedValue<'a>,
) -> Result<Vec<u8>> {
	let mut result: Vec<u8> = Default::default();
	for piece in value.0.iter() {
		match piece {
			ninja::ValuePiece::Reference(variable) => {
				let lookup = bindings.lookup(variable)
					.ok_or_else(|| anyhow!("Unbound variable: {}", variable))
					.with_context(|| format!("From bindings: {:?}", bindings))??;
				match lookup {
					LookupResult::Complex(value) => result.extend(evaluate(bindings, &value)?.as_slice()),
					LookupResult::Simple(value) => result.extend(value.as_bytes()),
				}
			},
			ninja::ValuePiece::Plain(bytes) => result.extend(*bytes),
		}
	}
	Ok(result)
}
