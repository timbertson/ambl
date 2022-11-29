use anyhow::*;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Relative(String);
impl Into<String> for Relative {
	fn into(self) -> String {
		self.0
	}
}

#[derive(Debug, Clone)]
pub struct Absolute(String);
impl Absolute {
	pub fn join(&self, rel: &Relative) -> Absolute {
		Self(format!("{}/{}", self.0, rel.0))
	}
}

impl Into<String> for Absolute {
	fn into(self) -> String {
		self.0
	}
}

impl Into<PathBuf> for Absolute {
	fn into(self) -> PathBuf {
		PathBuf::from(self.0)
	}
}
impl AsRef<str> for Absolute {
	fn as_ref(&self) -> &str {
		&self.0
	}
}

pub enum AnyPath {
	Relative(Relative),
	Absolute(Absolute),
}

impl AnyPath {
	pub fn new(s: String) -> AnyPath {
		if s.starts_with('/') {
			Self::Absolute(Absolute(s))
		} else {
			Self::Relative(Relative(s))
		}
	}

	pub fn into_relative(self) -> Result<Relative> {
		match self {
			AnyPath::Relative(rel) => Ok(rel),
			AnyPath::Absolute(abs) => Err(anyhow!("Not a relative path: {}", abs.0)),
		}
	}

	pub fn into_absolute(self) -> Result<Absolute> {
		match self {
			AnyPath::Relative(rel) => Err(anyhow!("Not an absolute path: {}", rel.0)),
			AnyPath::Absolute(abs) => Ok(abs),
		}
	}

	pub fn relative(s: String) -> Result<Relative> {
		Self::new(s).into_relative()
	}

	pub fn absolute(s: String) -> Result<Absolute> {
		Self::new(s).into_absolute()
	}

	pub fn path<P: AsRef<Path>>(p: P) -> Result<Self> {
		let s = p.as_ref().to_str().ok_or_else(||anyhow!("non UTF path"))?.to_owned();
		Ok(Self::new(s))
	}
}
