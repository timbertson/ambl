use anyhow::*;
use log::*;
use std::{path::{Path, PathBuf, Component}, fs::{self, Metadata}, io, os::unix::prelude::PermissionsExt};
use walkdir::{WalkDir, DirEntry};

// relative, but may contain ../ etc
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Relative(String);

impl Relative {
	pub fn normalize_in(&self, scope: Option<&Normalized>) -> Normalized {
		let base: PathBuf = scope.map(|s| s.to_owned().into()).unwrap_or_else(|| PathBuf::new());

		let suffix: PathBuf = self.clone().into();
		let mut result = base.clone();

		for part in suffix.components() {
			match part {
				Component::CurDir => (),
				Component::ParentDir => {
					if !result.pop() {
						result.push("..");
					}
				},
				Component::Normal(s) => result.push(s),
				Component::Prefix(_) | Component::RootDir => { panic!("Invalid relative path: {:?}", self) },
			}
		}
		Normalized(result)
	}
}

#[cfg(test)]
mod test {
	use super::*;
	
	fn rel(s: &str) -> Relative {
		Relative(s.to_owned())
	}

	fn norm(s: &str) -> Normalized {
		Normalized(rel(s).into())
	}

	#[test]
	fn test_normalize() {
		assert_eq!(rel("foo/bar/../baz").normalize_in(None), norm("foo/baz"));
		assert_eq!(rel("foo/bar/../baz").normalize_in(Some(&norm("x/y"))), norm("x/y/foo/baz"));
		assert_eq!(rel("../z").normalize_in(Some(&norm("x/y"))), norm("x/z"));
		assert_eq!(rel("x/").normalize_in(None), norm("x"));
	}
}

impl Into<String> for Relative {
	fn into(self) -> String {
		self.0
	}
}

impl Into<PathBuf> for Relative {
	fn into(self) -> PathBuf {
		PathBuf::from(self.0)
	}
}

// relative _and_ normalized (../ only appear at the start)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Normalized(PathBuf);
impl AsRef<str> for Normalized {
	fn as_ref(&self) -> &str {
		self.0.to_str().expect("invalid path")
	}
}

impl Into<PathBuf> for Normalized {
	fn into(self) -> PathBuf {
		PathBuf::from(self.0)
	}
}

#[derive(Debug, Clone)]
pub struct Absolute(String);
impl Absolute {
	pub fn join(&self, rel: &Relative) -> Absolute {
		Self(format!("{}/{}", self.0, rel.0))
	}
}

impl AsRef<str> for Relative {
	fn as_ref(&self) -> &str {
		&self.0
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

	pub fn path(p: PathBuf) -> Self {
		let s = p.into_os_string().into_string().expect("non UTF path");
		Self::new(s)
	}
}


pub fn rm_rf_and_ensure_parent<P: AsRef<Path>>(p: P) -> Result<()> {
	let p = p.as_ref();
	rm_rf(p)?;
	if let Some(parent) = p.parent() {
		fs::create_dir_all(parent)?;
	}
	Ok(())
}

fn rm_rf(p: &Path) -> Result<()> {
	if let Some(stat) = lstat_opt(p)? {
		ensure_writeable(p, &stat)?;
		if stat.is_dir() {
			for entry in WalkDir::new(p).follow_links(false).contents_first(true) {
				let entry = entry?;
				let stat = entry.metadata()?;
				let p = entry.path();
				ensure_writeable(p, &stat)?;
				if stat.is_dir() {
					fs::remove_dir(p)?;
				} else {
					fs::remove_file(p)?;
				}
			}
		} else {
			fs::remove_file(p)?;
		}
	}
	Ok(())
}

fn ensure_writeable(path: &Path, stat: &Metadata) -> Result<()> {
	let mut perms = stat.permissions();
	let mode = perms.mode();
	let writeable = mode | 0o200;
	if mode != writeable {
		debug!("making writeable: {}", path.display());
		perms.set_mode(writeable);
		fs::set_permissions(path, perms)?;
	}
	Ok(())
}

pub fn lstat_opt<P: AsRef<Path>>(p: P) -> Result<Option<fs::Metadata>> {
	match p.as_ref().symlink_metadata() {
		Result::Ok(m) => Ok(Some(m)),
		Result::Err(e) if e.kind() == io::ErrorKind::NotFound => Ok(None),
		Result::Err(e) => Err(e.into()),
	}
}
