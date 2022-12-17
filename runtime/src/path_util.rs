use anyhow::*;
use log::*;
use std::{ops::Deref, sync::Arc};
use serde::{Deserialize, Serialize};
use std::{path::{Path, PathBuf, Component}, fs::{self, Metadata}, io, os::unix::prelude::PermissionsExt, borrow::{Cow, Borrow}};
use walkdir::{WalkDir, DirEntry};

// relative, but may contain ../ etc
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Relative(String);

impl Relative {
	// TODO this could be a simple Arc::clone if the path is already normalized
	pub fn normalize_in(&self, scope: &Scope) -> Option<Normalized> {
		let scope_s: Option<String> = scope.into_normalized().map(|x| x.to_owned().into());
		let base: PathBuf = scope_s.map(PathBuf::from).unwrap_or_else(|| PathBuf::new());

		let suffix: PathBuf = self.clone().into();
		let mut result = base.clone();

		for part in suffix.components() {
			match part {
				Component::CurDir => (),
				Component::ParentDir => {
					if !result.pop() {
						return None
					}
				},
				Component::Normal(s) => result.push(s),
				Component::Prefix(_) | Component::RootDir => { panic!("Invalid relative path: {:?}", self) },
			}
		}
		Some(Normalized(result.into_os_string().into_string().expect("invalid path")))
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
		assert_eq!(rel("foo/bar/../baz").normalize_in(None), Some(norm("foo/baz")));
		assert_eq!(rel("foo/.//bar/../baz").normalize_in(Some(&norm("x/y"))), Some(norm("x/y/foo/baz")));
		assert_eq!(rel("../z").normalize_in(Some(&norm("x/y"))), Some(norm("x/z")));
		assert_eq!(rel("../../z").normalize_in(Some(&norm("x"))), None);
		assert_eq!(rel("x/").normalize_in(None), Some(norm("x")));
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

// relative _and_ normalized (no ../ or ./ coponents, no trailing slash)
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct Normalized(String);
impl AsRef<str> for Normalized {
	fn as_ref(&self) -> &str {
		self.0.as_ref()
	}
}

impl Into<PathBuf> for Normalized {
	fn into(self) -> PathBuf {
		PathBuf::from(self.0)
	}
}

impl Into<Relative> for Normalized {
	fn into(self) -> Relative {
		Relative(self.0)
	}
}

impl Into<String> for Normalized {
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

impl Into<String> for Absolute {
	fn into(self) -> String {
		self.0
	}
}

impl AsRef<str> for Absolute {
	fn as_ref(&self) -> &str {
		&self.0
	}
}

#[derive(Debug)]
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

	// TODO this could return a possibly-borrowed scope?
	pub fn normalize_in(&self, scope: &Scope) -> Result<Normalized> {
		self.normalize_in_opt(scope).ok_or_else(|| anyhow!("Normalized scope reqired, got {:?}", self))
	}

	pub fn normalize_in_opt(&self, scope: &Scope) -> Option<Normalized> {
		match self {
			AnyPath::Relative(rel) => rel.normalize_in(scope),
			AnyPath::Absolute(_) => None,
		}
	}
}

impl Into<String> for AnyPath {
	fn into(self) -> String {
		match self {
			AnyPath::Relative(v) => v.into(),
			AnyPath::Absolute(v) => v.into(),
		}
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


#[derive(Debug, PartialEq, Eq)]
pub struct Scope(Option<Arc<Normalized>>);

impl Scope {
	pub fn root() -> Self {
		Scope(None)
	}

	pub fn new(n: Normalized) -> Self {
		Scope(Some(Arc::new(n)))
	}
	
	pub fn into_normalized(&self) -> Option<&Normalized> {
		self.0.as_ref().map(|x| x.deref())
	}

	fn join(&self, sub: &Scope) -> Scope {
		match (&self.0, &sub.0) {
			(Some(base), Some(sub)) => {
				let base_str: &str = base.as_ref().as_ref();
				let sub_str: &str = sub.as_ref().as_ref();
				Self(Some(Arc::new(Normalized(format!("{}/{}", base_str, sub_str)))))
			},
			(None, _) => sub.clone(),
			(_, None) => self.clone(),
		}
	}

	pub fn within_scope<'a, 'b>(&'a self, name: &'b str, sub: &Scope) -> Option<Scoped<&'b str>> {
		match sub.0 {
			Some(ref sub_normalized) => {
				let sub_str : &str = sub_normalized.as_ref().as_ref();
				name.strip_prefix(sub_str).and_then(|s| s.strip_prefix("/")).map(|new_name| {
					let full_scope: Scope = self.join(sub);
					Scoped { scope: full_scope, value: new_name }
				})
			},
			None => Some(Scoped {
				scope: self.clone(),
				value: name
			}),
		}
	}
}

impl Clone for Scope {
	fn clone(&self) -> Self {
		Self(self.0.as_ref().map(Arc::clone))
	}
}

#[derive(Debug)]
pub struct Scoped<T> {
	pub scope: Scope,
	pub value: T,
}

impl<T> Scoped<T> {
	pub fn new(scope: Scope, value: T) -> Self {
		Self { scope, value }
	}
}

impl<'a> Scoped<&'a str> {
	pub fn within_scope(&'a self, sub: &Scope) -> Option<Scoped<&'a str>> {
		self.scope.within_scope(self.value, sub)
	}
}
