use anyhow::*;
use lazy_static;
use log::*;
use core::fmt;
use std::ffi::OsStr;
use std::iter;
use std::{ops::Deref, sync::Arc, fmt::{Display, Debug}, path::Components};
use serde::{Deserialize, Serialize};
use std::{path::{Path, PathBuf, Component}, fs::{self, Metadata}, io, os::unix::prelude::PermissionsExt, borrow::{Cow, Borrow}};
use walkdir::{WalkDir, DirEntry};

/*
Paths in ambl:

For simplicity, all paths are normalized _without_ regard to symlinks. foo/../bar is always terated the same as `bar`.
No normalized path contains a `.` component, or ends with a slash, unless they are literally `.` or `/`

Absolute paths start with /
External paths start with ../
Simple paths are just a series of path component, with no `..`

# Usage:

Scopes are represented as a Simple path from the project root.
File dependencies can be any kind.
*/

pub fn string_of_pathbuf(p: PathBuf) -> String {
	p.into_os_string().into_string().unwrap_or_else(|os| panic!("Invalid path: {:?}", os))
}

pub fn str_of_os(p: &OsStr) -> &str {
	p.to_str().unwrap_or_else(|| panic!("Invalid path"))
}

pub fn str_of_path<P: AsRef<Path> + ?Sized>(p: &P) -> &str {
	str_of_os(p.as_ref().as_os_str())
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

pub fn lexists<P: AsRef<Path>>(p: P) -> Result<bool> {
	Ok(lstat_opt(p)?.is_some())
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize, Hash)]
#[serde(into = "Option<Simple>", from = "Option<Simple>")]
pub struct Scope(Option<Arc<Simple>>);

impl Scope {
	pub fn root() -> Self {
		Scope(None)
	}

	pub fn new(n: Simple) -> Self {
		Scope(Some(Arc::new(n)))
	}

	pub fn from_normalized(n: Option<Simple>) -> Self {
		Scope(n.map(Arc::new))
	}
	
	// TODO rename as_simple
	pub fn into_simple(&self) -> Option<&Simple> {
		self.0.as_ref().map(|x| x.deref())
	}
	
	pub fn join(&self, sub: CPath) -> CPath {
		match &self.0 {
			Some(base) => base.0.join(&sub),
			None => sub
		}
	}

	pub fn join_simple(&self, sub: Simple) -> Simple {
		match &self.0 {
			Some(base) => Simple(base.0.join(&sub)),
			None => sub
		}
	}
}

impl Clone for Scope {
	fn clone(&self) -> Self {
		Self(self.0.as_ref().map(Arc::clone))
	}
}

impl Into<Option<Simple>> for Scope {
	fn into(self) -> Option<Simple> {
		self.0.as_deref().cloned()
	}
}

impl From<Option<Simple>> for Scope {
	fn from(n: Option<Simple>) -> Self {
		Self(n.map(Arc::new))
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

	pub fn root(value: T) -> Self {
		Self { scope: Scope::root(), value }
	}
	
	pub fn replace_value<R>(self, value: R) -> Scoped<R> {
		Scoped { scope: self.scope, value }
	}
	
	pub fn with_value<R>(&self, value: R) -> Scoped<R> {
		let Scoped { scope, value: _ } = self;
		Scoped::new(scope.clone(), value)
	}
	
	pub fn map<R, F: FnOnce(T) -> R>(self, f: F) -> Scoped<R> {
		let Scoped { scope, value } = self;
		Scoped { scope, value: f(value) }
	}

	pub fn map_ref<R, F: FnOnce(&T) -> R>(&self, f: F) -> Scoped<R> {
		let Scoped { scope, value } = self;
		Scoped { scope: scope.clone(), value: f(&value) }
	}

	pub fn as_ref(&self) -> Scoped<&T> {
		Scoped { scope: self.scope.clone(), value: &self.value }
	}
}

impl<T: Display> Display for Scoped<T> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		if let Some(scope) = self.scope.into_simple() {
			Display::fmt(&scope, f)?;
			write!(f, "/")?;
		}
		Display::fmt(&self.value, f)
	}
}

impl Scoped<Simple> {
	// flatten a scoped simple into a single simple
	pub fn flatten(&self) -> Simple {
		match self.scope.0 {
			None => self.value.to_owned(),
			Some(ref scope) => Simple(scope.0.join(&self.value)),
		}
	}
}

impl <C: Clone> Clone for Scoped<C> {
	fn clone(&self) -> Self {
		Self { scope: self.scope.clone(), value: self.value.clone() }
	}
}

// a "canonical" PathBuf which is:
// - always a valid string
// - normalized, i.e. contains no trailing `/`, and no internal `../` (but may start with one or more ../ segments)
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum CPath {
	P(PathBuf),
	Cwd,
}

lazy_static::lazy_static! {
	static ref CWD_PATH: PathBuf = PathBuf::from(".");
}

lazy_static::lazy_static!{
	static ref BUILTINS_ROOT: PathBuf = {
		PathBuf::from(match option_env!("PREFIX") {
			Some(prefix) => format!("{}/share/builtins", prefix),
			None => format!("{}/../target/wasm32-unknown-unknown/debug", env!("CARGO_MANIFEST_DIR")),
		})
	};
}

impl CPath {
	pub fn new(mut s: String) -> Self {
		if let Some(builtin) = s.strip_prefix("builtin:") {
			let mut owned_path = BUILTINS_ROOT.to_owned();
			owned_path.push(format!("ambl_builtin_{}.wasm", builtin));
			s = string_of_pathbuf(owned_path)
		}

		Self::canonicalize(PathBuf::from(s))
	}

	pub fn as_path(&self) -> &Path {
		match self {
			Self::P(p) => p.as_ref(),
			Self::Cwd => &CWD_PATH,
		}
	}

	pub fn as_str(&self) -> &str {
		self.as_ref()
	}

	pub fn into_simple(self) -> Result<Simple> {
		if self.kind() == Kind::Simple {
			Ok(Simple(self))
		} else {
			Err(anyhow!("Not a simple path: {}", self))
		}
	}

	pub fn into_simple_or_self(self) -> Result<Simple, Self> {
		if self.kind() == Kind::Simple {
			Result::Ok(Simple(self))
		} else {
			Result::Err(self)
		}
	}

	pub fn into_absolute(self) -> Result<Absolute> {
		if self.kind() == Kind::Absolute {
			Ok(Absolute(self))
		} else {
			Err(anyhow!("Not an absolute path: {}", self))
		}
	}

	fn is_canon(p: &PathBuf) -> bool {
		let s: &str = str_of_path(p);
		// treat traling slash as non-canon (unless it's "/")
		if s.ends_with(|c| c == '/' || c == '\\') && s != "/" {
			false
		} else {
			Self::is_canon_components(p.components(), true)
		}
	}

	fn is_canon_components(mut components: Components, is_start: bool) -> bool {
		match components.next() {
			None => true,
			Some(Component::CurDir) => false,
			Some(Component::ParentDir) => is_start && Self::is_canon_components(components, is_start),
			
			// Once we hit a root / normal, set is_start to false
			Some(Component::RootDir | Component::Normal(_) | Component::Prefix(_)) =>
				Self::is_canon_components(components, false),
		}
	}

	fn canonicalize(orig: PathBuf) -> Self {
		if Self::is_canon(&orig) {
			Self::P(orig)
		} else {
			let mut ret = PathBuf::new();
			if iter::once(Component::CurDir).eq(orig.components()) {
				return Self::Cwd
			}
			for part in orig.components() {
				match part {
					Component::RootDir => ret.push(Component::RootDir),
					Component::CurDir => (),
					Component::ParentDir => {
						if !ret.pop() {
							ret.push("..");
						}
					},
					Component::Normal(s) => ret.push(s),
					Component::Prefix(_) => todo!(),
				}
			}
			Self::P(ret)
		}
	}
	
	fn kind(&self) -> Kind {
		match self {
			Self::Cwd => Kind::External,
			Self::P(p) => match p.components().next().expect("empty path") {
				Component::RootDir => Kind::Absolute,
				Component::ParentDir => Kind::External,
				Component::Normal(s) => Kind::Simple,
				Component::CurDir | Component::Prefix(_) => panic!("Invalid CPath"),
			}
		}
	}
	
	pub fn join(&self, other: &CPath) -> Self {
		match (self, other) {
			(Self::Cwd, other) => other.to_owned(),
			(other, Self::Cwd) => other.to_owned(),
			(Self::P(p_self), Self::P(p_other)) => {
				Self::P(match other.kind() {
					Kind::Simple => {
						// Simples can just be concatenated
						let path: &Path = self.as_ref();
						let mut buf = p_self.to_owned();
						buf.push::<&Path>(p_other);
						buf
					},

					Kind::Absolute => {
						// ignore self
						p_other.to_owned()
					},

					Kind::External => {
						// Appending an external might create any output type, depending on
						// the kind of self and how many parent components there are
						let mut ret: PathBuf = p_self.to_owned();
						for component in p_other.components() {
							match component {
								Component::ParentDir => {
									if !ret.pop() {
										ret.push("..");
									}
								},
								Component::Normal(n) => ret.push(n),
								Component::RootDir | Component::Prefix(_) | Component::CurDir => panic!("invalid path"),
							}
						}
						ret
					},
				})
			},
		}
	}
}

impl Display for CPath {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let path: &Path = self.as_ref();
		Display::fmt(&path.display(), f)
	}
}

impl AsRef<CPath> for CPath {
	fn as_ref(&self) -> &CPath {
		self
	}
}

impl TryFrom<PathBuf> for CPath {
	type Error = Error;
	fn try_from(p: PathBuf) -> Result<Self> {
		let s = p.into_os_string().into_string().map_err(|_| {
			anyhow!("Path is not valid utf8")
		})?;
		Ok(CPath::new(s))
	}
}

impl Into<String> for CPath {
	fn into(self) -> String {
		let pb: PathBuf = self.into();
		string_of_pathbuf(pb)
	}
}

impl Into<PathBuf> for CPath {
	fn into(self) -> PathBuf {
		match self {
			CPath::P(p) => p,
			CPath::Cwd => CWD_PATH.to_owned(),
		}
	}
}

impl AsRef<Path> for CPath {
	fn as_ref(&self) -> &Path {
		self.as_path()
	}
}

impl AsRef<str> for CPath {
	fn as_ref(&self) -> &str {
		str_of_path(self.as_path())
	}
}

// Small wrapper for declaring that a path is relative to the project root, and does not need a scope
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Unscoped(pub CPath);
impl Unscoped {
	pub fn new(s: String) -> Unscoped {
		Unscoped(CPath::new(s))
	}
}

impl Unscoped {
	// merge a scope and a CPath into a single path
	pub fn from(path: CPath, scope: &Scope) -> Unscoped {
		Unscoped(match scope.0 {
			None => path,
			Some(ref scope) => scope.0.join(path.as_ref()),
		})
	}

	pub fn from_ref(path: &CPath, scope: &Scope) -> Unscoped {
		Unscoped(match scope.0 {
			None => path.to_owned(),
			Some(ref scope) => scope.0.join(path),
		})
	}

	pub fn from_scoped<P: AsRef<CPath>>(path: &Scoped<P>) -> Unscoped {
		Self::from_ref(&path.value.as_ref(), &path.scope)
	}

	pub fn from_string(path: String, scope: &Scope) -> Unscoped {
		Self::from(CPath::new(path), scope)
	}
}

impl Display for Unscoped {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		Display::fmt(&self.0, f)
	}
}

impl AsRef<Path> for Unscoped {
	fn as_ref(&self) -> &Path {
		self.0.as_path()
	}
}

// a CPath with only normal components (not absolute, and no leading ../)
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct Simple(CPath);

impl Simple {
	pub fn project<'a>(&self, s: &'a str) -> Option<&'a str> {
		let prefix: &str = self.0.as_ref();
		s.strip_prefix(prefix).and_then(|s| s.strip_prefix("/"))
	}
}

impl AsRef<CPath> for Simple {
	fn as_ref(&self) -> &CPath {
		&self.0
	}
}

impl Deref for Simple {
	type Target = CPath;

	fn deref(&self) -> &Self::Target {
		&self.0
	}
}

impl Into<PathBuf> for Simple {
	fn into(self) -> PathBuf {
		self.0.into()
	}
}

impl Into<Unscoped> for Simple {
	fn into(self) -> Unscoped {
		Unscoped(self.0)
	}
}

impl Display for Simple {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		Display::fmt(&self.0, f)
	}
}

impl TryFrom<String> for Simple {
	type Error = Error;

	fn try_from(s: String) -> Result<Self> {
		let c = CPath::new(s);
		if c.kind() == Kind::Simple {
			Ok(Self(c))
		} else {
			Err(anyhow!("Not a simple path: {:?}", c))
		}
	}
}

// a CPath with leading `../` components
pub struct External(CPath);

#[derive(Debug)]
pub struct Absolute(CPath);
impl Absolute {
	pub fn join(&self, other: &CPath) -> Self {
		Absolute(self.0.join(other))
	}
}

impl Into<PathBuf> for Absolute {
	fn into(self) -> PathBuf {
		self.0.into()
	}
}

impl Deref for Absolute {
	type Target = CPath;

	fn deref(&self) -> &Self::Target {
		&self.0
	}
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Kind {
	Simple,
	External,
	Absolute,
}

pub struct ResolveModule<'a> {
	pub source_module: Option<&'a Unscoped>,
	pub explicit_path: Option<Scoped<&'a CPath>>,
}
impl<'a> ResolveModule<'a> {
	pub fn resolve(self) -> Result<Unscoped> {
		self.explicit_path
			.as_ref()
			.map(Unscoped::from_scoped)
			.or_else(|| self.source_module.map(|p| p.to_owned()))
			.ok_or_else(||anyhow!("Received a WasmCall without a populated module"))
	}
}

#[cfg(test)]
mod test {
	use super::*;
	
	fn p(s: &str) -> CPath {
		CPath::new(s.to_owned())
	}

	fn s(s: &str) -> Scope {
		Scope::new(p(s).into_simple().unwrap())
	}

	#[test]
	fn test_normalization() {
		assert_eq!(p("foo/bar/.././/baz/"), p("foo/baz"));
		assert_eq!(p("foo/bar/../baz").kind(), Kind::Simple);
		assert_eq!(p("/").as_str(), "/");
		assert_eq!(p(".").as_str(), ".");

		assert_eq!(p("/foo/../foo"), p("/foo"));
		assert_eq!(p("/foo").kind(), Kind::Absolute);

		assert_eq!(p("../z").as_str(), "../z");
		assert_eq!(p("../../z").as_str(), "../../z");
		assert_eq!(p("../z").kind(), Kind::External);
	}

	#[test]
	fn test_join() {
		let scope = Scope::new(p("x/y").into_simple().unwrap());
		let join = |s| Scoped::new(scope.clone(), p(s)).as_cpath();
		assert_eq!(join("foo/bar").0, p("x/y/foo/bar"));
		assert_eq!(join("../z").0, p("x/z"));
		assert_eq!(join("../../../z").0, p("../z"));
		assert_eq!(join(".").0, p("x/y"));
	}
}
