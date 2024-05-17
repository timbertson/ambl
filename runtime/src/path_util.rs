use anyhow::*;
use lazy_static;
use log::*;
use core::fmt;
use std::ffi::OsStr;
use std::iter;
use std::ops::DerefMut;
use std::str::FromStr;
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

Mounts are represented as a Simple path from the project root.
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
		debug!("create_dir_all({})", parent.display());
		fs::create_dir_all(parent)?;
	}
	Ok(())
}

fn rm_rf(p: &Path) -> Result<()> {
	debug!("rm_rf({})", p.display());
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
	let p = p.as_ref();
	Ok(fsopt(p, p.symlink_metadata())?)
}

pub fn fsopt<P: AsRef<Path>, T>(p: P, result: io::Result<T>) -> Result<Option<T>> {
	match result {
		Result::Ok(x) => Ok(Some(x)),
		Result::Err(e) if e.kind() == io::ErrorKind::NotFound => Ok(None),
		Result::Err(e) => Err(e).with_context(|| format!("Accessing {}", p.as_ref().display())),
	}
}

pub fn lexists<P: AsRef<Path>>(p: P) -> Result<bool> {
	Ok(lstat_opt(p)?.is_some())
}

lazy_static::lazy_static! {
	static ref ROOT_EMBED: Embed<'static> = Embed {
		mount: None,
		scope: None,
		mount_depth: 0
	};
}

/*
 * Joining rules:
 *
 * Mounts always inherit from their parents.
 * Soped are ephemerial - if you join on top of a scoped path, it won't
 * inherit the scope unless you explicitly join e.g. `@scope/foo`.
 */
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct Embed<'a> {
	pub mount: Option<Cow<'a, Simple>>,
	pub scope: Option<Cow<'a, Simple>>,
	mount_depth: usize
}

impl<'a> Embed<'a> {
	pub fn new(mount: Option<Cow<'a, Simple>>, scope: Option<Cow<'a, Simple>>) -> Self {
		let mut ret = Self { mount, scope, mount_depth: 0 };
		ret.set_mount_depth();
		ret
	}
	
	fn set_mount_depth(&mut self) {
		self.mount_depth = self.mount.as_ref().map(|mount| {
			let simple: &Simple = mount.borrow();
			simple.as_str().chars().filter(|c| *c == '/').count() + 1
		}).unwrap_or(0);
	}

	pub fn root() -> Embed<'static> {
		ROOT_EMBED.clone()
	}

	pub fn static_root() -> &'static Embed<'static> {
		&ROOT_EMBED
	}

	pub fn path_to_root(&self) -> String {
		let mut buf = ".".to_owned();
		for _ in 0..self.mount_depth {
			buf.push_str("/..")
		}
		buf
	}

	pub fn push_mount_to(&self, path: &mut PathBuf) {
		if let Some(ref content) = self.mount {
			path.push(content.as_path())
		}
	}

	// Always expensive, removes all lifetime limits
	pub fn clone(&self) -> Embed<'static> {
		Embed {
			mount: self.mount.as_ref().map(|cow| Cow::Owned(cow.clone().into_owned())),
			scope: self.scope.as_ref().map(|cow| Cow::Owned(cow.clone().into_owned())),
			mount_depth: self.mount_depth
		}
	}

	// always cheap, but can't outlive the pointer it came from
	pub fn copy(&'a self) -> Self {
		Self {
			mount: self.mount.as_ref().map(|r| Cow::Borrowed((*r).deref())),
			scope: self.scope.as_ref().map(|r| Cow::Borrowed((*r).deref())),
			mount_depth: self.mount_depth
		}
	}
	
	// adding a mount removes the scope
	pub fn push_mount(&mut self, mount: &Simple) {
		self.scope = None;
		match self.mount.as_mut() {
			Some(existing) => existing.to_mut().push_simple(mount),
			None => {
				self.mount = Some(Cow::Owned(mount.to_owned()));
			}
		}
		self.set_mount_depth();
	}

	// setting a scope replaces an existing one
	pub fn set_scope(&mut self, scope: Option<Simple>) {
		self.scope = scope.map(Cow::Owned)
	}
}

#[derive(Debug)]
pub struct Embedded<'a, T> {
	pub embed: Embed<'a>,
	pub value: T,
}

impl<'a, T> Embedded<'a, T> {
	pub fn new(embed: Embed<'a>, value: T) -> Self {
		Self { embed: embed.clone(), value }
	}

	pub fn root(value: T) -> Self {
		Self { embed: Embed::root(), value }
	}

	pub fn replace_value<R>(self, value: R) -> Embedded<'a, R> {
		Embedded { embed: self.embed, value }
	}
	
	pub fn with_value<R>(&'a self, value: R) -> Embedded<'a, R> {
		let Embedded { embed, value: _ } = self;
		Embedded::new(embed.clone(), value)
	}
	
	pub fn map<R, F: FnOnce(T) -> R>(self, f: F) -> Embedded<'a, R> {
		let Embedded { embed, value } = self;
		Embedded { embed, value: f(value) }
	}

	pub fn map_ref<R, F: FnOnce(&T) -> R>(&'a self, f: F) -> Embedded<'a, R> {
		let Embedded { embed, value } = self;
		Embedded { embed: embed.clone(), value: f(&value) }
	}

	pub fn as_ref(&'a self) -> Embedded<'a, &T> {
		Embedded { embed: self.embed.clone(), value: &self.value }
	}
}

impl<'a, T: Display> Display for Embedded<'a, T> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		if let Some(mount) = self.embed.mount.as_ref() {
			Display::fmt(&mount, f)?;
			write!(f, "/")?;
		}
		Display::fmt(&self.value, f)
	}
}

impl<'a> Embedded<'a, Simple> {
	// flatten an embedded simple into a single simple
	pub fn flatten(&self) -> Simple {
		match self.embed.mount {
			None => self.value.to_owned(),
			Some(ref mount) => Simple(mount.0.join(&self.value)),
		}
	}
}

impl <'a, C: Clone> Clone for Embedded<'a, C> {
	fn clone(&self) -> Self {
		Self { embed: self.embed.clone(), value: self.value.clone() }
	}
}

// a "canonical" PathBuf which is:
// - always a valid string
// - normalized, i.e. contains no trailing `/`, and no internal `../` (but may start with one or more ../ segments)
// - has virtual path segments resolved (i.e. a leading @embed or @root)
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum CPath {
	P(PathBuf),
	Cwd,
}

lazy_static::lazy_static! {
	static ref CWD_PATH: PathBuf = PathBuf::from(".");
}

#[cfg(debug_assertions)]
const PROFILE: &'static str = "debug";

#[cfg(not(debug_assertions))]
const PROFILE: &'static str = "release";

lazy_static::lazy_static!{
	static ref BUILTINS_ROOT: PathBuf = {
		PathBuf::from(match option_env!("PREFIX") {
			Some(prefix) => format!("{}/share/builtins", prefix),
			// TODO pick a better path for this
			None => format!("{}/../target/wasm32-unknown-unknown/{}/component/", env!("CARGO_MANIFEST_DIR"), PROFILE),
		})
	};
}

impl CPath {
	pub fn new(s: String, embed: &Embed) -> Self {
		Self::canonicalize(s, embed)
	}

	pub fn from_path_nonvirtual<P: AsRef<Path>>(p: P) -> Result<Self> {
		let s = p.as_ref().as_os_str().to_str().ok_or_else(|| {
			anyhow!("Path is not valid utf8")
		})?;
		Ok(Self::new_nonvirtual(s.to_owned()))
	}
	
	// To be used only when we know the path can't contain virtual
	// prefixes, e.g loaded from serialization or we're concatenating
	// two cpaths. Mistaken use of this function will silently drop @embed
	// and @mount prefixes
	pub fn new_nonvirtual(s: String) -> Self {
		Self::canonicalize(s, &Embed::root())
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
		// treat trailing slash as non-canon (unless it's "/")
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

	fn canonicalize(mut s: String, embed: &Embed) -> Self {
		// first thing, resolve virtualization
		if s.starts_with("@scope/") {
			let scope_ref: Option<&Simple> = embed.scope.as_ref().map(|x| x.borrow());
			let replacement = scope_ref.map(|x| x.as_str());
			s.replace_range(0..6, replacement.unwrap_or("."));
		} else if s.starts_with("@root/") {
			s.replace_range(0..5, &embed.path_to_root());
		} else if let Some(builtin) = s.strip_prefix("builtin:") {
			// TODO rename @builtin/?
			let mut owned_path = BUILTINS_ROOT.to_owned();
			owned_path.push(format!("ambl_builtin_{}.wasm", builtin));
			s = string_of_pathbuf(owned_path)
		}

		let orig = PathBuf::from(s);
		if Self::is_canon(&orig) {
			Self::P(orig)
		} else {
			let mut ret = PathBuf::new();
			if iter::once(Component::CurDir).eq(orig.components()) {
				return Self::Cwd
			}
			let mut depth = 0;
			for part in orig.components() {
				match part {
					Component::RootDir => {
						ret.push(Component::RootDir);
						depth = 0;
					},
					Component::CurDir => (),
					Component::ParentDir => {
						if depth > 0 {
							let popped = ret.pop();
							assert!(popped == true);
						} else {
							ret.push("..");
						}
						depth -= 1;
					},
					Component::Normal(s) => {
						ret.push(s);
						depth += 1;
					}
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
	
	pub fn push_simple(&mut self, other: &Simple) {
		// Simples can just be concatenated
		match self {
			Self::P(ref mut p) => p.push(other.as_path()),
			Self::Cwd => {
				*self = other.0.to_owned();
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
						let mut ret = p_self.to_owned();
						ret.push(other.as_path());
						ret
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

// Small wrapper for declaring that a path is relative to the project root, and does not need a embed
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Unembedded(pub CPath);

impl Unembedded {
	// merge a embed and a CPath into a single path
	pub fn from(path: CPath, embed: &Embed) -> Unembedded {
		Unembedded(match embed.mount {
			None => path,
			Some(ref mount) => mount.join(path.as_ref()),
		})
	}

	pub fn from_ref(path: &CPath, embed: &Embed) -> Unembedded {
		Unembedded(match embed.mount {
			None => path.to_owned(),
			Some(ref mount) => mount.join(path),
		})
	}

	pub fn from_string(path: String, embed: &Embed) -> Unembedded {
		Self::from(CPath::new(path, embed), embed)
	}

	pub fn from_embedded<P: AsRef<CPath>>(path: &Embedded<P>) -> Unembedded {
		Self::from_ref(&path.value.as_ref(), &path.embed)
	}
	
	pub fn as_path(&self) -> &Path {
		self.0.as_path()
	}
}

impl Display for Unembedded {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		Display::fmt(&self.0, f)
	}
}

impl AsRef<Path> for Unembedded {
	fn as_ref(&self) -> &Path {
		self.0.as_path()
	}
}

impl Into<PathBuf> for Unembedded {
	fn into(self) -> PathBuf {
		self.0.into()
	}
}

// a CPath with only normal components (not absolute, and no leading ../)
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct Simple(CPath);

impl Simple {
	pub fn projected<'a>(&self, s: &'a str) -> Option<&'a str> {
		let prefix: &str = self.0.as_ref();
		s.strip_prefix(prefix).and_then(|s| s.strip_prefix("/"))
	}
	
	pub fn try_from(s: String, embed: &Embed) -> Result<Self> {
		let c = CPath::new(s, embed);
		if c.kind() == Kind::Simple {
			Ok(Self(c))
		} else {
			Err(anyhow!("Not a simple path: {:?}", c))
		}
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

impl DerefMut for Simple {
	fn deref_mut(&mut self) -> &mut Self::Target {
		&mut self.0
	}
}

impl Into<PathBuf> for Simple {
	fn into(self) -> PathBuf {
		self.0.into()
	}
}

impl Into<Unembedded> for Simple {
	fn into(self) -> Unembedded {
		Unembedded(self.0)
	}
}

impl Display for Simple {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		Display::fmt(&self.0, f)
	}
}

// a CPath with leading `../` components
pub struct External(CPath);

#[derive(Debug, Clone)]
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

impl AsRef<Path> for Absolute {
	fn as_ref(&self) -> &Path {
		self.0.as_ref()
	}
}

impl Deref for Absolute {
	type Target = CPath;

	fn deref(&self) -> &Self::Target {
		&self.0
	}
}

// // Enum to distinguish buildable targets from plain files
// #[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
// pub enum FileReference {
// 	Target(Simple),
// 	File(Unembedded),
// }

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Kind {
	Simple,
	External,
	Absolute,
}

pub struct ResolveModule<'a> {
	pub source_module: Option<&'a Unembedded>,
	pub explicit_path: Option<Embedded<'a, &'a CPath>>,
}
impl<'a> ResolveModule<'a> {
	pub fn resolve(self) -> Result<Unembedded> {
		self.explicit_path
			.as_ref()
			.map(Unembedded::from_embedded)
			.or_else(|| self.source_module.map(|p| p.to_owned()))
			.ok_or_else(||anyhow!("Received a WasmCall with no module specified, and no implicit module"))
	}
}

#[cfg(test)]
mod test {
	use super::*;
	
	fn p(s: &str) -> CPath {
		CPath::new_nonvirtual(s.to_owned())
	}
	
	fn simple(s: &str) -> Simple {
		CPath::new_nonvirtual(s.to_owned()).into_simple().unwrap()
	}

	fn p_virtual(s: &str, mount: Option<&str>, scope: Option<&str>) -> CPath {
		let embed = Embed::new(
			mount.map(|s| Cow::Owned(simple(s))),
			scope.map(|s| Cow::Owned(simple(s))),
		);
		CPath::new(s.to_owned(), &embed)
	}

	fn mount(s: &str) -> Embed {
		Embed::new(Some(Cow::Owned(simple(s))), None)
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
		assert_eq!(p("./../../z").as_str(), "../../z");
		assert_eq!(p("../z").kind(), Kind::External);
		assert_eq!(p("builtin:x"), p(&format!("{}/ambl_builtin_x.wasm", BUILTINS_ROOT.display())));
	}

	#[test]
	fn test_virtualisation() {
		assert_eq!(mount("x/y").path_to_root(), "./../..");
		assert_eq!(Embed::new(None, None).path_to_root(), ".");

		assert_eq!(p_virtual("@scope/x", None, None), p("x"));
		assert_eq!(p_virtual("@scope/x", Some("mount"), Some("scope")), p("scope/x"));
		assert_eq!(p_virtual("@scope/../x", Some("mount"), Some("some/internal/scope")), p("some/internal/x"));

		assert_eq!(p_virtual("@root/x", None, None), p("x"));
		assert_eq!(p_virtual("@root/../x", None, None), p("../x"));
		assert_eq!(p_virtual("@root/../x", None, Some("scope")), p("../x"));
		
		// @root undoes the effect of a mount
		assert_eq!(p_virtual("@root/x", Some("subdir"), None), p("../x"));
		assert_eq!(p_virtual("@root/../x", Some("subdir"), Some("scope")), p("../../x"));
		assert_eq!(p_virtual("@root/x", Some("nested/subdir"), Some("scope")), p("../../x"));
		
		let mut embed_mut = Embed::root();
		embed_mut.push_mount(&p("mnt").into_simple().unwrap());
		assert_eq!(CPath::new("@root/x".to_owned(), &embed_mut), p("../x"));

	}

	#[test]
	fn test_join() {
		let embed = mount("x/y");
		let join = |s: &str| Unembedded::from_string(s.to_owned(), &embed);
		assert_eq!(join("foo/bar").0, p("x/y/foo/bar"));
		assert_eq!(join("../z").0, p("x/z"));
		assert_eq!(join("../../../z").0, p("../z"));
		assert_eq!(join(".").0, p("x/y"));
	}
}
