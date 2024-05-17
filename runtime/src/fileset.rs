use std::iter;

use anyhow::*;
use env_logger::DEFAULT_FILTER_ENV;
use log::*;
use ambl_common::build::FileSelection;
use walkdir::WalkDir;
use crate::build_request::{FileSelectionGlob, ResolvedFilesetDependency};
use crate::path_util::{string_of_pathbuf, str_of_os};

lazy_static::lazy_static! {
	static ref IGNORE_DOTFILE: FileSelectionGlob = FileSelectionGlob::ExcludeGlob(glob::Pattern::new(".*").unwrap());
}

pub fn scan(spec: &ResolvedFilesetDependency) -> Result<Vec<String>> {
	let globs = spec.compile()
	.with_context(|| format!("Compiling fileset spec {:?}", spec))?;
	let mut result = Vec::new();
	let walker = WalkDir::new(spec.root.0.as_path()).follow_links(false).sort_by_file_name().into_iter();
	let filtered = walker.filter_entry(|dir_entry| {
		if dir_entry.path() == spec.root.0.as_path() {
			// don't filter the first entry
			return true
		}
		debug!("testing dir_entry: {:?}", dir_entry);
		let filename = str_of_os(dir_entry.file_name());
		should_include(filename, &globs.dirs)
	});
	for entry in filtered {
		let entry = entry.with_context(|| format!("fileset::scan({:?})", spec))?;
		let fname = entry.file_name().to_str()
			.ok_or_else(|| anyhow!("Invalid filename: {:?}", entry.file_name()))?;
		let stat = entry.metadata()?;
		if !stat.is_dir() {
			let name = str_of_os(entry.file_name());
			if should_include(name, &globs.files) {
				debug!("Including: {:?}", name);
				let p = entry.into_path();
				result.push(string_of_pathbuf(p));
			} else {
				debug!("Excluding: {:?}", name);
			}
		}
	}
	Ok(result)
}

pub fn should_include(filename: &str, rules: &Vec<FileSelectionGlob>) -> bool {
	let ignore_dotfile: &FileSelectionGlob = &IGNORE_DOTFILE;
	for rule in rules.iter().chain(iter::once(ignore_dotfile)) {
		debug!("testing {:?} against rule {:?}", filename, rule);
		match rule {
			FileSelectionGlob::IncludeGlob(g) => if g.matches(filename) {
				debug!("include");
				return true;
			},
			FileSelectionGlob::ExcludeGlob(g) => if g.matches(filename) {
				debug!("exclude");
				return false;
			},
		}
	}
	debug!("No rules matched {:?}", filename);
	if rules.is_empty() {
		return true
	}
	if rules.iter().all(|r| r.is_include()) {
		// if it's _only_ includes, exclude by default
		false
	} else {
		true
	}
}

#[cfg(test)]
mod test {
	use crate::path_util::{CPath, Unembedded};

use super::*;
	use FileSelection::*;
	use ambl_common::build::FilesetDependency;
use ambl_common::rule::dsl::*;
	fn base() -> FilesetDependency {
		fileset("unused".to_owned())
	}
	
	fn globs(f: FilesetDependency) -> Vec<FileSelectionGlob> {
		let FilesetDependency { root, dirs, files } = f;
		let res = ResolvedFilesetDependency {
			root: Unembedded(CPath::new_nonvirtual(root)),
			dirs,
			files,
		};
		debug!("Created {:?}", &res);
		res.compile().unwrap().files
	}
	
	#[test]
	fn test_include_exclude() {
		assert_eq!(false, should_include(".git", &globs(base().exclude_files(".*"))));
		assert_eq!(true, should_include(".git", &globs(base().include_files(".*"))));
		assert_eq!(true, should_include("foo.rs", &globs(base().include_files("*.rs"))));
	}

	
	#[test]
	fn test_first_match_wins() {
		assert_eq!(true, should_include(".rs", &globs(base().include_files("*.rs").exclude_files(".*"))));
		assert_eq!(false, should_include(".rs", &globs(base().exclude_files(".*").include_files("*.rs"))));
	}

	#[test]
	fn test_no_match_behaviour() {
		// if there's no match,
		// - exclude dotfiles
		// - include only: exclude by default
		// - exclude only: include by default
		// - mixed: include by default, unless
		assert_eq!(true, should_include("unknown", &globs(base().exclude_files(".*").include_files("*.rs"))));
		assert_eq!(true, should_include("unknown", &globs(base().exclude_files(".*"))));
		assert_eq!(false, should_include("unknown", &globs(base().include_files("*.rs"))));

		assert_eq!(false, should_include(".unknown", &globs(base().exclude_files("*.rs"))));
		assert_eq!(false, should_include(".unknown", &globs(base())));
	}
}
