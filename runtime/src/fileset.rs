use anyhow::*;
use log::*;
use trou_common::build::FileSelection;
use walkdir::WalkDir;
use crate::path_util::{string_of_pathbuf, str_of_os};
use crate::persist::{ResolvedFilesetDependency, FileSelectionGlob};

pub fn scan(spec: &ResolvedFilesetDependency) -> Result<Vec<String>> {
	let globs = spec.compile()
	.with_context(|| format!("Compiling fileset spec {:?}", spec))?;
	let mut result = Vec::new();
	let walker = WalkDir::new(spec.root.0.as_path()).follow_links(false).sort_by_file_name().into_iter();
	let filtered = walker.filter_entry(|dir_entry| {
		should_include(str_of_os(dir_entry.file_name()), &globs.dirs)
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
	if rules.is_empty() {
		return true
	}
	for rule in rules {
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
	if rules.iter().all(|r| r.is_include()) {
		// if it's _only_ includes, exclude by default
		false
	} else {
		true
	}
}

#[cfg(test)]
mod test {
	use crate::path_util::{CPath, Unscoped};

use super::*;
	use FileSelection::*;
	use trou_common::build::FilesetDependency;
use trou_common::rule::dsl::*;
	fn base() -> FilesetDependency {
		fileset("unused".to_owned())
	}
	
	fn globs(f: FilesetDependency) -> Vec<FileSelectionGlob> {
		let FilesetDependency { root, dirs, files } = f;
		let res = ResolvedFilesetDependency {
			root: Unscoped::new(root),
			dirs,
			files,
		};
		debug!("Created {:?}", &res);
		res.compile().unwrap().dirs
	}
	
	#[test]
	fn test_include_exclude() {
		assert_eq!(false, should_include(".git", &globs(base().exclude(".*"))));
		assert_eq!(true, should_include(".git", &globs(base().include(".*"))));
		assert_eq!(true, should_include("foo.rs", &globs(base().include("*.rs"))));
	}

	
	#[test]
	fn test_first_match_wins() {
		assert_eq!(true, should_include(".rs", &globs(base().include("*.rs").exclude(".*"))));
		assert_eq!(false, should_include(".rs", &globs(base().exclude(".*").include("*.rs"))));
	}

	#[test]
	fn test_no_match_behaviour() {
		// if there's no match, assume:
		// - include only: exclude by default
		// - exclude only: include by default
		// - mixed: include by default
		assert_eq!(true, should_include("unknown", &globs(base().exclude(".*").include("*.rs"))));
		assert_eq!(true, should_include("unknown", &globs(base().exclude(".*"))));
		assert_eq!(false, should_include("unknown", &globs(base().include("*.rs"))));
	}
}
