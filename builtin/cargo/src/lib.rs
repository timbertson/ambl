use anyhow::*;
use serde::{Deserialize, Serialize};
use ambl_api::*;


/*

Target structure:

workspace-meta
Cargo.lock

module/{crate}
sources/{crate}

Everything depends on workspace-meta.

All cargo commands run with --frozen and depend on the lockfile, _aside_ from the lockfile rule itself.

*/

// TODO how do we share this struct nicely without
// - making a second crate containing just the struct
// - exposing internal functions (like `cargo`) on any module that depends on us
// #[derive(Clone, Debug, Serialize, Deserialize)]
// #[serde(deny_unknown_fields)]
// pub struct CargoOpts {
// 	package: Option<String>,
// 	package_path: Option<String>,
// }

// impl Default for CargoOpts {
// 	fn default() -> Self {
// 		Self {
// 			package: Default::default(),
// 			package_path: Default::default(),
// 		}
// 	}
// }

// impl CargoOpts {
// 	fn init<C: AsRef<BaseCtx>>(c: C) -> Result<Self> {
// 		let c = c.as_ref();
// 		let opts: CargoOpts = c.parse_config()?;
// 		Ok(opts)
// 	}

// 	fn package<C: AsRef<BaseCtx>>(&self, c: C) -> Result<String> {
// 		let prefix = self.package_path.as_deref().unwrap_or(".");
// 		let cargo_contents = c.as_ref().read_file(format!("{}/Cargo.toml", prefix))?;
// 		let toml: CargoMinimal = toml::from_str(&cargo_contents)?;
// 		Ok(toml.package.name)
// 	}
// }

// #[derive(Serialize, Deserialize)]
// struct CargoPackage {
// 	name: String,
// }

// #[derive(Serialize, Deserialize)]
// struct CargoMinimal {
// 	package: CargoPackage,
// }

#[derive(Deserialize, Serialize)]
struct UserConfig {
	root: String,
}

impl Default for UserConfig {
	fn default() -> Self {
		Self { root: "..".to_owned() }
	}
}

#[derive(Deserialize, Serialize)]
struct ModuleConfig {
	name: String,
	dep_names: Vec<String>,
}
impl Default for ModuleConfig {
	fn default() -> Self {
		panic!("ModuleConfig must be supplied");
	}
}

#[derive(Deserialize)]
struct CargoMetaMinimal {
	packages: Vec<CargoMetaPackage>,
	// workspace_root: String,
}

#[derive(Deserialize)]
struct CargoMetaPackage {
	name: String,
	// manifest_path: String,
	dependencies: Vec<CargoMetaDependency>,
}

#[derive(Deserialize)]
struct CargoMetaDependency {
	name: String,
	
	#[serde(default)]
	path: Option<String>,
}

fn raw_cargo(c: &BaseCtx) -> Result<Command> {
	let exe = c.lookup(exe_lookup("cargo"))?.ok_or_else(|| anyhow!("cargo not on $PATH"))?;
	minimal_cargo_files(c)?;
	Ok(cmd(exe))
}

fn minimal_cargo_files(c: &BaseCtx) -> Result<()> {
	c.build("Cargo.toml")?;
	// We need src/{lib,main} for cargo to evaluate.
	// The contents don't actually matter, they could
	// even be blank
	if c.exists("src/lib.rs")? {
		c.build("src/lib.rs")?;
	} else {
		c.build("src/main.rs")?;
	}
	Ok(())
}

fn cargo(c: &BaseCtx) -> Result<Command> {
	c.build("Cargo.lock")?;
	let raw = raw_cargo(c)?;
	Ok(raw.arg("--locked"))
}

#[export]
fn build_workspace_meta(c: TargetCtx) -> Result<()> {
	let meta = c.run(raw_cargo(&c)?.args(vec!(
		"metadata", "--no-deps", "--format-version", "1"
	)).stdout(Stdout::String))?.into_string()?;
	debug(&format!("META: {}", &meta));
	c.write_dest(meta)?;
	Ok(())
}

fn workspace_meta<C: AsRef<BaseCtx>>(c: C) -> Result<CargoMetaMinimal> {
	let c = c.as_ref();
	// TODO mark with checksum?
	let contents = c.read_file("workspace-meta")?;
	Ok(serde_json::from_str(&contents).with_context(|| contents.clone())?)
}

#[export]
fn build_lockfile(c: TargetCtx) -> Result<()> {
	// TODO how do we make this fast? I don't want cargo to update the lockfile all the time,
	// but I also want it to update on demand. Maybe it needs a --force flag at the ambl level.
	let tmpdir = c.run(raw_cargo(&c)?.arg("generate-lockfile"))?.into_tempdir()?;
	tmpdir.copy_to_dest(&c, "Cargo.lock")?;
	Ok(())
}

#[export]
pub fn get_rules(_: BaseCtx) -> Result<Vec<Rule>> {
	Ok(vec!(
		target("workspace-meta", function("build_workspace_meta")),
		target("Cargo.lock", function("build_lockfile")),
		include(this_module().function("module_rules")),
	))
}

#[export]
pub fn module_rules(c: BaseCtx) -> Result<Vec<Rule>> {
	let meta = workspace_meta(&c)?;
	
	let mut result: Vec<Rule> = vec!();
	let module_names: Vec<String> = meta.packages.iter().map(|p| p.name.to_owned()).collect();
	result.push(target("build", function("build_all").config(&module_names)?));

	for p in meta.packages {
		let dep_names: Vec<String> = p.dependencies.iter()
			.filter(|d| d.path.is_some())
			.map(|d| d.name.to_owned()).collect();
		let conf = ModuleConfig {
			name: p.name.to_owned(),
			dep_names,
		};
		result.push(target(
				format!("sources/{}", &p.name),
				function("module_sources")
					.config(&conf)?));

		result.push(target(
				format!("module/{}", &p.name),
				function("module_build")
					.config(&conf)?));
	}
	Ok(result)
}

#[export]
pub fn build_all(c: TargetCtx) -> Result<()> {
	let meta = workspace_meta(&c)?;
	for pkg in meta.packages {
		c.build(format!("module/{}", pkg.name))?;
	}
	c.empty_dest()
}

#[export]
pub fn module_build(c: TargetCtx) -> Result<()> {
	let conf: ModuleConfig = c.parse_config()?;
	for name in conf.dep_names {
		// TODO parallel?
		c.build(format!("module/{}", &name))?;
	}
	c.build(format!("sources/{}", &conf.name))?;
	let tmp = c.run(cargo(&c)?
		// TODO run this with lock, but update the lock explicitly
		.args(vec!("build", "--target", "wasm32-unknown-unknown", "--package"))
		.arg(&conf.name)
		.impure_share_dir("target")
	)?.into_tempdir()?;

	// TODO full cargo substitution logic
	tmp.copy_to_dest(&c, format!("target/wasm32-unknown-unknown/debug/{}.wasm", conf.name.replace("-", "_")))?;
	Ok(())
}

#[export]
pub fn module_sources(c: TargetCtx) -> Result<()> {
	// TODO: depend on non-rs files?
	// TODO "." for scan includes the prefix, but then depending on the returned files includes it again!?
	for source_file in c.list_fileset(fileset("src").include_files("*.rs"))? {
		c.build(format!("../{}", source_file))?;
	}
	c.empty_dest()
}
