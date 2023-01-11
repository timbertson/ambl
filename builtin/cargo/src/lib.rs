use anyhow::*;
use serde::{Deserialize, Serialize};
use ambl_api::*;

macro_rules! ffi {
	($orig:ident) => {
		// place it in a module to avoid namespace clash, but export it to C unmangled.
		// I'd rather export as e.g. ${orig}_ffi but I don't think that's possible
		pub mod $orig {
			// use ambl_api::*;

			#[no_mangle]
			pub extern "C" fn $orig<'a>(ptr_in: *const u8, len_in: u32, ptr_out: &'a mut *mut u8, len_out: &'a mut u32) {
				super::wrap_fn_mut1(super::$orig, ptr_in, len_in, ptr_out, len_out)
			}
		}
	}
}

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

fn cargo(c: &BaseCtx) -> Result<Command> {
	let exe = c.lookup(exe_lookup("cargo"))?.ok_or_else(|| anyhow!("cargo not on $PATH"))?;
	c.build("Cargo.toml")?;
	Ok(cmd(exe))
}

ffi!(build_workspace_meta);
fn build_workspace_meta(c: TargetCtx) -> Result<()> {
	// We need src/{lib,main} for cargo to evaluate.
	// The contents don't actually matter, they could
	// even be blank
	if c.exists("src/lib.rs")? {
		c.build("src/lib.rs")?;
	} else {
		c.build("src/main.rs")?;
	}
	let meta = c.run(cargo(&c)?.args(vec!(
		"metadata", "--no-deps", "--format-version", "1"
	)).stdout(Stdout::String))?.into_string()?;
	c.write_dest(meta)?;
	Ok(())
}

fn workspace_meta<C: AsRef<BaseCtx>>(c: C) -> Result<CargoMetaMinimal> {
	let c = c.as_ref();
	// TODO mark with checksum?
	let contents = c.read_file("workspace-meta")?;
	Ok(serde_json::from_str(&contents).with_context(|| contents.clone())?)
}

// ffi!(build_lockfile);
// fn build_lockfile(c: TargetCtx) -> Result<()> {
// 	c.run(cargo(&c)?.args(vec!(
// 		"--offline", "--update"
// 	))).or_else(|_|
// 		c.run(cargo(&c)?.arg("generate-lockfile"))
// 	)?;
	
// 	// TODO if there were a full sandbox, how would we reference the _current_ lockfile on disk?
// 	// Perhaps there's a persistent_build_ctx or something which copies files if they exist?
// 	// Maybe it doesn't matter that we need the un-target version, we just want to copy <previous-version-of-my-target> if present...
// 	c.copy_to_dest(c.target())?;
// 	Ok(())
// }

ffi!(get_rules);
pub fn get_rules(_: BaseCtx) -> Result<Vec<Rule>> {
	Ok(vec!(
		target("workspace-meta", function("build_workspace_meta")),
		target("Cargo.lock", function("build_lockfile")),
		include(this_module().function("module_rules")),
	))
}

ffi!(module_rules);
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

ffi!(build_all);
pub fn build_all(c: BaseCtx) -> Result<()> {
	let meta = workspace_meta(&c)?;
	for pkg in meta.packages {
		c.build(format!("module/{}", pkg.name))?;
	}
	Ok(())
}

ffi!(module_build);
pub fn module_build(c: TargetCtx) -> Result<()> {
	let conf: ModuleConfig = c.parse_config()?;
	for name in conf.dep_names {
		// TODO parallel?
		c.build(format!("build/{}", &name))?;
	}
	c.build(format!("sources/{}", &conf.name))?;
	let tmp = c.run(cargo(&c)?
		// TODO run this with lock, but update the lock explicitly
		.args(vec!("build", "--target", "wasm32-unknown-unknown", "--package"))
		.arg(&conf.name)
		.impure_share_dir("target")
	)?.into_string()?;

	// TODO full cargo substitution logic
	c.copy_to_dest(format!("{}/target/wasm32-unknown-unknown/debug/{}.wasm", tmp, conf.name.replace("-", "_")))?;
	Ok(())
}

ffi!(module_sources);
pub fn module_sources(c: BaseCtx) -> Result<()> {
	// TODO: depend on non-rs files?
	// TODO "." for scan includes the prefix, but then depending on the returned files includes it again!?
	for source_file in c.list_fileset(fileset("src").include_files("*.rs"))? {
		debug(&format!("BUILDY: {}", source_file));
		c.build(format!("../{}", source_file))?;
	}
	Ok(())
}
