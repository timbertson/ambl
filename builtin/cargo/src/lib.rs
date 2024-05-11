#[ambl_api::export]
pub mod build {
	use anyhow::*;
	use serde::{Deserialize, Serialize};
	use ambl_api::*;
	use log::*;

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
		minimal_cargo_files(c)?;
		c.cmd_from_path("cargo")
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
		Ok(raw.arg("--offline"))
	}

	pub fn build_workspace_meta(c: TargetCtx) -> Result<()> {
		let meta = c.run_output(raw_cargo(&c)?.args(vec!(
			"metadata", "--no-deps", "--format-version", "1"
		))).context("cargo metadata output")?;
		// debug(&format!("META: {}", &meta));
		warn!("META: {}", &meta);
		c.write_dest(meta)?;
		Ok(())
	}

	fn workspace_meta<C: AsRef<BaseCtx>>(c: C) -> Result<CargoMetaMinimal> {
		let c = c.as_ref();
		let contents = c.read_file("workspace-meta")?;
		Ok(serde_json::from_str(&contents).with_context(|| contents.clone())?)
	}

	pub fn build_lockfile(c: TargetCtx) -> Result<()> {
		// TODO how do we make this fast? I don't want cargo to update the lockfile all the time,
		// but I also want it to update on demand. Maybe it needs a --force flag at the ambl level.
		let tmpdir = c.run(raw_cargo(&c)?.arg("fetch"))?.into_tempdir()?;
		tmpdir.copy_to_dest(&c, "Cargo.lock")?;
		Ok(())
	}

	pub fn get_rules(_: BaseCtx) -> Result<Vec<Rule>> {
		Ok(vec!(
			target("workspace-meta", target_fn!(build_workspace_meta)),
			target("Cargo.lock", target_fn!(build_lockfile)),
			include(rule_fn!(module_rules)),
		))
	}

	pub fn module_rules(c: BaseCtx) -> Result<Vec<Rule>> {
		let meta = workspace_meta(&c)?;
		
		let mut result: Vec<Rule> = vec!();
		let module_names: Vec<String> = meta.packages.iter().map(|p| p.name.to_owned()).collect();
		result.push(target("build", target_fn!(build_all).config(&module_names)?));

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
					target_fn!(module_sources)
						.config(&conf)?));

			result.push(target(
					format!("module/{}", &p.name),
					target_fn!(module_build)
						.config(&conf)?));
		}
		Ok(result)
	}

	pub fn build_all(c: TargetCtx) -> Result<()> {
		let meta = workspace_meta(&c)?;
		for pkg in meta.packages {
			c.build(format!("module/{}", pkg.name))?;
		}
		c.empty_dest()
	}

	pub fn module_build(c: TargetCtx) -> Result<()> {
		let conf: ModuleConfig = c.parse_config()?;
		for name in conf.dep_names {
			// TODO parallel?
			c.build(format!("module/{}", &name))?;
		}
		c.build(format!("sources/{}", &conf.name))?;

		let tmp = c.run(cargo(&c)?
			.args(vec!("build", "--target", "wasm32-unknown-unknown", "--package"))
			.arg(&conf.name)
			.impure_share_dir("target")
		)?.into_tempdir()?;

		// TODO full cargo substitution logic
		let raw_wasm_name = format!("target/wasm32-unknown-unknown/debug/{}.wasm", conf.name.replace("-", "_"));
		
		let wasm_bytes = tmp.read_file_bytes(&c, raw_wasm_name)?;
		let component_bytes = convert_wasm_to_component(&wasm_bytes)?;
		c.write_dest(component_bytes)?;

		Ok(())
	}
	
	fn convert_wasm_to_component(contents: &[u8]) -> Result<Vec<u8>> {
		use wit_component::ComponentEncoder;

		let wasm = wat::parse_bytes(contents)?;
		let encoder = ComponentEncoder::default()
				.validate(true)
				.module(&wasm)?;

		let bytes = encoder
				.encode()
				.context("failed to encode a component from module")?;

		Ok(bytes)
	}

	pub fn module_sources(c: TargetCtx) -> Result<()> {
		// TODO: depend on non-rs files?
		// TODO "." for scan includes the prefix, but then depending on the returned files includes it again!?
		for source_file in c.list_fileset(fileset("src").include_files("*.rs"))? {
			c.build(format!("../{}", source_file))?;
		}
		c.empty_dest()
	}
}
