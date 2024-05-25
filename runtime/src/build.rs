use std::fs;
use std::path::PathBuf;

use ambl_common::build::InvokeResponse;
use log::*;
use anyhow::*;

use crate::build_request::BuildRequest;
use crate::module::BuildModule;
use crate::path_util::{Embed, Embedded, Simple, Unembedded};
use crate::persist::{BuildResult, BuildResultWithDeps, DepSet, Cached, PersistFile, BuildRecord};
use crate::project::{ProjectMutex, ProjectMutexPair, Project, ActiveBuildToken, Implicits, HasImplicits};
use crate::sync::{Mutexed, MutexRef};

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Forced(pub bool);

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum BuildReason {
	Dependency(ActiveBuildToken),
	Speculative, // speculative execution of a build target, not associated with an active build
	Import, // Importing a module to evaluate its rules. This doesn't cause
	// dependencies to be registered, but the get_rules() call may
	Explicit(Forced), // explicit user request, fail if not a target
}

impl BuildReason {
	pub fn parent(&self) -> Option<ActiveBuildToken> {
		match self {
			BuildReason::Dependency(p) => Some(*p),
			BuildReason::Speculative => None,
			BuildReason::Import => None,
			BuildReason::Explicit(_) => None,
		}
	}
}

#[derive(Clone, Copy)]
pub enum OutputMode { Single, Multiple }

impl OutputMode {
	pub fn from_outputs(outputs: &Vec<String>) -> Self {
		if outputs.is_empty() {
			OutputMode::Single
		} else {
			OutputMode::Multiple
		}
	}
}

pub struct TargetContext {
	pub dest_tmp_path: Option<Unembedded>,
	pub embed: Embed<'static>,
	pub implicits: Implicits,
	pub output_mode: Option<OutputMode>,
}

impl TargetContext {
	pub fn dest_tmp_path(&self) -> Result<&Unembedded> {
		self.dest_tmp_path.as_ref()
			.ok_or_else(|| anyhow!("dest_tmp_path not defined (not a target?)"))
	}
}

pub struct BuildCache;

impl BuildCache {
	pub fn build_trivial<'a, M: BuildModule, Build>(
		project: ProjectMutex<'a, M>,
		key: &BuildRequest,
		build_fn: Build
	) -> Result<ProjectMutexPair<'a, M, BuildResponse>> where
		Build: FnOnce() -> Result<BuildResult>
	{
		Self::build_simple(project, key, |project| {
			let mut project: Mutexed<Project<M>> = project;
			let ret = project.unlocked_block(|_| {
				build_fn()
			});
			Ok((project, ret?))
		})
	}

	pub fn build_simple<'a, M: BuildModule, Build>(
		project: ProjectMutex<'a, M>,
		key: &BuildRequest,
		build_fn: Build
	) -> Result<ProjectMutexPair<'a, M, BuildResponse>> where
		Build: FnOnce(Mutexed<Project<M>>) -> Result<ProjectMutexPair<M, BuildResult>>
	{
		// TODO name better?
		let get_ctx = |project| {
			build_fn(project)
		};

		let needs_rebuild = |project, ctx: &BuildResult, cached: &BuildResultWithDeps| {
			let project: Mutexed<Project<M>> = project;
			Ok((project, ctx != &cached.record.result))
		};

		let do_build = |project, ctx: BuildResult| {
			let project: Mutexed<Project<M>> = project;
			Ok((project, BuildResultWithDeps::simple(ctx)))
		};

		Self::build_full(project, key, get_ctx, needs_rebuild, do_build)
	}

	pub fn build_with_deps<'a, M: BuildModule, Ctx: HasImplicits, ComputeCtx, Build>(
		project: ProjectMutex<'a, M>,
		reason: &BuildReason,
		key: &BuildRequest,
		get_ctx: ComputeCtx,
		build_fn: Build
	) -> Result<ProjectMutexPair<'a, M, BuildResponse>> where
		ComputeCtx: FnOnce(ProjectMutex<'a, M>) -> Result<ProjectMutexPair<'a, M, Ctx>>,
		Build: FnOnce(ProjectMutex<'a, M>, &Ctx, ActiveBuildToken) -> Result<ProjectMutexPair<'a, M, BuildResultWithDeps>>
	{
		let build_fn = |project, ctx: Ctx| {
			let build_token = ActiveBuildToken::generate();
			debug!("created build token {:?} beneath {:?} for {:?}", build_token, reason.parent(), key);
			build_fn(project, &ctx, build_token)
		};
		let needs_rebuild = |project, ctx: &Ctx, cached: &BuildResultWithDeps| {
			match reason {
				BuildReason::Explicit(Forced(true)) => {
					debug!("Forcing rebuild of {:?}", key);
					return Ok((project, true))
				},
				_ => (),
			}
			if cached.record.implicits.as_ref() != ctx.opt_implicits() {
				debug!("cached {:?} needs rebuild because its implicits have changed", cached);
				Ok((project, true))
			} else {
				// if cached value doesn't have deps, it may have been e.g. a File which has now become a Target
				if let Some(ref deps) = cached.deps {
					Self::requires_build(project, deps)
				} else {
					debug!("cached {:?} needs rebuild because it has no stored deps", cached);
					Ok((project, true))
				}
			}
		};
		Self::build_full(project, key, get_ctx, needs_rebuild, build_fn)
	}

	fn build_full<'a, M: BuildModule, Ctx, ComputeCtx, NeedsRebuild, Build>(
		mut project: ProjectMutex<'a, M>,
		key: &BuildRequest,
		get_ctx: ComputeCtx,
		needs_rebuild: NeedsRebuild,
		build_fn: Build
	) -> Result<ProjectMutexPair<'a, M, BuildResponse>> where
		ComputeCtx: FnOnce(ProjectMutex<'a, M>) -> Result<ProjectMutexPair<'a, M, Ctx>>,
		NeedsRebuild: FnOnce(ProjectMutex<'a, M>, &Ctx, &BuildResultWithDeps) -> Result<ProjectMutexPair<'a, M, bool>>,
		Build: FnOnce(ProjectMutex<'a, M>, Ctx) -> Result<ProjectMutexPair<'a, M, BuildResultWithDeps>>
	{
		// to_owned releases project borrow
		let from_cache = match project.build_cache.lookup(key)?.map(|c| c.to_owned()) {
			Some(Cached::Fresh(cached)) => {
				// already checked in this invocation, short-circuit
				debug!("Short circuit, already built {:?} ({:?})", key, &cached);
				CacheAware::Fresh(cached.record)
			},

			Some(Cached::Cached(cached)) => {
				let (project_ret, ctx) = get_ctx(project)?;
				debug!("Checking if rebuild is needed for {:?}", &cached);
				let (project_ret, needs_build) = needs_rebuild(project_ret, &ctx, &cached)?;
				project = project_ret;
				if !needs_build {
					debug!("Marking cached result for {:?} ({:?}) as fresh", key, &cached);
					project.build_cache.update(key.to_owned(), cached.to_owned())?;
					CacheAware::Fresh(cached.record)
				} else {
					CacheAware::Stale(ctx)
				}
			},

			None => {
				let (project_ret, ctx) = get_ctx(project)?;
				project = project_ret;
				CacheAware::Stale(ctx)
			},
		};
		
		let result = match from_cache {
			CacheAware::Fresh(dep) => dep,
			CacheAware::Stale(ctx) => {
				let (project_ret, built) = build_fn(project, ctx)?;
				project = project_ret;
				project.build_cache.update(key.clone(), built.clone())?;
				built.record
			},
		};
		Ok((project, BuildResponse::new(result)))
	}

	pub fn requires_build<'a, M: BuildModule>(
		mut project: ProjectMutex<'a, M>,
		// TODO accept BuildResultWithDeps and a build type?
		cached: &DepSet,
	) -> Result<ProjectMutexPair<'a, M, bool>> {
		let mut needs_build = false;
		
		let reason = BuildReason::Speculative;

		for (dep_key, dep_cached) in cached.iter() {
			debug!("requires_build() recursing over dependency {:?}", dep_key);
			
			// TODO: if we returned the project in the error case from Project::build, this wouldn't be necessary
			let mut project_err_handle = unsafe { project.unsafe_clone() };
			
			// always build the dep (which will be immediate if it's cached and doesn't need rebuilding)
			// TODO handle failure
			let implicits = dep_cached.implicits.as_ref().unwrap_or(Implicits::none());
			let (project_ret, dep_latest) = match Project::build(project, implicits, dep_key, &reason) {
				Result::Ok(pair) => pair,
				Result::Err(e) => {
					warn!("Speculative build failed for {:?}; assuming dirty", dep_key);
					let project = project_err_handle.lock("speculative build failure")?;
					let project = unsafe { project.unsafe_cast_lifetime::<'a>() };
					// the build failed, so it's definitely not clean
					return Ok((project, true));
				},
			};
			project = project_ret;
			
			// if the result differs from what this target was based on, rebuild this target
			if !(dep_latest.result.is_equivalent_to(dep_cached)) {
				debug!("Dependency {:?} state ({:?}) has changed since ({:?}); triggering rebuild of parent",
					dep_key,
					&dep_latest.result,
					dep_cached,
				);
				needs_build = true;
				break;
			}
		}

		Ok((project, needs_build))
	}
}

enum CacheAware<Ctx> {
	Fresh(BuildRecord),
	Stale(Ctx),
}

pub struct BuildResponse {
	// the result, to be persisted
	pub result: BuildRecord,
	override_response: Option<InvokeResponse>,
}

impl BuildResponse {
	pub fn new(result: BuildRecord) -> Self {
		Self { result, override_response: None }
	}

	pub fn full(result: BuildRecord, response: InvokeResponse) -> Self {
		Self { result, override_response: Some(response) }
	}
	
	pub fn as_target(&self) -> Option<&Simple> {
		match self.result.result {
			BuildResult::File(ref f) => f.target.as_ref(),
			_ => None,
		}
	}
	
	pub fn into_response(self) -> Result<InvokeResponse> {
		use BuildResult::*;
		let Self { override_response, result } = self;
		if let Some(response) = override_response {
			Ok(response)
		} else {
			match result.result {
				File(file) => Ok(InvokeResponse::Unit),
				Bool(b) => Ok(InvokeResponse::Bool(b)),
				Fileset(fileset) => Ok(InvokeResponse::StrVec(fileset)),
				Env(env) => Ok(InvokeResponse::StrOpt(env)),
				EnvKeys(env) => Ok(InvokeResponse::StrVec(env)),
				Wasm(wasm) => Ok(InvokeResponse::Str(serde_json::to_string(&wasm)?)),
				AlwaysDirty => Ok(InvokeResponse::Unit),
				AlwaysClean => Ok(InvokeResponse::Unit),
			}
		}
	}
}
