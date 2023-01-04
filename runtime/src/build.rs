use std::fs;

use ambl_common::build::{InvokeResponse, FileDependencyType};
use log::*;
use anyhow::*;

use crate::build_request::{BuildRequest, PostBuild};
use crate::module::BuildModule;
use crate::path_util::{Scoped, Scope};
use crate::persist::{BuildResult, BuildResultWithDeps, DepSet, Cached, PersistFile};
use crate::project::{ProjectMutex, ProjectMutexPair, Project, ActiveBuildToken};
use crate::sync::Mutexed;


#[derive(Clone, PartialEq, Eq, Debug)]
pub enum BuildReason {
	Dependency(ActiveBuildToken),
	Speculative, // speculative execution of a build target, not associated with an active build
	Import, // Importing a module to evaluate its rules. This doesn't cause
	// dependencies to be registered, but the get_rules() call may
	Explicit, // explicit user request, fail if not a target
}

impl BuildReason {
	pub fn parent(&self) -> Option<ActiveBuildToken> {
		match self {
			BuildReason::Dependency(p) => Some(*p),
			BuildReason::Speculative => None,
			BuildReason::Import => None,
			BuildReason::Explicit => None,
		}
	}
}

pub struct BuildCache;

impl BuildCache {
	pub fn build_simple<'a, M: BuildModule, Build>(
		project: ProjectMutex<'a, M>,
		key: &BuildRequest,
		build_fn: Build
	) -> Result<ProjectMutexPair<'a, M, BuildResponse>> where
		Build: FnOnce() -> Result<BuildResult>
	{
		// TODO name better?
		let get_ctx = |project| {
			let mut project: Mutexed<Project<M>> = project;
			let ret = project.unlocked_block(|_| {
				build_fn()
			});
			Ok((project, ret?))
		};

		let needs_rebuild = |project, ctx: &BuildResult, cached: &BuildResultWithDeps| {
			let project: Mutexed<Project<M>> = project;
			Ok((project, ctx != &cached.result))
		};

		let do_build = |project, ctx: BuildResult| {
			let project: Mutexed<Project<M>> = project;
			Ok((project, BuildResultWithDeps::simple(ctx)))
		};

		Self::build_full(project, key, get_ctx, needs_rebuild, do_build)
	}

	pub fn build_with_deps<'a, M: BuildModule, Ctx, ComputeCtx, Build>(
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
			// if cached value doesn't have deps, it may have been e.g. a File which has now become a Target
			if let Some(ref deps) = cached.deps {
				return Self::requires_build(project, cached.require_deps()?);
			}
			Ok((project, true))
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
				CacheAware::Fresh(cached.result)
			},

			Some(Cached::Cached(cached)) => {
				let (project_ret, ctx) = get_ctx(project)?;
				let (project_ret, needs_build) = needs_rebuild(project_ret, &ctx, &cached)?;
				project = project_ret;
				if !needs_build {
					debug!("Marking cached result for {:?} ({:?}) as fresh", key, &cached);
					project.build_cache.update(key.to_owned(), cached.to_owned())?;
					CacheAware::Fresh(cached.result)
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
				built.result
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
			
			// always build the dep (which will be immediate if it's cached and doesn't need rebuilding)
			let (project_ret, dep_latest) = Project::build(project, dep_key, &reason)?;
			project = project_ret;
			
			// if the result differs from what this target was based on, rebuild this target
			if dep_latest.result.has_changed_since(dep_cached) {
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
	Fresh(BuildResult),
	Stale(Ctx),
}

pub struct BuildResponse {
	// the result, to be persisted
	pub result: BuildResult,
	override_response: Option<InvokeResponse>,
}

impl BuildResponse {
	pub fn new(result: BuildResult) -> Self {
		Self { result, override_response: None }
	}

	pub fn full(result: BuildResult, response: InvokeResponse) -> Self {
		Self { result, override_response: Some(response) }
	}
	
	pub fn into_response<M: BuildModule>(self, project: &Project<M>, post_build: &PostBuild) -> Result<InvokeResponse> {
		use BuildResult::*;
		let Self { override_response, result } = self;
		if let Some(response) = override_response {
			Ok(response)
		} else {
			match result {
				File(file) => Self::response_of_file(file, project, post_build),
				Target(file) => Self::response_of_file(Some(file), project, post_build),
				Fileset(fileset) => Ok(InvokeResponse::StrVec(fileset)),
				Env(env) => Ok(InvokeResponse::Str(env)),
				Wasm(wasm) => Ok(InvokeResponse::Str(wasm)),
				AlwaysDirty => Ok(InvokeResponse::Unit),
				AlwaysClean => Ok(InvokeResponse::Unit),
			}
		}
	}

	fn response_of_file<M: BuildModule>(file: Option<PersistFile>, project: &Project<M>, post_build: &PostBuild) -> Result<InvokeResponse> {
		let post_build = match post_build {
			PostBuild::FileDependency(f) => f,
			_ => panic!("file response with non-file request"),
		};
		let path = &post_build.path;

		// Only allow a missing file if we are explicitly testing for existence
		if file.is_none() && post_build.ret != FileDependencyType::Existence {
			return Err(anyhow!("No such file or directory: {}", &path));
		}

		Ok(match post_build.ret {
			FileDependencyType::Unit => InvokeResponse::Unit,
			FileDependencyType::Existence => InvokeResponse::Bool(file.is_some()),
			FileDependencyType::Contents => {
				let file = file.ok_or_else(|| anyhow!("No file produced for target {}", &path))?;
				let contents = if let Some(ref target) = file.target {
					let full_path = project.dest_path(&Scoped::new(Scope::root(), target.to_owned()))?;
					fs::read_to_string(full_path.as_ref()).with_context(||
						format!("Can't read target {} (from {})", &path, &full_path)
					)
				} else {
					fs::read_to_string(&path.0).with_context(||
						format!("Can't read {}", &path)
					)
				};
				InvokeResponse::Str(contents?)
			},
		})
	}
}
