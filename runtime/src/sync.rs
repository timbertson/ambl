use std::sync::{TryLockResult, RwLockReadGuard, RwLockWriteGuard};

use log::*;
use anyhow::*;

pub fn lock_failed(desc: &str) -> Error {
	anyhow!("Failed to acquire lock: {}", desc)
}

// rwlock*ref provides a convenient wrapper to pretend StateRef contains a State instead of Option<State>
pub struct RwLockReadRef<'a, T>(pub TryLockResult<RwLockReadGuard<'a, Option<T>>>);
impl<'a, T> RwLockReadRef<'a, T> {
	pub fn as_ref(&self) -> Result<&T> {
		self.0.as_ref()
			.map_err(|e| anyhow!("Can't acquire lock: {:?}", e))?
			.as_ref().ok_or_else(||anyhow!("Uninitialized store"))
	}
}

#[cfg(debug_assertions)]
impl<'a, T> Drop for RwLockReadRef<'a, T> {
	fn drop(&mut self) {
		debug!("readLock: drop");
	}
}

pub struct RwLockWriteRef<'a, T>(pub TryLockResult<RwLockWriteGuard<'a, Option<T>>>);
impl<'a, T> RwLockWriteRef<'a, T> {
	pub fn as_ref(&mut self) -> Result<&mut T> {
		self.0.as_mut()
			.map_err(|e| anyhow!("Can't acquire lock: {:?}", e))?
			.as_mut().ok_or_else(||anyhow!("Uninitialized store"))
	}
}

#[cfg(debug_assertions)]
impl<'a, T> Drop for RwLockWriteRef<'a, T> {
	fn drop(&mut self) {
		debug!("writeLock: drop");
	}
}
