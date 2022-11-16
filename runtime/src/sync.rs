use std::{sync::{TryLockResult, RwLockReadGuard, RwLockWriteGuard, Arc, Mutex, MutexGuard}, ops::{Deref, DerefMut}};

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

/*
MutexRef and Mutexed form a pair of types. Once can be changed into the
other via .lock() and .unlock() transitions, but the ownership encourages
correct behaviour (unlocking / locking consumes the object itself).

MutexRef implements clone, so it's still possibly to have incorrect behaviour
(by cloning it and then attempting to reentrantly lock it). But typically
you'll just have one, and you'll only clone it when giving it to another
class which you shouldn't then call reentrantly.
*/

// mutexRef is the toplevel owner. It can generate many handles
pub struct MutexRef<T>(Arc<Mutex<T>>);

impl<T> MutexRef<T> {
	pub fn new(t: T) -> Self { Self(Arc::new(Mutex::new(t))) }

	pub fn handle(&self) -> MutexHandle<T> {
		MutexHandle(Arc::clone(&self.0))
	}
}

impl<T> Clone for MutexRef<T> {
	fn clone(&self) -> Self {
		Self(Arc::clone(&self.0))
	}
}

// a handle is single use. You can lock it (and then unlock it),
// but you can't make copies. Generally you thread this through
// a single logical thread of control.
pub struct MutexHandle<T>(Arc<Mutex<T>>);

impl<T> MutexHandle<T> {
	pub fn lock(&mut self, desc: &'static str) -> Result<Mutexed<'_, T>> {
		let arc = Arc::clone(&self.0);
		let guard: MutexGuard<T> = self.0.lock().map_err(|_| lock_failed(desc))?;
		Ok(Mutexed { arc, guard })
	}
}

// Mutexed is an actively locked resource. You can drop it, or unlock()
// to turn it back into a handle. This is passed by value if you want
// the callee to be able to release it, or by ref otherwise
pub struct Mutexed<'a, T> {
	arc: Arc<Mutex<T>>,
	guard: MutexGuard<'a, T>,
}

impl<'a, T> Mutexed<'a, T> {
	pub fn unlock(self) -> MutexHandle<T> {
		// drops self.guard
		MutexHandle(self.arc)
	}
	
	// pub fn add_ref(&mut self) -> MutexRef<T> {
	// 	MutexRef(Arc::clone(&self.arc))
	// }
}

impl<'a, T> Deref for Mutexed<'a, T> {
	type Target = MutexGuard<'a, T>;

	fn deref(&self) -> &Self::Target {
		&self.guard
	}
}


impl<'a, T> DerefMut for Mutexed<'a, T> {
	fn deref_mut(&mut self) -> &mut Self::Target {
		&mut self.guard
	}
}
