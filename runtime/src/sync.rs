use std::{sync::{TryLockResult, RwLockReadGuard, RwLockWriteGuard, Arc, Mutex, MutexGuard, RwLock}, ops::{Deref, DerefMut}};

use log::*;
use anyhow::*;

pub fn lock_failed(desc: &str) -> Error {
	anyhow!("Failed to acquire lock: {}", desc)
}

pub fn lock_poisioned() -> Error {
	anyhow!("Failed to acquire poisoned lock")
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
	pub fn lock<'a>(&'a mut self, desc: &'static str) -> Result<Mutexed<'a, T>> {
		let arc = Arc::clone(&self.0);
		let guard: MutexGuard<T> = self.0.lock().map_err(|_| lock_failed(desc))?;
		Ok(Mutexed { arc, guard: Some(guard) })
	}
	
	#[cfg(test)]
	pub unsafe fn unsafe_lock<'a>(&'a self, desc: &'static str) -> Result<Mutexed<'a, T>> {
		let arc = Arc::clone(&self.0);
		let guard: MutexGuard<T> = self.0.lock().map_err(|_| lock_failed(desc))?;
		Ok(Mutexed { arc, guard: Some(guard) })
	}
}

// Mutexed is an actively locked resource. You can drop it, or unlock()
// to turn it back into a handle. This is passed by value if you want
// the callee to be able to release it, or by ref otherwise
pub struct Mutexed<'a, T> {
	arc: Arc<Mutex<T>>,
	guard: Option<MutexGuard<'a, T>>, // only set to None during unlocked_block
}

impl<'a, T> Mutexed<'a, T> {
	pub fn unlock(self) -> MutexHandle<T> {
		// drops self.guard
		MutexHandle(self.arc)
	}

	// unlock this mutex for the duration of a block. This is needed because otherwise users run into
	// lifetime issues when they need to re-lock the mutex without altering the lifetime.
	pub fn unlocked_block<'b, R, F: FnOnce(&mut MutexHandle<T>) -> Result<R>>(&'b mut self, f: F) -> Result<R> {
		self.guard = None; // drops guard

		// Correctness: the following code either resets self.guard to Some, or it panics
		let mut unlocked = MutexHandle(Arc::clone(&self.arc));
		let result = f(&mut unlocked);
		drop(unlocked);
		let reacquired = self.arc.lock().expect("failed to reacquire lock in unlocked_block");

		// Correctness: the 'b lifetime we have may be shorter than 'a. HOWEVER, it's guaranteed that
		// the underlying lock _will_ live for a, since there exists a reference to the mutex with lifetime 'a.
		self.guard = Some(unsafe {
			std::mem::transmute::<MutexGuard<'b, T>, MutexGuard<'a, T>>(reacquired) as MutexGuard<'a, T>
		});
		result
	}
	
	// pub fn add_ref(&mut self) -> MutexRef<T> {
	// 	MutexRef(Arc::clone(&self.arc))
	// }
}

impl<'a, T> Deref for Mutexed<'a, T> {
	type Target = MutexGuard<'a, T>;

	fn deref(&self) -> &Self::Target {
		self.guard.as_ref().expect("guard is None")
	}
}

impl<'a, T> DerefMut for Mutexed<'a, T> {
	fn deref_mut(&mut self) -> &mut Self::Target {
		self.guard.as_mut().expect("guard is None")
	}
}

#[derive(Debug)]
pub struct RwRef<T>(Arc<RwLock<T>>);

impl<T> RwRef<T> {
	pub fn new(v: T) -> Self {
		Self(Arc::new(RwLock::new(v)))
	}

	pub fn handle(&self) -> RwHandle<T> {
		RwHandle(Arc::clone(&self.0))
	}
}

impl<T> Clone for RwRef<T> {
	fn clone(&self) -> Self {
		Self(Arc::clone(&self.0))
	}
}

pub struct RwHandle<T>(Arc<RwLock<T>>);
impl<T> RwHandle<T> {
	pub fn read(&mut self) -> Result<RwReadGuard<T>> {
		let guard = self.0.read().map_err(|_| lock_poisioned())?;
		Ok(RwReadGuard {
			arc: Arc::clone(&self.0),
			guard
		})
	}

	pub fn write(&mut self) -> Result<RwWriteGuard<T>> {
		let guard = self.0.write().map_err(|_| lock_poisioned())?;
		Ok(RwWriteGuard {
			arc: Arc::clone(&self.0),
			guard
		})
	}

	pub fn with_write<'a, R, F>
	(&'a mut self, f: F) -> Result<R>
		where for<'b> F: FnOnce(&'b mut T) -> Result<R>
	{
		f(self.write()?.deref_mut())
	}
}

pub struct RwReadGuard<'a, T> {
	arc: Arc<RwLock<T>>,
	guard: RwLockReadGuard<'a, T>,
}
impl<'a, T> RwReadGuard<'a, T> {
	pub fn unlock(self) -> RwHandle<T> {
		RwHandle(self.arc)
	}
}

impl<'a, T> Deref for RwReadGuard<'a, T> {
	type Target = T;

	fn deref(&self) -> &Self::Target {
		self.guard.deref()
	}
}

pub struct RwWriteGuard<'a, T> {
	arc: Arc<RwLock<T>>,
	guard: RwLockWriteGuard<'a, T>,
}

impl<'a, T> RwWriteGuard<'a, T> {
	pub fn unlock(self) -> RwHandle<T> {
		RwHandle(self.arc)
	}
}

impl<'a, T> Deref for RwWriteGuard<'a, T> {
	type Target = T;

	fn deref(&self) -> &Self::Target {
		self.guard.deref()
	}
}

impl<'a, T> DerefMut for RwWriteGuard<'a, T> {
	fn deref_mut(&mut self) -> &mut Self::Target {
		self.guard.deref_mut()
	}
}
