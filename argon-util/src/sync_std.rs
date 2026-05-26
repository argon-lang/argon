pub use std::sync::{Mutex, MutexGuard, OnceLock, RwLock, RwLockReadGuard, RwLockWriteGuard};

pub fn mutex_lock<T>(lock: &Mutex<T>) -> MutexGuard<'_, T> {
    lock.lock().unwrap_or_else(|err| err.into_inner())
}

pub fn rwlock_read<T>(lock: &RwLock<T>) -> RwLockReadGuard<'_, T> {
    lock.read().unwrap_or_else(|err| err.into_inner())
}

pub fn rwlock_write<T>(lock: &RwLock<T>) -> RwLockWriteGuard<'_, T> {
    lock.write().unwrap_or_else(|err| err.into_inner())
}

pub mod parallel {
    pub use rayon::prelude::*;
}
