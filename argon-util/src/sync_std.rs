pub use std::sync::{Mutex, MutexGuard, OnceLock, RwLock, RwLockReadGuard, RwLockWriteGuard};

pub mod parallel {
    pub use rayon::prelude::*;
}
