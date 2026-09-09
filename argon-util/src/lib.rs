#![no_std]

extern crate alloc;
#[cfg(feature = "std")]
extern crate std;
mod error;

#[cfg(feature = "std")]
#[path = "sync.rs"]
pub mod sync;

#[cfg(not(feature = "std"))]
#[path = "sync_no_std.rs"]
pub mod sync;

mod multi_slice;
mod unload_cell;

use alloc::sync::Arc;
use core::hash::Hash;
pub use error::*;
pub use multi_slice::*;
pub use unload_cell::{Unload, UnloadCell};

#[derive(Debug, Clone)]
pub struct UniqueIdentifier(Arc<()>);

impl UniqueIdentifier {
    pub fn new() -> Self {
        Self(Arc::new(()))
    }
}

impl PartialEq for UniqueIdentifier {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for UniqueIdentifier {}

impl Hash for UniqueIdentifier {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        (Arc::as_ptr(&self.0) as usize).hash(state);
    }
}

#[derive(Debug, Clone)]
pub struct Fuel {
    amount: u32,
}

impl Fuel {
    pub fn new(amount: u32) -> Self {
        Self { amount }
    }

    pub fn consume(&mut self) {
        if self.amount > 0 {
            self.amount -= 1;
        }
    }

    pub fn is_empty(&self) -> bool {
        self.amount == 0
    }
}
