mod error;
mod vec_deque_slice;

pub use error::*;
use std::hash::Hash;
use std::sync::Arc;
pub use vec_deque_slice::*;

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
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Arc::as_ptr(&self.0).hash(state);
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
