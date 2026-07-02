use crate::sync::mutex_lock;
use alloc::sync::Arc;
use alloc::vec::Vec;
use std::sync::Mutex;

pub struct UnloadCell<A> {
    state: Mutex<UnloadCellState<A>>,
}

enum UnloadCellState<A> {
    Uninitialized,
    Initialized(A),
    Unloaded,
}

impl<A> UnloadCell<A> {
    pub fn new() -> Self {
        Self {
            state: Mutex::new(UnloadCellState::Uninitialized),
        }
    }

    pub fn initialize(&self, f: impl FnOnce() -> A) -> A
    where
        A: Clone,
    {
        let mut state = mutex_lock(&self.state);

        match &*state {
            UnloadCellState::Uninitialized => {
                let result = f();
                *state = UnloadCellState::Initialized(result.clone());
                result
            }
            UnloadCellState::Initialized(result) => result.clone(),
            UnloadCellState::Unloaded => panic!("UnloadCell is unloaded"),
        }
    }

    fn replace_unloaded(&self) -> Option<A> {
        let mut state = mutex_lock(&self.state);
        match core::mem::replace(&mut *state, UnloadCellState::Unloaded) {
            UnloadCellState::Uninitialized | UnloadCellState::Unloaded => None,
            UnloadCellState::Initialized(value) => Some(value),
        }
    }
}

pub trait Unload {
    fn unload(&self);
}

impl<A: Unload> Unload for UnloadCell<A> {
    fn unload(&self) {
        if let Some(value) = self.replace_unloaded() {
            value.unload();
        }
    }
}

impl<A: Unload + ?Sized> Unload for Arc<A> {
    fn unload(&self) {
        self.as_ref().unload();
    }
}

impl<A: Unload> Unload for Option<A> {
    fn unload(&self) {
        if let Some(value) = self {
            value.unload();
        }
    }
}

impl<A: Unload> Unload for Vec<A> {
    fn unload(&self) {
        for value in self {
            value.unload();
        }
    }
}
