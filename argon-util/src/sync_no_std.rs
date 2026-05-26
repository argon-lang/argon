use core::cell::{BorrowError, BorrowMutError, OnceCell, Ref, RefCell, RefMut};
use core::fmt;

pub type MutexGuard<'a, T> = RefMut<'a, T>;
pub type RwLockReadGuard<'a, T> = Ref<'a, T>;
pub type RwLockWriteGuard<'a, T> = RefMut<'a, T>;

pub struct Mutex<T>(RefCell<T>);

impl<T> Mutex<T> {
    pub const fn new(value: T) -> Self {
        Self(RefCell::new(value))
    }

    pub fn lock(&self) -> Result<MutexGuard<'_, T>, BorrowMutError> {
        self.0.try_borrow_mut()
    }

    pub fn get_mut(&mut self) -> &mut T {
        self.0.get_mut()
    }

    pub fn into_inner(self) -> T {
        self.0.into_inner()
    }
}

impl<T: Default> Default for Mutex<T> {
    fn default() -> Self {
        Self::new(T::default())
    }
}

impl<T: fmt::Debug> fmt::Debug for Mutex<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

pub struct RwLock<T>(RefCell<T>);

impl<T> RwLock<T> {
    pub const fn new(value: T) -> Self {
        Self(RefCell::new(value))
    }

    pub fn read(&self) -> Result<RwLockReadGuard<'_, T>, BorrowError> {
        self.0.try_borrow()
    }

    pub fn write(&self) -> Result<RwLockWriteGuard<'_, T>, BorrowMutError> {
        self.0.try_borrow_mut()
    }

    pub fn get_mut(&mut self) -> &mut T {
        self.0.get_mut()
    }

    pub fn into_inner(self) -> T {
        self.0.into_inner()
    }
}

impl<T: Default> Default for RwLock<T> {
    fn default() -> Self {
        Self::new(T::default())
    }
}

impl<T: fmt::Debug> fmt::Debug for RwLock<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

pub type OnceLock<T> = OnceCell<T>;
