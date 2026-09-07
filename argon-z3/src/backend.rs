//! Private boundary for the native Z3 backend.
//!
//! Keeping the FFI module behind this boundary lets a future wasm build
//! replace the implementation without changing the safe API modules.

pub(crate) use z3_sys as sys;
