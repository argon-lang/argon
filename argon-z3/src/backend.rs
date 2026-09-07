//! Private boundary for the Z3 backend.
//!
//! The unknown-wasm backend talks to the separately-loaded Z3 module, while
//! all other targets use the native `z3-sys` bindings.

#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
pub(crate) mod wasm;

#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
pub(crate) use wasm as sys;

#[cfg(not(all(target_arch = "wasm32", target_os = "unknown")))]
pub(crate) use z3_sys as sys;
