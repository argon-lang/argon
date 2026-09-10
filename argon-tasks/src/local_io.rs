#[cfg(not(target_family = "wasm"))]
#[path = "local_io_native.rs"]
mod implementation;

#[cfg(all(target_family = "wasm", feature = "nodejs"))]
#[path = "local_io_node.rs"]
mod implementation;

#[cfg(any(not(target_family = "wasm"), feature = "nodejs"))]
pub use implementation::*;
