pub mod metadata;

#[cfg(target_family = "wasm")]
pub mod wasm;

mod backend_js;
mod backend_jvm;
#[cfg(all(not(target_family = "wasm"), feature = "std"))]
pub mod native;

pub use backend_js::{BackendJS, CodegenJSOptions, PlatformMetadataJSOptions};
pub use backend_jvm::{BackendJVM, JvmCodegenOptions, JvmPlatformMetadataOptions};
#[cfg(all(not(target_family = "wasm"), feature = "std"))]
pub use native::{NativeBackendJS, NativeBackendJVM};
#[cfg(target_family = "wasm")]
pub use wasm::{WasmBackendJS, WasmBackendJVM};
