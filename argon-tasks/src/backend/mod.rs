pub mod metadata;

#[cfg(target_family = "wasm")]
pub mod wasm;

mod backend_js;
#[cfg(all(not(target_family = "wasm"), feature = "std"))]
pub mod native;

pub use backend_js::{BackendJS, CodegenJSOptions, PlatformMetadataJSOptions};
#[cfg(all(not(target_family = "wasm"), feature = "std"))]
pub use native::NativeBackendJS;
#[cfg(target_family = "wasm")]
pub use wasm::WasmBackendJS;
