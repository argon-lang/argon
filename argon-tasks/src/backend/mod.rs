pub mod metadata;

#[cfg(target_family = "wasm")]
pub mod wasm;

mod backend_js;
#[cfg(not(target_family = "wasm"))]
pub mod native;

pub use backend_js::{BackendJS, CodegenJSOptions, PlatformMetadataJSOptions};
#[cfg(not(target_family = "wasm"))]
pub use native::NativeBackendJS;
#[cfg(target_family = "wasm")]
pub use wasm::WasmBackendJS;
