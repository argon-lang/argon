#![no_std]

extern crate alloc;
#[cfg(feature = "std")]
extern crate std;

pub mod decoder;
pub mod encoder;
pub mod ids;
pub mod vm;
