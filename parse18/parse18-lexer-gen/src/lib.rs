#![allow(clippy::missing_panics_doc, reason = "Only used at build time")]
#![allow(clippy::unwrap_used, reason = "Only used at build time")]

pub mod builder;
pub mod codegen;
pub mod fsm;
pub mod regex;

pub trait Lexer {}
