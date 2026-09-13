use std::env;
use std::fs;
use std::io;
use std::path::PathBuf;

#[path = "build_support/argon_parser.rs"]
mod argon_parser_gen;
#[path = "build_support/double_quote_string_lexer.rs"]
mod double_quote_string_lexer;
#[path = "build_support/token_lexer.rs"]
mod token_lexer;

fn main() -> io::Result<()> {
    println!("cargo::rerun-if-changed=build.rs");
    println!("cargo::rerun-if-changed=build_support/token_lexer.rs");
    println!("cargo::rerun-if-changed=build_support/double_quote_string_lexer.rs");
    println!("cargo::rerun-if-changed=build_support/argon_parser.rs");
    println!("cargo::rerun-if-changed=../parse18/parse18-lexer-gen/src");
    println!("cargo::rerun-if-changed=../parse18/parse18-ll-gen/src");
    println!("cargo::rerun-if-changed=../argon-testcases/testcases");

    let out_dir = PathBuf::from(env::var_os("OUT_DIR").expect("OUT_DIR is set by Cargo"));

    fs::write(
        out_dir.join("token_lexer.rs"),
        capture(token_lexer::emit_rust_lexer)?,
    )?;
    fs::write(
        out_dir.join("double_quote_string_lexer.rs"),
        capture(double_quote_string_lexer::emit_rust_lexer)?,
    )?;
    fs::write(
        out_dir.join("argon_parser.rs"),
        capture(argon_parser_gen::emit_rust_parser)?,
    )?;

    Ok(())
}

fn capture(emit: impl FnOnce(&mut Vec<u8>) -> io::Result<()>) -> io::Result<Vec<u8>> {
    let mut buffer = Vec::new();
    emit(&mut buffer)?;
    Ok(buffer)
}
