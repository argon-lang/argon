#![allow(dead_code)]

use parse18_lexer_gen::{
    builder::DFABuilder,
    codegen::rust::{emit_rust, RustSettings},
    regex::Regex,
};
use std::io::{self, Write};

fn build_dfa() -> parse18_lexer_gen::fsm::DFA<String> {
    let mut b: DFABuilder<String> = DFABuilder::new();

    b.add(
        Regex::pattern(r##"[^\\"#]+"##),
        "StringTokenType::LiteralText",
    );
    b.add(Regex::str(r"\b"), "StringTokenType::EscapedBackspace");
    b.add(Regex::str(r"\f"), "StringTokenType::EscapedFormFeed");
    b.add(Regex::str(r"\n"), "StringTokenType::EscapedNewLine");
    b.add(Regex::str(r"\r"), "StringTokenType::EscapedCarriageReturn");
    b.add(Regex::str(r"\t"), "StringTokenType::EscapedTab");
    b.add(Regex::str(r"\v"), "StringTokenType::EscapedVerticalTab");
    b.add(Regex::str(r"\\"), "StringTokenType::EscapedBackslash");
    b.add(Regex::str(r"'"), "StringTokenType::EscapedSingleQuote");
    b.add(Regex::str(r#"\""#), "StringTokenType::EscapedDoubleQuote");
    b.add(Regex::str(r"\#"), "StringTokenType::EscapedHash");
    b.add(Regex::str(r"\["), "StringTokenType::EscapedOpenBracket");
    b.add(Regex::str(r"\]"), "StringTokenType::EscapedCloseBracket");
    b.add(Regex::str(r"\{"), "StringTokenType::EscapedOpenCurly");
    b.add(Regex::str(r"\}"), "StringTokenType::EscapedCloseCurly");
    b.add(
        Regex::pattern(r"\\u\{[[:xdigit:]]+\}"),
        "StringTokenType::EscapedUnicodeCodePoint",
    );
    b.add(Regex::str("#{"), "StringTokenType::InterpolationStart");
    b.add(Regex::str("\""), "StringTokenType::EndOfString");

    b.into_dfa()
}

pub fn emit_rust_lexer<W: Write>(w: &mut W) -> io::Result<()> {
    let dfa = build_dfa();

    let settings = RustSettings {
        module: None,
        visibility: "pub ".to_owned(),
        type_name: "StringLexer".to_owned(),
        token_type: "crate::token::StringTokenType".to_owned(),
        acceptance_expr: true,
        header_stmts: vec!["use crate::token::StringTokenType;".to_owned()],
    };

    emit_rust(w, dfa, &settings)
}
