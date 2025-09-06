use parse18_lexer_gen::{builder::DFABuilder, regex::Regex, codegen};


fn main() {
    let mut b: DFABuilder<String> = DFABuilder::new();

    b.add(Regex::pattern(r##"[^\\"#]+"##), "StringTokenType.LiteralText");
    b.add(Regex::str(r"\b"), "StringTokenType.EscapedBackspace");
    b.add(Regex::str(r"\f"), "StringTokenType.EscapedFormFeed");
    b.add(Regex::str(r"\n"), "StringTokenType.EscapedNewLine");
    b.add(Regex::str(r"\r"), "StringTokenType.EscapedCarriageReturn");
    b.add(Regex::str(r"\t"), "StringTokenType.EscapedTab");
    b.add(Regex::str(r"\v"), "StringTokenType.EscapedVerticalTab");
    b.add(Regex::str(r"\\"), "StringTokenType.EscapedBackslash");
    b.add(Regex::str(r"'"), "StringTokenType.EscapedSingleQuote");
    b.add(Regex::str(r#"\""#), "StringTokenType.EscapedDoubleQuote");
    b.add(Regex::str(r"\#"), "StringTokenType.EscapedHash");
    b.add(Regex::str(r"\["), "StringTokenType.EscapedOpenBracket");
    b.add(Regex::str(r"\]"), "StringTokenType.EscapedCloseBracket");
    b.add(Regex::str(r"\{"), "StringTokenType.EscapedOpenCurly");
    b.add(Regex::str(r"\}"), "StringTokenType.EscapedCloseCurly");
    b.add(Regex::pattern(r"\\u\{[[:xdigit:]]+\}"), "StringTokenType.EscapedUnicodeCodePoint");
    b.add(Regex::str("#{"), "StringTokenType.InterpolationStart");
    b.add(Regex::str("\""), "StringTokenType.EndOfString");

    let dfa = b.into_dfa();

    let settings = codegen::scala::ScalaSettings {
        package: "dev.argon.parser".to_owned(),
        access_modifier: "private[parser] ".to_owned(),
        object_name: "StringLexer".to_owned(),
        token_type: "StringTokenType".to_owned(),
    };

    codegen::scala::emit_scala(&mut std::io::stdout(), dfa, &settings).unwrap();
}
