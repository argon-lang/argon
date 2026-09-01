use parse18_lexer_gen::{
    builder::DFABuilder,
    codegen::rust::{RustSettings, emit_rust},
    regex::Regex,
};
use std::io::{self, Write};

fn build_dfa() -> parse18_lexer_gen::fsm::DFA<String> {
    let mut b: DFABuilder<String> = DFABuilder::new();

    b.add(Regex::pattern(r"//[^\n]*"), "TokenType::Whitespace");
    b.add(Regex::pattern(r"[^\S\n]+"), "TokenType::Whitespace");
    b.add(Regex::pattern(r"\\\n"), "TokenType::Whitespace");
    b.add(Regex::pattern(r"\n"), "TokenType::Token(Token::NewLine)");
    b.add(Regex::str("\""), "TokenType::Token(Token::StringStart)");

    b.add(
        Regex::pattern(r"[0-9]+(_[0-9]+)*[\p{L}_][\p{L}\p{N}\p{Mn}\p{Mc}_]*"),
        "TokenType::InvalidInteger",
    );
    b.add(Regex::pattern(r"0[xX][[:xdigit:]]+(_[[:xdigit:]]+)*[\p{L}_&&[^[[:xdigit:]]]][\p{L}\p{N}\p{Mn}\p{Mc}_]*"), "TokenType::InvalidInteger");
    b.add(
        Regex::pattern(r"0[bB][01]+(_[01]+)*[\p{L}_][\p{L}\p{N}\p{Mn}\p{Mc}_]*"),
        "TokenType::InvalidInteger",
    );
    b.add(
        Regex::pattern(r"0o[0-7]+(_[0-7]+)*[\p{L}_][\p{L}\p{N}\p{Mn}\p{Mc}_]*"),
        "TokenType::InvalidInteger",
    );

    b.add(
        Regex::pattern(r"[0-9]+(_[0-9]+)*([iu][0-9]+)?"),
        "TokenType::DecInteger",
    );
    b.add(
        Regex::pattern(r"0[xX][[:xdigit:]]+(_[[:xdigit:]]+)*([iu][0-9]+)?"),
        "TokenType::HexInteger",
    );
    b.add(
        Regex::pattern(r"0[bB][01]+(_[01]+)*([iu][0-9]+)?"),
        "TokenType::BinInteger",
    );
    b.add(
        Regex::pattern(r"0o[0-7]+(_[0-7]+)*([iu][0-9]+)?"),
        "TokenType::OctInteger",
    );

    b.add(
        Regex::pattern(r"[\p{L}_][\p{L}\p{N}\p{Mn}\p{Mc}_]*[!?]?"),
        "TokenType::Identifier",
    );

    b.add(
        Regex::str("__argon_builtin"),
        "TokenType::Token(Token::KwArgonBuiltin)",
    );
    b.add(Regex::str("def"), "TokenType::Token(Token::KwDef)");
    b.add(Regex::str("proc"), "TokenType::Token(Token::KwProc)");
    b.add(Regex::str("do"), "TokenType::Token(Token::KwDo)");
    b.add(Regex::str("end"), "TokenType::Token(Token::KwEnd)");
    b.add(Regex::str("let"), "TokenType::Token(Token::KwLet)");
    b.add(Regex::str("val"), "TokenType::Token(Token::KwVal)");
    b.add(Regex::str("mut"), "TokenType::Token(Token::KwMut)");
    b.add(Regex::str("use"), "TokenType::Token(Token::KwUse)");
    b.add(Regex::str("shared"), "TokenType::Token(Token::KwShared)");
    b.add(Regex::str("module"), "TokenType::Token(Token::KwModule)");
    b.add(Regex::str("record"), "TokenType::Token(Token::KwRecord)");
    b.add(Regex::str("enum"), "TokenType::Token(Token::KwEnum)");
    b.add(Regex::str("trait"), "TokenType::Token(Token::KwTrait)");
    b.add(
        Regex::str("instance"),
        "TokenType::Token(Token::KwInstance)",
    );
    b.add(Regex::str("new"), "TokenType::Token(Token::KwNew)");
    b.add(Regex::str("with"), "TokenType::Token(Token::KwWith)");
    b.add(Regex::str("true"), "TokenType::Token(Token::KwTrue)");
    b.add(Regex::str("false"), "TokenType::Token(Token::KwFalse)");
    b.add(Regex::str("as"), "TokenType::Token(Token::KwAs)");
    b.add(Regex::str("is"), "TokenType::Token(Token::KwIs)");
    b.add(Regex::str("import"), "TokenType::Token(Token::KwImport)");
    b.add(Regex::str("export"), "TokenType::Token(Token::KwExport)");
    b.add(Regex::str("public"), "TokenType::Token(Token::KwPublic)");
    b.add(
        Regex::str("protected"),
        "TokenType::Token(Token::KwProtected)",
    );
    b.add(Regex::str("private"), "TokenType::Token(Token::KwPrivate)");
    b.add(
        Regex::str("internal"),
        "TokenType::Token(Token::KwInternal)",
    );
    b.add(Regex::str("final"), "TokenType::Token(Token::KwFinal)");
    b.add(
        Regex::str("override"),
        "TokenType::Token(Token::KwOverride)",
    );
    b.add(Regex::str("virtual"), "TokenType::Token(Token::KwVirtual)");
    b.add(
        Regex::str("abstract"),
        "TokenType::Token(Token::KwAbstract)",
    );
    b.add(Regex::str("if"), "TokenType::Token(Token::KwIf)");
    b.add(Regex::str("then"), "TokenType::Token(Token::KwThen)");
    b.add(Regex::str("else"), "TokenType::Token(Token::KwElse)");
    b.add(Regex::str("elsif"), "TokenType::Token(Token::KwElsif)");
    b.add(Regex::str("match"), "TokenType::Token(Token::KwMatch)");
    b.add(Regex::str("case"), "TokenType::Token(Token::KwCase)");
    b.add(Regex::str("loop"), "TokenType::Token(Token::KwLoop)");
    b.add(Regex::str("while"), "TokenType::Token(Token::KwWhile)");
    b.add(Regex::str("break"), "TokenType::Token(Token::KwBreak)");
    b.add(Regex::str("return"), "TokenType::Token(Token::KwReturn)");
    b.add(Regex::str("next"), "TokenType::Token(Token::KwNext)");
    b.add(Regex::str("redo"), "TokenType::Token(Token::KwRedo)");
    b.add(Regex::str("retry"), "TokenType::Token(Token::KwRetry)");
    b.add(Regex::str("type"), "TokenType::Token(Token::KwType)");
    b.add(Regex::str("type!"), "TokenType::Token(Token::KwBigType)");
    b.add(Regex::str("extern"), "TokenType::Token(Token::KwExtern)");
    b.add(Regex::str("raise"), "TokenType::Token(Token::KwRaise)");
    b.add(Regex::str("begin"), "TokenType::Token(Token::KwBegin)");
    b.add(Regex::str("rescue"), "TokenType::Token(Token::KwRescue)");
    b.add(Regex::str("finally"), "TokenType::Token(Token::KwFinally)");
    b.add(Regex::str("erased"), "TokenType::Token(Token::KwErased)");
    b.add(Regex::str("token"), "TokenType::Token(Token::KwToken)");
    b.add(
        Regex::str("requires"),
        "TokenType::Token(Token::KwRequires)",
    );
    b.add(Regex::str("ensures"), "TokenType::Token(Token::KwEnsures)");
    b.add(
        Regex::str("maintains"),
        "TokenType::Token(Token::KwMaintains)",
    );
    b.add(Regex::str("assert"), "TokenType::Token(Token::KwAssert)");
    b.add(Regex::str("summon"), "TokenType::Token(Token::KwSummon)");
    b.add(Regex::str("result"), "TokenType::Token(Token::KwResult)");
    b.add(Regex::str("witness"), "TokenType::Token(Token::KwWitness)");
    b.add(
        Regex::str("extension"),
        "TokenType::Token(Token::KwExtension)",
    );
    b.add(Regex::str("inverse"), "TokenType::Token(Token::KwInverse)");
    b.add(Regex::str("update"), "TokenType::Token(Token::KwUpdate)");
    b.add(Regex::str("inline"), "TokenType::Token(Token::KwInline)");
    b.add(
        Regex::str("__argon_unsafe_assume_pure"),
        "TokenType::Token(Token::KwArgonUnsafeAssumePure)",
    );
    b.add(
        Regex::str("operator"),
        "TokenType::Token(Token::KwOperator)",
    );
    b.add(Regex::str("unary"), "TokenType::Token(Token::KwUnary)");
    b.add(Regex::str("boxed"), "TokenType::Token(Token::KwBoxed)");
    b.add(Regex::str("box"), "TokenType::Token(Token::KwBox)");
    b.add(Regex::str("unbox"), "TokenType::Token(Token::KwUnbox)");
    b.add(Regex::str("fn"), "TokenType::Token(Token::KwFn)");
    // b.add(Regex::str("sealed"), "Token::KwSealed");
    b.add(Regex::str("_"), "TokenType::Token(Token::KwUnderscore)");

    b.add(Regex::str("&&&"), "TokenType::Token(Token::OpBitAnd)");
    b.add(Regex::str("|||"), "TokenType::Token(Token::OpBitOr)");
    b.add(Regex::str("&&"), "TokenType::Token(Token::OpLogicalAnd)");
    b.add(Regex::str("^^^"), "TokenType::Token(Token::OpBitXor)");
    b.add(Regex::str("~~~"), "TokenType::Token(Token::OpBitNot)");
    b.add(Regex::str("<<<"), "TokenType::Token(Token::OpShiftLeft)");
    b.add(Regex::str(">>>"), "TokenType::Token(Token::OpShiftRight)");
    b.add(Regex::str("||"), "TokenType::Token(Token::OpLogicalOr)");
    b.add(Regex::str("!="), "TokenType::Token(Token::OpNotEquals)");
    b.add(Regex::str("<="), "TokenType::Token(Token::OpLessThanEq)");
    b.add(Regex::str(">="), "TokenType::Token(Token::OpGreaterThanEq)");
    b.add(Regex::str(":="), "TokenType::Token(Token::OpAssign)");
    b.add(Regex::str("->"), "TokenType::Token(Token::OpArrow)");
    b.add(Regex::str("=>"), "TokenType::Token(Token::OpFatArrow)");
    b.add(Regex::str("++"), "TokenType::Token(Token::OpConcat)");
    b.add(Regex::str(".."), "TokenType::Token(Token::OpDotDot)");
    b.add(Regex::str("**"), "TokenType::Token(Token::OpStarStar)");
    b.add(Regex::str("=="), "TokenType::Token(Token::OpPropEqual)");
    b.add(Regex::str("::"), "TokenType::Token(Token::SymColonColon)");
    b.add(
        Regex::str("\\/"),
        "TokenType::Token(Token::OpPropDisjunction)",
    );
    b.add(
        Regex::str("/\\"),
        "TokenType::Token(Token::OpPropConjunction)",
    );
    b.add(Regex::str("="), "TokenType::Token(Token::OpEquals)");
    b.add(Regex::str("≠"), "TokenType::Token(Token::OpNotEquals)");
    b.add(Regex::str("≤"), "TokenType::Token(Token::OpLessThanEq)");
    b.add(Regex::str("≥"), "TokenType::Token(Token::OpGreaterThanEq)");
    b.add(Regex::str("."), "TokenType::Token(Token::SymDot)");
    b.add(Regex::str(","), "TokenType::Token(Token::SymComma)");
    b.add(Regex::str(";"), "TokenType::Token(Token::Semicolon)");
    b.add(Regex::str("("), "TokenType::Token(Token::SymOpenParen)");
    b.add(Regex::str(")"), "TokenType::Token(Token::SymCloseParen)");
    b.add(Regex::str("["), "TokenType::Token(Token::SymOpenBracket)");
    b.add(Regex::str("]"), "TokenType::Token(Token::SymCloseBracket)");
    b.add(Regex::str("{"), "TokenType::Token(Token::SymOpenCurly)");
    b.add(Regex::str("}"), "TokenType::Token(Token::SymCloseCurly)");
    b.add(Regex::str("!"), "TokenType::Token(Token::OpLogicalNot)");
    b.add(Regex::str("+"), "TokenType::Token(Token::OpPlus)");
    b.add(Regex::str("-"), "TokenType::Token(Token::OpMinus)");
    b.add(Regex::str("*"), "TokenType::Token(Token::OpStar)");
    b.add(Regex::str("×"), "TokenType::Token(Token::OpMul)");
    b.add(Regex::str("/"), "TokenType::Token(Token::OpSlash)");
    b.add(Regex::str("÷"), "TokenType::Token(Token::OpDiv)");
    b.add(Regex::str("<"), "TokenType::Token(Token::OpLessThan)");
    b.add(Regex::str(">"), "TokenType::Token(Token::OpGreaterThan)");
    b.add(Regex::str(":"), "TokenType::Token(Token::SymColon)");
    b.add(Regex::str("@"), "TokenType::Token(Token::SymAt)");
    b.add(Regex::str("|"), "TokenType::Token(Token::SymPipe)");
    b.add(Regex::str("'"), "TokenType::Token(Token::SymSingleQuote)");

    b.into_dfa()
}

pub fn emit_rust_lexer<W: Write>(w: &mut W) -> io::Result<()> {
    let dfa = build_dfa();

    let settings = RustSettings {
        module: None,
        visibility: "pub ".to_owned(),
        type_name: "TokenLexer".to_owned(),
        token_type: "crate::token::TokenType".to_owned(),
        acceptance_expr: true,
        header_stmts: vec!["use crate::token::{Token, TokenType};".to_owned()],
    };

    emit_rust(w, dfa, &settings)
}
