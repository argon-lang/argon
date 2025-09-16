use parse18_lexer_gen::{builder::DFABuilder, regex::Regex, codegen};

fn main() {
    let mut b: DFABuilder<String> = DFABuilder::new();

    b.add(Regex::pattern(r"//[^\n]*"), "TokenType.Whitespace");
    b.add(Regex::pattern(r"[^\S\n]+"), "TokenType.Whitespace");
    b.add(Regex::pattern(r"\\\n"), "TokenType.Whitespace");
    b.add(Regex::pattern(r"\n"), "TokenType.Token(Token.NewLine)");
    b.add(Regex::str("\""), "TokenType.StringStart");

    b.add(Regex::pattern(r"[0-9]+(_[0-9]+)*[\p{L}_][\p{L}\p{N}\p{Mn}\p{Mc}_]*"), "TokenType.InvalidInteger");
    b.add(Regex::pattern(r"0[xX][[:xdigit:]]+(_[[:xdigit:]]+)*[\p{L}_&&[^[[:xdigit:]]]][\p{L}\p{N}\p{Mn}\p{Mc}_]*"), "TokenType.InvalidInteger");
    b.add(Regex::pattern(r"0[bB][01]+(_[01]+)*[\p{L}_][\p{L}\p{N}\p{Mn}\p{Mc}_]*"), "TokenType.InvalidInteger");
    b.add(Regex::pattern(r"0o[0-7]+(_[0-7]+)*[\p{L}_][\p{L}\p{N}\p{Mn}\p{Mc}_]*"), "TokenType.InvalidInteger");

    b.add(Regex::pattern(r"[0-9]+(_[0-9]+)*"), "TokenType.DecInteger");
    b.add(Regex::pattern(r"0[xX][[:xdigit:]]+(_[[:xdigit:]]+)*"), "TokenType.HexInteger");
    b.add(Regex::pattern(r"0[bB][01]+(_[01]+)*"), "TokenType.BinInteger");
    b.add(Regex::pattern(r"0o[0-7]+(_[0-7]+)*"), "TokenType.OctInteger");


    b.add(Regex::pattern(r"[\p{L}_][\p{L}\p{N}\p{Mn}\p{Mc}_]*[!?]?"), "TokenType.Identifier");

    b.add(Regex::str("__argon_builtin"), "TokenType.Token(Token.KW_ARGON_BUILTIN)");
    b.add(Regex::str("def"), "TokenType.Token(Token.KW_DEF)");
    b.add(Regex::str("proc"), "TokenType.Token(Token.KW_PROC)");
    b.add(Regex::str("do"), "TokenType.Token(Token.KW_DO)");
    b.add(Regex::str("end"), "TokenType.Token(Token.KW_END)");
    b.add(Regex::str("let"), "TokenType.Token(Token.KW_LET)");
    b.add(Regex::str("val"), "TokenType.Token(Token.KW_VAL)");
    b.add(Regex::str("mut"), "TokenType.Token(Token.KW_MUT)");
    b.add(Regex::str("module"), "TokenType.Token(Token.KW_MODULE)");
    b.add(Regex::str("record"), "TokenType.Token(Token.KW_RECORD)");
    b.add(Regex::str("enum"), "TokenType.Token(Token.KW_ENUM)");
    b.add(Regex::str("trait"), "TokenType.Token(Token.KW_TRAIT)");
    b.add(Regex::str("new"), "TokenType.Token(Token.KW_NEW)");
    b.add(Regex::str("true"), "TokenType.Token(Token.KW_TRUE)");
    b.add(Regex::str("false"), "TokenType.Token(Token.KW_FALSE)");
    b.add(Regex::str("as"), "TokenType.Token(Token.KW_AS)");
    b.add(Regex::str("is"), "TokenType.Token(Token.KW_IS)");
    b.add(Regex::str("import"), "TokenType.Token(Token.KW_IMPORT)");
    b.add(Regex::str("export"), "TokenType.Token(Token.KW_EXPORT)");
    b.add(Regex::str("public"), "TokenType.Token(Token.KW_PUBLIC)");
    b.add(Regex::str("protected"), "TokenType.Token(Token.KW_PROTECTED)");
    b.add(Regex::str("private"), "TokenType.Token(Token.KW_PRIVATE)");
    b.add(Regex::str("internal"), "TokenType.Token(Token.KW_INTERNAL)");
    b.add(Regex::str("if"), "TokenType.Token(Token.KW_IF)");
    b.add(Regex::str("then"), "TokenType.Token(Token.KW_THEN)");
    b.add(Regex::str("else"), "TokenType.Token(Token.KW_ELSE)");
    b.add(Regex::str("elsif"), "TokenType.Token(Token.KW_ELSIF)");
    b.add(Regex::str("match"), "TokenType.Token(Token.KW_MATCH)");
    b.add(Regex::str("case"), "TokenType.Token(Token.KW_CASE)");
    b.add(Regex::str("type"), "TokenType.Token(Token.KW_TYPE)");
    b.add(Regex::str("type!"), "TokenType.Token(Token.KW_BIGTYPE)");
    b.add(Regex::str("extern"), "TokenType.Token(Token.KW_EXTERN)");
    b.add(Regex::str("raise"), "TokenType.Token(Token.KW_RAISE)");
    b.add(Regex::str("begin"), "TokenType.Token(Token.KW_BEGIN)");
    b.add(Regex::str("rescue"), "TokenType.Token(Token.KW_RESCUE)");
    b.add(Regex::str("finally"), "TokenType.Token(Token.KW_FINALLY)");
    b.add(Regex::str("erased"), "TokenType.Token(Token.KW_ERASED)");
    b.add(Regex::str("requires"), "TokenType.Token(Token.KW_REQUIRES)");
    b.add(Regex::str("ensures"), "TokenType.Token(Token.KW_ENSURES)");
    b.add(Regex::str("maintains"), "TokenType.Token(Token.KW_MAINTAINS)");
    b.add(Regex::str("assert"), "TokenType.Token(Token.KW_ASSERT)");
    b.add(Regex::str("summon"), "TokenType.Token(Token.KW_SUMMON)");
    b.add(Regex::str("witness"), "TokenType.Token(Token.KW_WITNESS)");
    b.add(Regex::str("extension"), "TokenType.Token(Token.KW_EXTENSION)");
    b.add(Regex::str("inverse"), "TokenType.Token(Token.KW_INVERSE)");
    b.add(Regex::str("update"), "TokenType.Token(Token.KW_UPDATE)");
    b.add(Regex::str("inline"), "TokenType.Token(Token.KW_INLINE)");
    b.add(Regex::str("operator"), "TokenType.Token(Token.KW_OPERATOR)");
    b.add(Regex::str("unary"), "TokenType.Token(Token.KW_UNARY)");
    b.add(Regex::str("boxed"), "TokenType.Token(Token.KW_BOXED)");
    b.add(Regex::str("box"), "TokenType.Token(Token.KW_BOX)");
    b.add(Regex::str("unbox"), "TokenType.Token(Token.KW_UNBOX)");
    b.add(Regex::str("fn"), "TokenType.Token(Token.KW_FN)");
    // b.add(Regex::str("sealed"), "TokenType.Token(Token.KW_SEALED)");
    b.add(Regex::str("_"), "TokenType.Token(Token.KW_UNDERSCORE)");




    b.add(Regex::str("&&&"), "TokenType.Token(Token.OP_BITAND)");
    b.add(Regex::str("|||"), "TokenType.Token(Token.OP_BITOR)");
    b.add(Regex::str("&&"), "TokenType.Token(Token.OP_LOGICAL_AND)");
    b.add(Regex::str("^^^"), "TokenType.Token(Token.OP_BITXOR)");
    b.add(Regex::str("~~~"), "TokenType.Token(Token.OP_BITNOT)");
    b.add(Regex::str("<<<"), "TokenType.Token(Token.OP_SHIFTLEFT)");
    b.add(Regex::str(">>>"), "TokenType.Token(Token.OP_SHIFTRIGHT)");
    b.add(Regex::str("||"), "TokenType.Token(Token.OP_LOGICAL_OR)");
    b.add(Regex::str("!="), "TokenType.Token(Token.OP_NOTEQUALS)");
    b.add(Regex::str("<="), "TokenType.Token(Token.OP_LESSTHANEQ)");
    b.add(Regex::str(">="), "TokenType.Token(Token.OP_GREATERTHANEQ)");
    b.add(Regex::str(":="), "TokenType.Token(Token.OP_ASSIGN)");
    b.add(Regex::str("->"), "TokenType.Token(Token.OP_ARROW)");
    b.add(Regex::str("=>"), "TokenType.Token(Token.OP_FAT_ARROW)");
    b.add(Regex::str("++"), "TokenType.Token(Token.OP_CONCAT)");
    b.add(Regex::str(".."), "TokenType.Token(Token.OP_DOTDOT)");
    b.add(Regex::str("**"), "TokenType.Token(Token.OP_STARSTAR)");
    b.add(Regex::str("=="), "TokenType.Token(Token.OP_PROP_EQUAL)");
    b.add(Regex::str("::"), "TokenType.Token(Token.SYM_COLONCOLON)");
    b.add(Regex::str("\\/"), "TokenType.Token(Token.OP_PROP_DISJUNCTION)");
    b.add(Regex::str("/\\"), "TokenType.Token(Token.OP_PROP_CONJUNCTION)");
    b.add(Regex::str("="), "TokenType.Token(Token.OP_EQUALS)");
    b.add(Regex::str("≠"), "TokenType.Token(Token.OP_NOTEQUALS)");
    b.add(Regex::str("≤"), "TokenType.Token(Token.OP_LESSTHANEQ)");
    b.add(Regex::str("≥"), "TokenType.Token(Token.OP_GREATERTHANEQ)");
    b.add(Regex::str("."), "TokenType.Token(Token.SYM_DOT)");
    b.add(Regex::str(","), "TokenType.Token(Token.SYM_COMMA)");
    b.add(Regex::str(";"), "TokenType.Token(Token.Semicolon)");
    b.add(Regex::str("("), "TokenType.Token(Token.SYM_OPENPAREN)");
    b.add(Regex::str(")"), "TokenType.Token(Token.SYM_CLOSEPAREN)");
    b.add(Regex::str("["), "TokenType.Token(Token.SYM_OPENBRACKET)");
    b.add(Regex::str("]"), "TokenType.Token(Token.SYM_CLOSEBRACKET)");
    b.add(Regex::str("{"), "TokenType.Token(Token.SYM_OPENCURLY)");
    b.add(Regex::str("}"), "TokenType.Token(Token.SYM_CLOSECURLY)");
    b.add(Regex::str("!"), "TokenType.Token(Token.OP_LOGICAL_NOT)");
    b.add(Regex::str("+"), "TokenType.Token(Token.OP_PLUS)");
    b.add(Regex::str("-"), "TokenType.Token(Token.OP_MINUS)");
    b.add(Regex::str("*"), "TokenType.Token(Token.OP_STAR)");
    b.add(Regex::str("×"), "TokenType.Token(Token.OP_MUL)");
    b.add(Regex::str("/"), "TokenType.Token(Token.OP_SLASH)");
    b.add(Regex::str("÷"), "TokenType.Token(Token.OP_DIV)");
    b.add(Regex::str("<"), "TokenType.Token(Token.OP_LESSTHAN)");
    b.add(Regex::str(">"), "TokenType.Token(Token.OP_GREATERTHAN)");
    b.add(Regex::str(":"), "TokenType.Token(Token.SYM_COLON)");
    b.add(Regex::str("@"), "TokenType.Token(Token.SYM_AT)");
    b.add(Regex::str("|"), "TokenType.Token(Token.SYM_PIPE)");



    let dfa = b.into_dfa();

    let settings = codegen::scala::ScalaSettings {
        package: "dev.argon.parser".to_owned(),
        access_modifier: "private[parser] ".to_owned(),
        object_name: "TokenLexer".to_owned(),
        token_type: "TokenType".to_owned(),
    };

    codegen::scala::emit_scala(&mut std::io::stdout(), dfa, &settings).unwrap();
}
