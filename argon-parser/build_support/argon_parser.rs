use parse18_ll_gen::codegen::rust::{RustSettings, emit_rust};
use parse18_ll_gen::grammar::{
    Grammar, GrammarFactory, GrammarTypes, RuleInfo, RuleSetInfo, SymbolInfo, TerminalInfo, error,
    nonterm, rule as base_rule, ruleset as base_ruleset, term,
};
use std::io::{self, Write};

#[allow(unused, reason = "Some cases represent reserved keywords")]
#[allow(
    clippy::enum_variant_names,
    reason = "Names are too common without the suffix"
)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, strum::Display)]
pub enum Token {
    // String lexing (interpolation-capable)
    #[strum(serialize = "StringStart")]
    StringStart,
    #[strum(serialize = "StringEnd")]
    StringEnd,
    #[strum(serialize = "StringText")]
    StringText,
    #[strum(serialize = "StringInterpolationStart")]
    StringInterpolationStart,

    // Literals and identifiers
    #[strum(serialize = "IntToken")]
    IntToken,
    #[strum(serialize = "Identifier")]
    IdentifierToken,

    // Layout
    #[strum(serialize = "NewLine")]
    NewLine,
    #[strum(serialize = "Semicolon")]
    Semicolon,

    // Keywords
    #[strum(serialize = "KwArgonBuiltin")]
    KwArgonBuiltin,
    #[strum(serialize = "KwDef")]
    KwDef,
    #[strum(serialize = "KwProc")]
    KwProc,
    #[strum(serialize = "KwDo")]
    KwDo,
    #[strum(serialize = "KwEnd")]
    KwEnd,
    #[strum(serialize = "KwLet")]
    KwLet,
    #[strum(serialize = "KwVal")]
    KwVal,
    #[strum(serialize = "KwMut")]
    KwMut,
    #[strum(serialize = "KwModule")]
    KwModule,
    #[strum(serialize = "KwRecord")]
    KwRecord,
    #[strum(serialize = "KwEnum")]
    KwEnum,
    #[strum(serialize = "KwTrait")]
    KwTrait,
    #[strum(serialize = "KwInstance")]
    KwInstance,
    #[strum(serialize = "KwNew")]
    KwNew,
    #[strum(serialize = "KwWith")]
    KwWith,
    #[strum(serialize = "KwTrue")]
    KwTrue,
    #[strum(serialize = "KwFalse")]
    KwFalse,
    #[strum(serialize = "KwAs")]
    KwAs,
    #[strum(serialize = "KwIs")]
    KwIs,
    #[strum(serialize = "KwImport")]
    KwImport,
    #[strum(serialize = "KwExport")]
    KwExport,
    #[strum(serialize = "KwPublic")]
    KwPublic,
    #[strum(serialize = "KwProtected")]
    KwProtected,
    #[strum(serialize = "KwPrivate")]
    KwPrivate,
    #[strum(serialize = "KwInternal")]
    KwInternal,
    #[strum(serialize = "KwAbstract")]
    KwAbstract,
    #[strum(serialize = "KwFinal")]
    KwFinal,
    #[strum(serialize = "KwVirtual")]
    KwVirtual,
    #[strum(serialize = "KwOverride")]
    KwOverride,
    #[strum(serialize = "KwIf")]
    KwIf,
    #[strum(serialize = "KwThen")]
    KwThen,
    #[strum(serialize = "KwElse")]
    KwElse,
    #[strum(serialize = "KwElsif")]
    KwElsif,
    #[strum(serialize = "KwMatch")]
    KwMatch,
    #[strum(serialize = "KwCase")]
    KwCase,
    #[strum(serialize = "KwLoop")]
    KwLoop,
    #[strum(serialize = "KwWhile")]
    KwWhile,
    #[strum(serialize = "KwBreak")]
    KwBreak,
    #[strum(serialize = "KwNext")]
    KwNext,
    #[strum(serialize = "KwRedo")]
    KwRedo,
    #[strum(serialize = "KwRetry")]
    KwRetry,
    #[strum(serialize = "KwAnd")]
    KwAnd,
    #[strum(serialize = "KwOr")]
    KwOr,
    #[strum(serialize = "KwType")]
    KwType,
    #[strum(serialize = "KwBigType")]
    KwBigType,
    #[strum(serialize = "KwUnderscore")]
    KwUnderscore,
    #[strum(serialize = "KwExtern")]
    KwExtern,
    #[strum(serialize = "KwRaise")]
    KwRaise,
    #[strum(serialize = "KwBegin")]
    KwBegin,
    #[strum(serialize = "KwRescue")]
    KwRescue,
    #[strum(serialize = "KwFinally")]
    KwFinally,
    #[strum(serialize = "KwErased")]
    KwErased,
    #[strum(serialize = "KwToken")]
    KwToken,
    #[strum(serialize = "KwRequires")]
    KwRequires,
    #[strum(serialize = "KwEnsures")]
    KwEnsures,
    #[strum(serialize = "KwMaintains")]
    KwMaintains,
    #[strum(serialize = "KwAssert")]
    KwAssert,
    #[strum(serialize = "KwSummon")]
    KwSummon,
    #[strum(serialize = "KwWitness")]
    KwWitness,
    #[strum(serialize = "KwInline")]
    KwInline,
    #[strum(serialize = "KwExtension")]
    KwExtension,
    #[strum(serialize = "KwInverse")]
    KwInverse,
    #[strum(serialize = "KwUpdate")]
    KwUpdate,
    #[strum(serialize = "KwOperator")]
    KwOperator,
    #[strum(serialize = "KwUnary")]
    KwUnary,
    #[strum(serialize = "KwBoxed")]
    KwBoxed,
    #[strum(serialize = "KwBox")]
    KwBox,
    #[strum(serialize = "KwUnbox")]
    KwUnbox,
    #[strum(serialize = "KwFn")]
    KwFn,

    // Operators
    #[strum(serialize = "OpLogicalAnd")]
    OpLogicalAnd,
    #[strum(serialize = "OpLogicalOr")]
    OpLogicalOr,
    #[strum(serialize = "OpEquals")]
    OpEquals,
    #[strum(serialize = "OpNotEquals")]
    OpNotEquals,
    #[strum(serialize = "OpLessThanEq")]
    OpLessThanEq,
    #[strum(serialize = "OpGreaterThanEq")]
    OpGreaterThanEq,
    #[strum(serialize = "OpShiftLeft")]
    OpShiftLeft,
    #[strum(serialize = "OpShiftRight")]
    OpShiftRight,
    #[strum(serialize = "OpAssign")]
    OpAssign,
    #[strum(serialize = "OpLogicalNot")]
    OpLogicalNot,
    #[strum(serialize = "OpPlus")]
    OpPlus,
    #[strum(serialize = "OpMinus")]
    OpMinus,
    #[strum(serialize = "OpStar")]
    OpStar,
    #[strum(serialize = "OpMul")]
    OpMul,
    #[strum(serialize = "OpSlash")]
    OpSlash,
    #[strum(serialize = "OpDiv")]
    OpDiv,
    #[strum(serialize = "OpBitAnd")]
    OpBitAnd,
    #[strum(serialize = "OpBitOr")]
    OpBitOr,
    #[strum(serialize = "OpBitXor")]
    OpBitXor,
    #[strum(serialize = "OpBitNot")]
    OpBitNot,
    #[strum(serialize = "OpLessThan")]
    OpLessThan,
    #[strum(serialize = "OpGreaterThan")]
    OpGreaterThan,
    #[strum(serialize = "OpUnion")]
    OpUnion,
    #[strum(serialize = "OpIntersection")]
    OpIntersection,
    #[strum(serialize = "OpConcat")]
    OpConcat,
    #[strum(serialize = "OpStarStar")]
    OpStarStar,
    #[strum(serialize = "OpArrow")]
    OpArrow,
    #[strum(serialize = "OpFatArrow")]
    OpFatArrow,
    #[strum(serialize = "OpPropEqual")]
    OpPropEqual,
    #[strum(serialize = "OpPropDisjunction")]
    OpPropDisjunction,
    #[strum(serialize = "OpPropConjunction")]
    OpPropConjunction,
    #[strum(serialize = "OpDotDot")]
    OpDotDot,

    // Symbols
    #[strum(serialize = "SymDot")]
    SymDot,
    #[strum(serialize = "SymComma")]
    SymComma,
    #[strum(serialize = "SymColon")]
    SymColon,
    #[strum(serialize = "SymColonColon")]
    SymColonColon,
    #[strum(serialize = "SymOpenParen")]
    SymOpenParen,
    #[strum(serialize = "SymCloseParen")]
    SymCloseParen,
    #[strum(serialize = "SymOpenBracket")]
    SymOpenBracket,
    #[strum(serialize = "SymCloseBracket")]
    SymCloseBracket,
    #[strum(serialize = "SymOpenCurly")]
    SymOpenCurly,
    #[strum(serialize = "SymCloseCurly")]
    SymCloseCurly,
    #[strum(serialize = "SymPipe")]
    SymPipe,
    #[strum(serialize = "SymAt")]
    SymAt,
    #[strum(serialize = "SymSingleQuote")]
    SymSingleQuote,
}

impl TerminalInfo for Token {
    fn has_payload(self) -> bool {
        matches!(
            self,
            Token::StringText | Token::IntToken | Token::IdentifierToken
        )
    }

    fn payload_type(self) -> Option<&'static str> {
        match self {
            Token::StringText | Token::IdentifierToken => Some("Box<str>"),
            Token::IntToken => Some("BigUint"),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, strum::Display)]
enum Rule {
    Start,

    IdentifierOptional,
    Identifier,
    ComplexIdentifier,
    BinaryOperatorName,
    UnaryOperatorName,
    MethodName,
    NewLines,
    StatementSeparator,

    IfExpr,
    IfExprFromCond,
    IfExprAfterThen,

    LoopExpr,
    WhileExpr,
    LoopLabel,
    BreakExpr,
    NextExpr,
    RedoExpr,
    RetryExpr,

    StringExpr,
    StringExprRest,
    StringExprContent,
    StringFragment,
    StringInterpolationRest,

    MatchExpr,
    MatchCases,
    MatchCase,
    MatchCaseBody,

    #[strum(serialize = "PrimaryExpr_{0}")]
    PrimaryExpr(ParenAllowedState),

    #[strum(serialize = "PostfixExpr_{0}")]
    PostfixExpr(ParenAllowedState),

    #[strum(serialize = "CurryCallExpr_Prefix{0}")]
    CurryCallExpr(ParenAllowedState),
    CurryCallExprArgs1,
    CurryCallExprArgs2,
    EnclosedArgList,
    EnclosedArgListParen,
    EnclosedArgListQuote,
    EnclosedArgListExtraQuotes,
    RecordLiteralFieldsWithBlockEnd,
    RecordLiteralFields,
    RecordLiteralField,
    UnaryExpr,
    TypeExpr,
    MultiplicativeExpr,
    AdditiveExpr,
    ShiftExpr,
    BitwiseAndExpr,
    BitwiseXorExpr,
    BitwiseOrExpr,
    FunctionTypeExpr,
    FunctionTypeExprRest,
    RelationalExpr,
    EqualityExpr,
    LogicalAndExpr,
    LogicalOrExpr,
    PropConjunctionExpr,
    PropDisjunctionExpr,
    PropEqualityExpr,
    AsExpr,
    IsExpr,
    RaiseExpr,
    ClosureExpr,
    TupleExpr,
    TupleExprRest,
    AssignExpr,
    AssertExpr,
    ControlExpr,
    Expression,
    TypeBinding,

    Pattern,
    #[strum(serialize = "SimplePattern_{0}")]
    SimplePattern(ParenAllowedState),
    NonTuplePattern,
    TuplePattern,
    NonEmptyTuplePattern,
    PatternPath,
    ConstructorArgsPattern,
    ConstructorArgPattern,

    Modifiers,
    Modifiers1,
    Modifier,
    VariableDeclarationRest,
    VariableDeclarationBinding,
    VariableMutSpec,
    MutSpec,
    VariableDeclarationTypeSpec,
    VariableDeclarationValue,
    MethodOrFunctionDeclarationStmtRest,
    MethodPurity,
    MethodParameters,
    MethodParameterList,
    MethodParameterListModifiers,
    MethodParameterListModifier,
    MethodParameterListContents,
    MethodParameterListContents1,
    MethodParameter,
    MethodReturnType,
    MethodEnsuresClause,
    MethodBody,
    FunctionBody,
    ExternFunctionBody,
    RecordDeclarationStmtRest,
    RecordBody,
    RecordBodyStmt,
    RecordField,
    EnumDeclarationStmtRest,
    EnumBody,
    EnumBodyStmt,
    EnumConstructorVariant,
    TraitDeclarationStmtRest,
    TraitBody,
    TraitBodyStmt,
    NewTraitObjectBody,
    NewTraitObjectBodyStmt,
    TypeDeclarationTypeAnnotation,
    InstanceDeclarationStmtRest,

    TubeName,
    ImportStmt,
    ExportStmt,
    ImportPath,
    ImportPathRelativePrefix,
    ImportPathSegment,
    ImportPathMulti,

    Statement,
    StatementList,
    BlockBodyWithEnd,
    BlockBody,

    ModuleDeclaration,
    ModulePath,
    ModulePath1,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, strum::Display)]
enum ParenAllowedState {
    #[strum(serialize = "ParenAllowed")]
    Allowed,

    #[strum(serialize = "ParenNotAllowed")]
    NotAllowed,
}

use Rule::*;
use Token::*;

#[derive(Clone, Copy, Debug)]
struct ParserFactory;

impl GrammarTypes for ParserFactory {
    type Terminal = Token;
    type Rule = Rule;
    type ExternalRuleType = String;
    type ExternalFunction = String;
    type ExternalLexMode = String;
}

impl GrammarFactory for ParserFactory {
    fn start_rule(&self) -> Self::Rule {
        Start
    }

    fn create_rule_set(&self, r: Self::Rule) -> RuleSetInfo<Self> {
        match r {
            Start => ruleset(
                "ModuleDeclaration",
                [
                    rule(
                        [
                            nonterm(NewLines).discard(),
                            nonterm(ModuleDeclaration),
                            nonterm(StatementList),
                        ],
                        "module_declaration"
                    )
                ],
            ),

            IdentifierOptional => ruleset(
                "Option<Identifier>",
                [
                    rule([ term(KwUnderscore).discard() ], "(|| None)"),
                    rule([ nonterm(Identifier) ], "Some"),
                ],
            ),
            Identifier => ruleset(
                "Identifier",
                [
                    rule([ term(IdentifierToken) ], "identifier_expr_named_token"),
                    rule([ nonterm(ComplexIdentifier) ], "identity"),
                ],
            ),
            ComplexIdentifier => ruleset(
                "Identifier",
                [
                    rule([ term(KwExtension).discard(), nonterm(NewLines).discard(), nonterm(Identifier) ], "(move |id| identifier_expr_extension(id))"),
                    rule([ term(KwInverse).discard(), nonterm(NewLines).discard(), nonterm(Identifier) ], "(move |id| identifier_expr_inverse(id))"),
                    rule([ term(KwUpdate).discard(), nonterm(NewLines).discard(), nonterm(Identifier) ], "(move |id| identifier_expr_update(id))"),
                    rule([ term(KwUnary).discard(), nonterm(NewLines).discard(), term(KwOperator).discard(), nonterm(NewLines).discard(), nonterm(UnaryOperatorName) ], "Identifier::UnaryOp"),
                    rule([ term(KwOperator).discard(), nonterm(NewLines).discard(), nonterm(BinaryOperatorName) ], "Identifier::BinaryOp"),
                    rule([ term(KwOperator).discard(), nonterm(NewLines).discard(), term(SymOpenBracket).discard(), term(SymCloseBracket).discard() ], "(|| Identifier::Index)"),
                ],
            ),
            MethodName => ruleset(
                "Identifier",
                [
                    rule([ nonterm(Identifier) ], "identity"),
                    rule([ nonterm(Identifier), term(OpAssign).discard() ], "identifier_expr_update"),
                ],
            ),

            BinaryOperatorName => ruleset(
                "BinaryOperatorIdentifier",
                [
                    binary_operator_name_rule(OpPlus, "Plus"),
                    binary_operator_name_rule(OpMinus, "Minus"),
                    binary_operator_name_rule(OpStar, "Mul"),
                    binary_operator_name_rule(OpMul, "Mul"),
                    binary_operator_name_rule(OpSlash, "Div"),
                    binary_operator_name_rule(OpDiv, "Div"),
                    binary_operator_name_rule(OpConcat, "Concat"),
                    binary_operator_name_rule(OpEquals, "Equal"),
                    binary_operator_name_rule(OpNotEquals, "NotEqual"),
                    binary_operator_name_rule(OpLessThan, "LessThan"),
                    binary_operator_name_rule(OpLessThanEq, "LessThanEq"),
                    binary_operator_name_rule(OpGreaterThan, "GreaterThan"),
                    binary_operator_name_rule(OpGreaterThanEq, "GreaterThanEq"),
                    binary_operator_name_rule(OpBitAnd, "BitAnd"),
                    binary_operator_name_rule(OpBitOr, "BitOr"),
                    binary_operator_name_rule(OpBitXor, "BitXOr"),
                    binary_operator_name_rule(OpShiftLeft, "ShiftLeft"),
                    binary_operator_name_rule(OpShiftRight, "ShiftRight"),
                ],
            ),
            UnaryOperatorName => ruleset(
                "UnaryOperatorIdentifier",
                [
                    unary_operator_name_rule(OpPlus, "Plus"),
                    unary_operator_name_rule(OpMinus, "Minus"),
                    unary_operator_name_rule(OpBitNot, "BitNot"),
                ],
            ),

            NewLines => ruleset(
                "()",
                [
                    rule([ term(NewLine).discard(), nonterm(NewLines).discard() ], "(|| ())"),
                    rule([], "(|| ())"),
                ],
            ),
            StatementSeparator => ruleset(
                "()",
                [
                    rule([ term(NewLine).discard() ], "(|| ())"),
                    rule([ term(Semicolon).discard() ], "(|| ())"),
                ],
            ),
            IfExpr => ruleset(
                "Expr",
                [
                    rule([ term(KwIf).discard(), nonterm(IfExprFromCond) ], "identity"),
                ],
            ),
            IfExprFromCond => ruleset(
                "Expr",
                [
                    rule(
                        [
                            nonterm(Expression).with_location(),
                            term(KwThen).discard(),
                            nonterm(IfExprAfterThen),
                        ],
                        "if_expr_from_cond_apply",
                    ),
                ],
            ).lex_mode("LexerMode::SkipNewLines"),
            IfExprAfterThen => ruleset(
                "Box<dyn FnOnce(WithLocation<Expr>) -> Expr>",
                [
                    rule(
                        [
                            nonterm(StatementList).with_location(),
                            term(KwEnd).with_location().discard(),
                        ],
                        "if_expr_after_then_end",
                    ),
                    rule(
                        [
                            nonterm(StatementList).with_location(),
                            term(KwElse).discard(),
                            nonterm(StatementList).with_location(),
                            term(KwEnd).discard(),
                        ],
                        "if_expr_after_then_else",
                    ),
                    rule(
                        [
                            nonterm(StatementList).with_location(),
                            term(KwElsif).discard(),
                            nonterm(IfExprFromCond).with_location(),
                        ],
                        "if_expr_after_then_elsif",
                    ),
                ],
            ).lex_mode("LexerMode::Normal"),

            LoopExpr => ruleset(
                "Expr",
                [
                    rule([ term(KwLoop).discard(), nonterm(LoopLabel), nonterm(StatementList).with_location(), term(KwEnd).discard() ], "expr_loop"),
                ],
            ),

            WhileExpr => ruleset(
                "Expr",
                [
                    rule([ term(KwWhile).discard(), nonterm(LoopLabel), nonterm(StatementList).with_location(), term(KwDo).discard(), nonterm(StatementList).with_location(), term(KwEnd).discard() ], "expr_while"),
                    rule([ term(KwWhile).discard(), nonterm(LoopLabel), nonterm(StatementList).with_location(), term(KwEnd).with_location() ], "expr_while_no_body"),
                ],
            ),

            LoopLabel => ruleset(
                "Option<WithLocation<Identifier>>",
                [
                    rule([ term(SymSingleQuote).discard(), nonterm(Identifier).with_location() ], "Some"),
                    rule([], "(|| None)"),
                ],
            ),



            StringExpr => ruleset(
                "StringLiteral",
                [
                    rule([ term(StringStart).discard(), nonterm(StringExprRest) ], "identity"),
                ]
            ).lex_mode("LexerMode::StringText"),
            StringExprRest => ruleset(
                "StringLiteral",
                [
                    rule([ nonterm(StringExprContent), term(StringEnd).discard() ], "simplify_fragments"),
                ]
            ),
            StringExprContent => ruleset(
                "VecDeque<StringFragment>",
                [
                    rule([], "empty_seq"),
                    rule([ nonterm(StringFragment), nonterm(StringExprContent) ], "(move |h, t| prepend(h, t))"),
                ],
            ),
            StringFragment => ruleset(
                "StringFragment",
                [
                    rule([ term(StringText) ], "string_fragment_text_token"),
                    rule([ term(StringInterpolationStart).discard(), nonterm(StringInterpolationRest) ], "identity"),
                ],
            ),
            StringInterpolationRest => ruleset(
                "StringFragment",
                [
                    rule([ nonterm(Expression).with_location(), term(SymCloseCurly).discard() ], "string_fragment_interpolate"),
                ],
            ).lex_mode("LexerMode::SkipNewLines"),

            MatchExpr => ruleset(
                "Expr",
                [
                    rule(
                        [
                            term(KwMatch).discard(),
                            nonterm(Expression).with_location(),
                            nonterm(MatchCases),
                            term(KwEnd).discard(),
                        ],
                        "expr_match",
                    )
                ]
            ).lex_mode("LexerMode::SkipNewLines"),
            MatchCases => ruleset(
                "VecDeque<WithLocation<MatchCase>>",
                [
                    rule([], "empty_seq"),
                    rule(
                        [
                            nonterm(MatchCase).with_location(),
                            nonterm(MatchCases),
                        ],
                        "(move |h, t| prepend(h, t))",
                    ),
                ]
            ),
            MatchCase => ruleset(
                "MatchCase",
                [
                    rule(
                        [
                            term(KwCase).discard(),
                            nonterm(Pattern).with_location(),
                            term(OpFatArrow).discard(),
                            nonterm(MatchCaseBody).with_location(),
                        ],
                        "match_case"
                    ),
                ],
            ),
            MatchCaseBody => ruleset(
                "Expr",
                [
                    rule([ nonterm(BlockBody) ], "identity"),
                ],
            ).lex_mode("LexerMode::Normal"),

            PrimaryExpr(ParenAllowedState::NotAllowed) => ruleset(
                "Expr",
                [
                    rule([ nonterm(Identifier) ], "expr_identifier"),
                    rule([ nonterm(StringExpr) ], "expr_string_literal"),
                    rule([ nonterm(MatchExpr) ], "identity"),
                    rule([ term(IntToken) ], "expr_int_literal_token"),
                    rule([ term(KwTrue).discard() ], "(|| expr_bool_literal(true))"),
                    rule([ term(KwFalse).discard() ], "(|| expr_bool_literal(false))"),
                    rule([ nonterm(IfExpr) ], "identity"),
                    rule([ term(KwBegin).discard(), nonterm(BlockBodyWithEnd) ], "with_location_value"),
                    // function result value
                    rule([ term(KwArgonBuiltin).discard(), term(IdentifierToken) ], "expr_builtin_token"),
                    rule([ nonterm(LoopExpr) ], "identity"),
                    rule([ nonterm(WhileExpr) ], "identity"),
                    expr_error(),
                ],
            ),
            PrimaryExpr(ParenAllowedState::Allowed) => ruleset(
                "Expr",
                [
                    rule([ nonterm(PrimaryExpr(ParenAllowedState::NotAllowed)) ], "identity"),
                    rule([ term(SymOpenParen).discard(), term(SymCloseParen).discard() ], "(|| expr_tuple(empty_seq()))"),
                    rule([ term(SymOpenParen).discard(), nonterm(Expression).with_location(), term(SymCloseParen).discard() ], "(|e| Expr::Paren(Box::new(e)))"),
                    expr_error(),
                ],
            ),
            PostfixExpr(paren_allowed) => {
                let mut rules = vec![
                    expr_error(),
                    rule([ nonterm(PrimaryExpr(paren_allowed)) ], "identity"),
                    rule(
                        [
                            nonterm(PostfixExpr(paren_allowed)).with_location(),
                            term(SymDot).discard(),
                            nonterm(Identifier).with_location(),
                        ],
                        "expr_dot",
                    ),
                    rule(
                        [
                            nonterm(PostfixExpr(paren_allowed)).with_location(),
                            term(SymDot).discard(),
                            term(KwBoxed).discard(),
                        ],
                        "expr_boxed_type",
                    ),
                    rule(
                        [
                            nonterm(PostfixExpr(paren_allowed)).with_location(),
                            term(SymDot).discard(),
                            term(KwBox).discard(),
                        ],
                        "expr_box",
                    ),
                    rule(
                        [
                            nonterm(PostfixExpr(paren_allowed)).with_location(),
                            term(SymDot).discard(),
                            term(KwUnbox).discard(),
                        ],
                        "expr_unbox",
                    ),
                    rule(
                        [
                            nonterm(PostfixExpr(paren_allowed)).with_location(),
                            term(SymDot).discard(),
                            term(KwNew).discard(),
                            nonterm(NewTraitObjectBody),
                            term(KwEnd).discard(),
                        ],
                        "expr_new_trait_object"
                    ),
                    rule(
                        [
                            nonterm(PostfixExpr(paren_allowed)).with_location(),
                            term(SymOpenCurly).discard(),
                            nonterm(RecordLiteralFieldsWithBlockEnd),
                        ],
                        "expr_record_literal",
                    ),
                ];

                if paren_allowed == ParenAllowedState::Allowed {
                    rules.push(rule(
                        [
                            nonterm(PostfixExpr(paren_allowed)).with_location(),
                            nonterm(EnclosedArgList),
                        ],
                        "(move |func_expr, args| { let (list_type, arg_expr) = args; expr_function_call(func_expr, list_type, arg_expr) })",
                    ));
                }

                ruleset(
                    "Expr",
                    rules,
                )
            },
            CurryCallExpr(prefix_paren_allowed) => ruleset(
                "Expr",
                [
                    rule(
                        [
                            nonterm(PostfixExpr(prefix_paren_allowed)).with_location(),
                            nonterm(CurryCallExprArgs1),
                        ],
                        "(move |func_expr, args| curried_call(func_expr, args))",
                    ),
                    expr_error(),
                ],
            ),
            CurryCallExprArgs1 => ruleset(
                "VecDeque<(FunctionParameterListType, WithLocation<Expr>)>",
                [
                    rule([], "empty_seq"),
                    rule(
                        [
                            nonterm(PostfixExpr(ParenAllowedState::NotAllowed)).with_location(),
                            nonterm(CurryCallExprArgs2),
                        ],
                        "(move |h, t| prepend((FunctionParameterListType::NormalList, h), t))",
                    ),
                ],
            ),
            CurryCallExprArgs2 => ruleset(
                "VecDeque<(FunctionParameterListType, WithLocation<Expr>)>",
                [
                    rule([], "empty_seq"),
                    rule(
                        [
                            nonterm(EnclosedArgList),
                            nonterm(CurryCallExprArgs2),
                        ],
                        "(move |h, t| prepend(h, t))",
                    ),
                    rule(
                        [
                            nonterm(PostfixExpr(ParenAllowedState::NotAllowed)).with_location(),
                            nonterm(CurryCallExprArgs2),
                        ],
                        "(move |h, t| prepend((FunctionParameterListType::NormalList, h), t))",
                    ),
                ],
            ),
            EnclosedArgList => ruleset(
                "(FunctionParameterListType, WithLocation<Expr>)",
                [
                    rule([ term(SymOpenParen).discard(), nonterm(EnclosedArgListParen) ], "identity"),
                    rule(
                        [
                            term(SymSingleQuote).discard(),
                            nonterm(EnclosedArgListExtraQuotes),
                            term(SymOpenParen).discard(),
                            nonterm(EnclosedArgListQuote),
                        ],
                        "(|num_extra_quotes, arg| (FunctionParameterListType::InferrableList(num_extra_quotes), arg))",
                    ),
                ],
            ),
            EnclosedArgListParen => ruleset(
                "(FunctionParameterListType, WithLocation<Expr>)",
                [
                    rule([ term(KwRequires).discard(), nonterm(Expression).with_location(), term(SymCloseParen).discard() ], "(move |arg_expr| (FunctionParameterListType::RequiresList, arg_expr))"),
                    rule([ term(SymCloseParen).with_location() ], "enclosed_arg_list_paren_empty"),
                    rule([ nonterm(Expression).with_location(), term(SymCloseParen).discard() ], "(move |arg_expr| (FunctionParameterListType::NormalList, arg_expr))"),
                ],
            ),
            EnclosedArgListQuote => ruleset(
                "WithLocation<Expr>",
                [
                    rule([ term(SymCloseParen).with_location() ], "(|close_bracket: WithLocation<_>| WithLocation::new(expr_tuple(empty_seq()), close_bracket.location))"),
                    rule([ nonterm(Expression).with_location(), term(SymCloseParen).discard() ], "identity"),
                ],
            ),
            EnclosedArgListExtraQuotes => ruleset(
                "BigUint",
                [
                    rule([], "(|| BigUint::ZERO)"),
                    rule(
                        [
                            term(SymSingleQuote).discard(),
                            nonterm(EnclosedArgListExtraQuotes),
                        ],
                        "(|n| n + 1u32)",
                    ),
                ],
            ),
            RecordLiteralFieldsWithBlockEnd => ruleset(
                "WithLocation<VecDeque<WithLocation<RecordFieldLiteral>>>",
                [
                    rule([ nonterm(RecordLiteralFields).with_location(), term(SymCloseCurly).discard() ], "identity"),
                ],
            ),
            RecordLiteralFields => ruleset(
                "VecDeque<WithLocation<RecordFieldLiteral>>",
                [
                    rule([], "empty_seq"),
                    rule([ nonterm(StatementSeparator).discard(), nonterm(RecordLiteralFields) ], "identity" ),
                    rule([ nonterm(RecordLiteralField).with_location(), nonterm(StatementSeparator).discard(), nonterm(RecordLiteralFields) ], "(move |h, t| prepend(h, t))"),
                    rule([ nonterm(RecordLiteralField).with_location() ], "seq1"),
                ],
            ),
            RecordLiteralField => ruleset(
                "RecordFieldLiteral",
                [
                    rule(
                        [
                            nonterm(Identifier).with_location(),
                            nonterm(NewLines).discard(),
                            term(SymColon).discard(),
                            nonterm(NewLines).discard(),
                            nonterm(Expression).with_location(),
                        ],
                        "record_field_literal"),
                ],
            ),

            UnaryExpr => ruleset(
                "Expr",
                [
                    rule([ nonterm(CurryCallExpr(ParenAllowedState::Allowed)) ], "identity"),
                    rule([ nonterm(TypeExpr) ], "identity"),
                    unary_operator_expr_rule(OpBitNot, "BitNot"),
                    unary_operator_expr_rule(OpLogicalNot, "LogicalNot"),
                    unary_operator_expr_rule(OpPlus, "Plus"),
                    unary_operator_expr_rule(OpMinus, "Minus"),
                    expr_error(),
                ],
            ),
            TypeExpr => ruleset(
                "Expr",
                [
                    rule([ term(KwType).discard() ], "(|| expr_type())"),
                    rule([ term(KwBigType).discard(), term(SymOpenBracket).discard(), term(IntToken), term(SymCloseBracket).discard() ], "expr_big_type_token"),
                ],
            ),
            MultiplicativeExpr => ruleset(
                "Expr",
                [
                    rule([ nonterm(UnaryExpr) ], "identity"),
                    binary_operator_expr_rule(MultiplicativeExpr, OpStar, UnaryExpr, "Mul"),
                    binary_operator_expr_rule(MultiplicativeExpr, OpMul, UnaryExpr, "Mul"),
                    binary_operator_expr_rule(MultiplicativeExpr, OpSlash, UnaryExpr, "Div"),
                    binary_operator_expr_rule(MultiplicativeExpr, OpDiv, UnaryExpr, "Div"),
                    expr_error(),
                ],
            ),
            AdditiveExpr => ruleset(
                "Expr",
                [
                    rule([ nonterm(MultiplicativeExpr) ], "identity"),
                    binary_operator_expr_rule(AdditiveExpr, OpPlus, MultiplicativeExpr, "Plus"),
                    binary_operator_expr_rule(AdditiveExpr, OpMinus, MultiplicativeExpr, "Minus"),
                    binary_operator_expr_rule(AdditiveExpr, OpConcat, MultiplicativeExpr, "Concat"),
                    expr_error(),
                ],
            ),
            ShiftExpr => ruleset(
                "Expr",
                [
                    rule([ nonterm(AdditiveExpr) ], "identity"),
                    binary_operator_expr_rule(ShiftExpr, OpShiftLeft, AdditiveExpr, "ShiftLeft"),
                    binary_operator_expr_rule(ShiftExpr, OpShiftRight, AdditiveExpr, "ShiftRight"),
                    expr_error(),
                ],
            ),
            BitwiseAndExpr => ruleset(
                "Expr",
                [
                    rule([ nonterm(ShiftExpr) ], "identity"),
                    binary_operator_expr_rule(BitwiseAndExpr, OpBitAnd, ShiftExpr, "BitAnd"),
                    expr_error(),
                ],
            ),
            BitwiseXorExpr => ruleset(
                "Expr",
                [
                    rule([ nonterm(BitwiseAndExpr) ], "identity"),
                    binary_operator_expr_rule(BitwiseXorExpr, OpBitXor, BitwiseAndExpr, "BitXOr"),
                    expr_error(),
                ],
            ),
            BitwiseOrExpr => ruleset(
                "Expr",
                [
                    rule([ nonterm(BitwiseXorExpr) ], "identity"),
                    binary_operator_expr_rule(BitwiseOrExpr, OpBitOr, BitwiseXorExpr, "BitOr"),
                    expr_error(),
                ],
            ),
            FunctionTypeExpr => ruleset(
                "Expr",
                [
                    rule([ nonterm(BitwiseOrExpr).with_location() ], "with_location_value"),
                    rule(
                        [
                            term(KwFn).discard(),
                            nonterm(BitwiseOrExpr).with_location(),
                            term(OpArrow).discard(),
                            nonterm(FunctionTypeExprRest).with_location(),
                        ],
                        "expr_function_type"
                    ),
                    expr_error(),
                ],
            ),
            FunctionTypeExprRest => ruleset(
                "Expr",
                [
                    rule(
                        [
                            nonterm(BitwiseOrExpr).with_location(),
                        ],
                        "with_location_value"
                    ),
                    rule(
                        [
                            nonterm(BitwiseOrExpr).with_location(),
                            term(OpArrow).discard(),
                            nonterm(FunctionTypeExprRest).with_location(),
                        ],
                        "expr_function_type"
                    ),
                ],
            ),
            RelationalExpr => ruleset(
                "Expr",
                [
                    rule([ nonterm(FunctionTypeExpr) ], "identity"),
                    binary_operator_expr_rule(RelationalExpr, OpLessThan, FunctionTypeExpr, "LessThan"),
                    binary_operator_expr_rule(RelationalExpr, OpLessThanEq, FunctionTypeExpr, "LessThanEq"),
                    binary_operator_expr_rule(RelationalExpr, OpGreaterThan, FunctionTypeExpr, "GreaterThan"),
                    binary_operator_expr_rule(RelationalExpr, OpGreaterThanEq, FunctionTypeExpr, "GreaterThanEq"),
                    expr_error(),
                ],
            ),
            EqualityExpr => ruleset(
                "Expr",
                [
                    rule([ nonterm(RelationalExpr) ], "identity"),
                    binary_operator_expr_rule(EqualityExpr, OpEquals, RelationalExpr, "Equal"),
                    binary_operator_expr_rule(EqualityExpr, OpNotEquals, RelationalExpr, "NotEqual"),
                    expr_error(),
                ],
            ),
            LogicalAndExpr => ruleset(
                "Expr",
                [
                    rule([ nonterm(EqualityExpr) ], "identity"),
                    binary_operator_expr_rule(LogicalAndExpr, OpLogicalAnd, EqualityExpr, "LogicalAnd"),
                    expr_error(),
                ],
            ),
            LogicalOrExpr => ruleset(
                "Expr",
                [
                    rule([ nonterm(LogicalAndExpr) ], "identity"),
                    binary_operator_expr_rule(LogicalOrExpr, OpLogicalOr, LogicalAndExpr, "LogicalOr"),
                    expr_error(),
                ],
            ),
            PropConjunctionExpr => ruleset(
                "Expr",
                [
                    rule([ nonterm(LogicalOrExpr) ], "identity"),
                    binary_operator_expr_rule(PropConjunctionExpr, OpPropConjunction, LogicalOrExpr, "PropConjunction"),
                    expr_error(),
                ],
            ),
            PropDisjunctionExpr => ruleset(
                "Expr",
                [
                    rule([ nonterm(PropConjunctionExpr) ], "identity"),
                    binary_operator_expr_rule(PropDisjunctionExpr, OpPropDisjunction, PropConjunctionExpr, "PropDisjunction"),
                    expr_error(),
                ],
            ),
            PropEqualityExpr => ruleset(
                "Expr",
                [
                    rule([ nonterm(PropDisjunctionExpr) ], "identity"),
                    binary_operator_expr_rule(PropEqualityExpr, OpPropEqual, PropDisjunctionExpr, "PropEqual"),
                    expr_error(),
                ],
            ),
            AsExpr => ruleset(
                "Expr",
                [
                    rule([ nonterm(PropEqualityExpr).with_location() ], "with_location_value"),
                    rule([ nonterm(AsExpr).with_location(), term(KwAs).discard(), nonterm(PropEqualityExpr).with_location() ], "expr_as"),
                    expr_error(),
                ],
            ),
            IsExpr => ruleset(
                "Expr",
                [
                    rule([ nonterm(AsExpr).with_location() ], "with_location_value"),
                    rule([ nonterm(AsExpr).with_location(), term(KwIs).discard(), nonterm(NonTuplePattern).with_location() ], "expr_is"),
                    expr_error(),
                ],
            ),
            RaiseExpr => ruleset(
                "Expr",
                [
                    rule([ nonterm(IsExpr) ], "identity"),
                    rule([ term(KwRaise).discard(), nonterm(IsExpr).with_location() ], "expr_raise"),
                    expr_error(),
                ],
            ),
            ClosureExpr => ruleset(
                "Expr",
                [
                    rule([ nonterm(RaiseExpr) ], "identity"),
                    rule(
                        [
                            term(SymPipe).discard(),
                            nonterm(IdentifierOptional),
                            term(SymPipe).discard(),
                            nonterm(ClosureExpr).with_location(),
                        ],
                        "expr_function_literal",
                    ),
                    expr_error(),
                ],
            ),
            TupleExpr => ruleset(
                "Expr",
                [
                    rule([ nonterm(ClosureExpr).with_location() ], "with_location_value"),
                    rule([ nonterm(ClosureExpr).with_location(), term(SymComma).discard(), nonterm(TupleExprRest) ], "(move |h, t| expr_tuple(prepend(h, t)))"),
                    expr_error(),
                ],
            ),
            TupleExprRest => ruleset(
                "VecDeque<WithLocation<Expr>>",
                [
                    rule([ nonterm(ClosureExpr).with_location() ], "seq1"),
                    rule([ nonterm(ClosureExpr).with_location(), term(SymComma).discard(), nonterm(TupleExprRest) ], "(move |h, t| prepend(h, t))"),
                ],
            ),
            AssignExpr => ruleset(
                "Expr",
                [
                    rule([ nonterm(TupleExpr).with_location() ], "with_location_value"),
                    rule([ nonterm(TupleExpr).with_location(), term(OpAssign).with_location(), nonterm(TupleExpr).with_location() ], "(move |left, op: WithLocation<_>, right| binary_op(left, WithLocation::new(BinaryOperator::Assign, op.location), right))"),
                ],
            ),
            AssertExpr => ruleset(
                "Expr",
                [
                    rule([ term(KwAssert).discard(), nonterm(NewLines).discard(), nonterm(Expression).with_location() ], "expr_assert"),
                ]
            ),
            BreakExpr => ruleset(
                "Expr",
                [
                    rule([ term(KwBreak).discard(), nonterm(LoopLabel) ], "expr_break_no_arg"),
                    rule([ term(KwBreak).discard(), nonterm(LoopLabel), nonterm(TupleExpr).with_location() ], "expr_break"),
                ]
            ),
            NextExpr => ruleset(
                "Expr",
                [
                    rule([ term(KwNext).discard(), nonterm(LoopLabel) ], "expr_next"),
                ]
            ),
            RedoExpr => ruleset(
                "Expr",
                [
                    rule([ term(KwRedo).discard(), nonterm(LoopLabel) ], "expr_redo"),
                ]
            ),
            RetryExpr => ruleset(
                "Expr",
                [
                    rule([ term(KwRetry).discard(), nonterm(LoopLabel) ], "expr_retry"),
                ]
            ),
            ControlExpr => ruleset(
                "Expr",
                [
                    rule([ nonterm(AssertExpr) ], "identity"),
                    rule([ nonterm(BreakExpr) ], "identity"),
                    rule([ nonterm(NextExpr) ], "identity"),
                    rule([ nonterm(RedoExpr) ], "identity"),
                    rule([ nonterm(RetryExpr) ], "identity"),
                ]
            ),
            Expression => ruleset(
                "Expr",
                [
                    rule([ nonterm(ControlExpr) ], "identity"),
                    rule([ nonterm(AssignExpr) ], "identity"),
                    expr_error(),
                ],
            ),
            TypeBinding => ruleset(
                "Expr",
                [
                    rule([ nonterm(FunctionTypeExpr) ], "identity"),
                ],
            ),


            Pattern => ruleset(
                "Pattern",
                [
                    rule([ nonterm(TuplePattern) ], "identity"),
                ]
            ),
            SimplePattern(paren_allowed) => {
                let mut rules = vec![
                    rule([ term(KwUnderscore).discard() ], "(|| pattern_discard())"),

                    rule([ nonterm(StringExpr) ], "pattern_string"),
                    rule([ term(IntToken) ], "pattern_int_token"),
                    rule([ term(KwTrue).discard() ], "(|| pattern_bool(true))"),
                    rule([ term(KwFalse).discard() ], "(|| pattern_bool(false))"),
                ];

                if paren_allowed == ParenAllowedState::Allowed {
                    rules.push(rule(
                        [ term(SymOpenParen).discard(), nonterm(Pattern), term(SymCloseParen).discard() ],
                        "identity",
                    ));
                }

                ruleset("Pattern", rules)
            },
            TuplePattern => ruleset(
                "Pattern",
                [
                    rule([], "(|| pattern_tuple(empty_seq()))"),
                    rule([ nonterm(NonTuplePattern).with_location() ], "with_location_value"),
                    rule([ nonterm(NonTuplePattern).with_location(), term(SymComma).discard(), nonterm(NonEmptyTuplePattern) ], "(move |h, t| pattern_tuple(prepend(h, t)))"),
                ],
            ),
            NonEmptyTuplePattern => ruleset(
                "VecDeque<WithLocation<Pattern>>",
                [
                    rule([], "empty_seq"),
                    rule([ nonterm(NonTuplePattern).with_location() ], "seq1"),
                    rule(
                        [ nonterm(NonTuplePattern).with_location(), term(SymComma).discard(), nonterm(NonEmptyTuplePattern) ],
                        "(move |h, t| prepend(h, t))",
                    )
                ],
            ),
            NonTuplePattern => ruleset(
                "Pattern",
                [
                    rule([ nonterm(SimplePattern(ParenAllowedState::Allowed)) ], "identity"),
                    rule(
                        [
                            nonterm(PatternPath).with_location(),
                            nonterm(ConstructorArgsPattern),
                        ],
                        "pattern_constructor",
                    ),
                    rule(
                        [
                            nonterm(MutSpec),
                            nonterm(Identifier).with_location(),
                            term(KwAs).discard(),
                            nonterm(SimplePattern(ParenAllowedState::Allowed)).with_location()
                        ],
                        "(move |mut_spec, id, pattern| pattern_binding(mut_spec, id, pattern))",
                    ),
                    rule(
                        [
                            nonterm(MutSpec),
                            nonterm(Identifier).with_location(),
                        ],
                        "pattern_binding_discard",
                    ),
                ],
            ),
            PatternPath => ruleset(
                "PatternPath",
                [
                    rule(
                        [ nonterm(Identifier).with_location() ],
                        "pattern_path_base",
                    ),
                    rule(
                        [ nonterm(PatternPath).with_location(), term(SymDot).discard(), nonterm(Identifier).with_location() ],
                        "pattern_path_member",
                    ),
                ],
            ),
            ConstructorArgsPattern => ruleset(
                "VecDeque<PatternArgument>",
                [
                    rule([], "empty_seq"),
                    rule(
                        [
                            nonterm(ConstructorArgPattern),
                            nonterm(ConstructorArgsPattern),
                        ],
                        "(move |h, t| prepend(h, t))",
                    ),

                ],
            ),
            ConstructorArgPattern => ruleset(
                "PatternArgument",
                [
                    rule(
                        [ nonterm(PatternPath).with_location() ],
                        "pattern_argument_from_path",
                    ),
                    rule(
                        [ nonterm(SimplePattern(ParenAllowedState::NotAllowed)).with_location() ],
                        "(move |pattern| pattern_argument(FunctionParameterListType::NormalList, pattern))",
                    ),
                    rule(
                        [
                            term(SymOpenParen).discard(),
                            nonterm(Pattern).with_location(),
                            term(SymCloseParen).discard(),
                        ],
                        "(move |pattern| pattern_argument(FunctionParameterListType::NormalList, pattern))",
                    ),
                    rule(
                        [
                            term(SymOpenParen).discard(),
                            term(KwRequires).discard(),
                            nonterm(Pattern).with_location(),
                            term(SymCloseParen).discard(),
                        ],
                        "(move |pattern| pattern_argument(FunctionParameterListType::RequiresList, pattern))",
                    ),
                    rule(
                        [
                            term(SymSingleQuote).discard(),
                            nonterm(EnclosedArgListExtraQuotes),
                            term(SymOpenParen).discard(),
                            nonterm(Pattern).with_location(),
                            term(SymCloseParen).discard(),
                        ],
                        "(move |num_extra_quotes, pattern| pattern_argument(FunctionParameterListType::InferrableList(num_extra_quotes), pattern))",
                    ),
                ],
            ),


            Modifiers => ruleset(
                "Vec<WithLocation<Modifier>>",
                [
                    rule([], "(|| Vec::new())"),
                    rule([ nonterm(Modifier).with_location(), nonterm(Modifiers1) ], "(move |h, t| vec_deque_to_vec(prepend(h, t)))" ),
                ],
            ),
            Modifiers1 => ruleset(
                "VecDeque<WithLocation<Modifier>>",
                [
                    rule([], "empty_seq"),
                    rule([ term(NewLine).discard(), nonterm(Modifiers1) ], "identity"),
                    rule([ nonterm(Modifier).with_location(), nonterm(Modifiers1) ], "(move |h, t| prepend(h, t))" ),
                ],
            ),
            Modifier => ruleset(
                "Modifier",
                [
                    rule([ term(KwPublic).discard() ], "(|| Modifier::Public)"),
                    rule([ term(KwPrivate).discard() ], "(|| Modifier::Private)"),
                    rule([ term(KwProtected).discard() ], "(|| Modifier::Protected)"),
                    rule([ term(KwInternal).discard() ], "(|| Modifier::Internal)"),
                    rule([ term(KwErased).discard() ], "(|| Modifier::Erased)"),
                    rule([ term(KwToken).discard() ], "(|| Modifier::Token)"),
                    rule([ term(KwWitness).discard() ], "(|| Modifier::Witness)"),
                    rule([ term(KwInline).discard() ], "(|| Modifier::Inline)"),
                    rule([ term(KwFinal).discard() ], "(|| Modifier::Final)"),
                    rule([ term(KwVirtual).discard() ], "(|| Modifier::Virtual)"),
                    rule([ term(KwOverride).discard() ], "(|| Modifier::Override)"),
                ],
            ),

            VariableDeclarationRest => ruleset(
                "Box<dyn FnOnce(Vec<WithLocation<Modifier>>) -> Stmt>",
                [
                    rule(
                        [
                            term(KwLet).discard(),
                            nonterm(VariableDeclarationBinding),
                            nonterm(VariableDeclarationValue).with_location(),
                        ],
                        "variable_declaration_rest_builder"
                    ),
                ],
            ),
            VariableDeclarationBinding => ruleset(
                "(bool, Option<Identifier>, Option<WithLocation<Expr>>)",
                [
                    rule(
                        [
                            nonterm(VariableMutSpec),
                            nonterm(IdentifierOptional),
                            nonterm(VariableDeclarationTypeSpec),
                            term(OpEquals).discard(),
                            nonterm(NewLines).discard(),
                        ],
                        "(move |is_mutable, id, type_annotation| (is_mutable, id, type_annotation))",
                    ),
                ]
            ).lex_mode("LexerMode::SkipNewLines"),
            VariableMutSpec => ruleset(
                "bool",
                [
                    rule([], "(|| false)"),
                    rule([ nonterm(MutSpec) ], "identity"),
                ]
            ),
            MutSpec => ruleset(
                "bool",
                [
                    rule([ term(KwVal).discard() ], "(|| false)"),
                    rule([ term(KwMut).discard() ], "(|| true)"),
                ]
            ),
            VariableDeclarationTypeSpec => ruleset(
                "Option<WithLocation<Expr>>",
                [
                    rule([], "(|| None)"),
                    rule([ term(SymColon).discard(), nonterm(TypeBinding).with_location() ], "Some"),
                ]
            ),
            VariableDeclarationValue => ruleset(
                "Expr",
                [
                    rule([ nonterm(Expression) ], "identity"),
                ],
            ).lex_mode("LexerMode::Normal"),

            MethodOrFunctionDeclarationStmtRest => ruleset(
                "Box<dyn FnOnce((Vec<WithLocation<Modifier>>, bool)) -> DeclarationStmt>",
                [
                    rule(
                        [
                            nonterm(Identifier).with_location(),
                            term(SymDot).discard(),
                            nonterm(MethodName).with_location(),
                            nonterm(MethodParameters),
                            term(SymColon).discard(),
                            nonterm(MethodReturnType).with_location(),
                            nonterm(MethodBody),
                        ],
                        "method_declaration_stmt_rest_from_named_instance"
                    ),
                    rule(
                        [
                            term(KwUnderscore).with_location(),
                            term(SymDot).discard(),
                            nonterm(MethodName).with_location(),
                            nonterm(MethodParameters),
                            term(SymColon).discard(),
                            nonterm(MethodReturnType).with_location(),
                            nonterm(MethodBody),
                        ],
                        "method_declaration_stmt_rest_from_discard_instance"
                    ),
                    rule(
                        [
                            nonterm(Identifier).with_location(),
                            nonterm(MethodParameters),
                            term(SymColon).discard(),
                            nonterm(MethodReturnType).with_location(),
                            nonterm(FunctionBody),
                        ],
                        "method_declaration_stmt_rest_function"
                    ),
                ],
            ).lex_mode("LexerMode::SkipNewLines"),
            MethodPurity => ruleset(
                "bool",
                [
                    rule([ term(KwDef).discard() ], "(|| true)"),
                    rule([ term(KwProc).discard() ], "(|| false)"),
                ],
            ),
            MethodParameters => ruleset(
                "VecDeque<WithLocation<FunctionParameterList>>",
                [
                    rule([], "empty_seq"),
                    rule([ nonterm(MethodParameterList).with_location(), nonterm(MethodParameters) ], "(move |h, t| prepend(h, t))"),
                ],
            ),
            MethodParameterList => ruleset(
                "FunctionParameterList",
                [
                    rule(
                        [
                            term(SymOpenParen).discard(),
                            term(KwRequires).discard(),
                            nonterm(MethodParameterListModifiers),
                            nonterm(MethodParameterListContents),
                            term(SymCloseParen).discard(),
                        ],
                        "(move |modifiers, data| { let (parameters, has_trailing_comma) = data; function_parameter_list(FunctionParameterListType::RequiresList, modifiers, parameters, has_trailing_comma) })"
                    ),
                    rule(
                        [
                            term(SymOpenParen).discard(),
                            nonterm(MethodParameterListModifiers),
                            nonterm(MethodParameterListContents),
                            term(SymCloseParen).discard(),
                        ],
                        "(move |modifiers, data| { let (parameters, has_trailing_comma) = data; function_parameter_list(FunctionParameterListType::NormalList, modifiers, parameters, has_trailing_comma) })"
                    ),
                    rule(
                        [
                            term(SymSingleQuote).discard(),
                            nonterm(EnclosedArgListExtraQuotes),
                            term(SymOpenBracket).discard(),
                            nonterm(MethodParameterListModifiers),
                            nonterm(MethodParameterListContents),
                            term(SymCloseBracket).discard(),
                        ],
                        "(move |num_extra_quotes, modifiers, data| { let (parameters, has_trailing_comma) = data; function_parameter_list(FunctionParameterListType::InferrableList(num_extra_quotes), modifiers, parameters, has_trailing_comma) })"
                    ),
                ],
            ).lex_mode("LexerMode::SkipNewLines"),
            MethodParameterListModifiers => ruleset(
                "VecDeque<WithLocation<Modifier>>",
                [
                    rule([], "empty_seq"),
                    rule([ nonterm(MethodParameterListModifier).with_location(), nonterm(MethodParameterListModifiers) ], "(move |h, t| prepend(h, t))")
                ],
            ),
            MethodParameterListModifier => ruleset(
                "Modifier",
                [
                    rule([ term(KwErased).discard() ], "(|| Modifier::Erased)"),
                    rule([ term(KwToken).discard() ], "(|| Modifier::Token)"),
                ],
            ),
            MethodParameterListContents => ruleset(
                "(VecDeque<WithLocation<FunctionParameter>>, bool)",
                [
                    rule([], "(|| (empty_seq(), false))"),
                    rule([ nonterm(MethodParameter).with_location(), nonterm(MethodParameterListContents1) ], "(move |h, tail_info| { let (t, has_trailing_comma) = tail_info; (prepend(h, t), has_trailing_comma) })"),
                ],
            ),
            MethodParameterListContents1 => ruleset(
                "(VecDeque<WithLocation<FunctionParameter>>, bool)",
                [
                    rule([], "(|| (empty_seq(), false))"),
                    rule([ term(SymComma).discard() ], "(|| (empty_seq(), true))"),
                    rule(
                        [
                            term(SymComma).discard(),
                            nonterm(MethodParameter).with_location(),
                            nonterm(MethodParameterListContents1),
                        ],
                        "(move |h, tail_info| { let (t, has_trailing_comma) = tail_info; (prepend(h, t), has_trailing_comma) })"
                    ),
                ],
            ),
            MethodParameter => ruleset(
                "FunctionParameter",
                [
                    rule(
                        [
                            nonterm(Identifier),
                            term(SymColon).discard(),
                            nonterm(TypeBinding).with_location(),
                        ],
                        "(move |name, param_type| function_parameter(param_type, name))"
                    ),
                ],
            ),
            MethodReturnType => ruleset(
                "ReturnTypeSpecifier",
                [
                    rule([ nonterm(TypeBinding).with_location(), nonterm(MethodEnsuresClause) ], "return_type_specifier"),
                ],
            ),
            MethodEnsuresClause => ruleset(
                "VecDeque<WithLocation<Expr>>",
                [
                    rule([], "empty_seq"),
                    rule([ term(KwEnsures).discard(), nonterm(TypeBinding).with_location(), nonterm(MethodEnsuresClause) ], "(move |h, t| prepend(h, t))"),
                ],
            ),
            MethodBody => ruleset(
                "Option<FunctionBody>",
                [
                    rule([ term(KwDo).discard(), nonterm(BlockBodyWithEnd) ], "(move |e| Some(function_body_expr_body(e)))"),
                    rule([ term(OpEquals).discard(), nonterm(NewLines).discard(), nonterm(Expression).with_location() ], "(move |e| Some(function_body_expr_body(e)))"),
                    rule([ term(OpEquals).discard(), nonterm(NewLines).discard(), nonterm(ExternFunctionBody) ], "Some"),
                    rule([ term(OpEquals).discard(), nonterm(NewLines).discard(), term(KwAbstract).discard() ], "(|| None)"),
                ],
            ).lex_mode("LexerMode::Normal"),
            FunctionBody => ruleset(
                "FunctionBody",
                [
                    rule([ term(KwDo).discard(), nonterm(BlockBodyWithEnd) ], "function_body_expr_body"),
                    rule([ term(OpEquals).discard(), nonterm(NewLines).discard(), nonterm(Expression).with_location() ], "function_body_expr_body"),
                    rule([ term(OpEquals).discard(), nonterm(NewLines).discard(), nonterm(ExternFunctionBody) ], "identity"),
                ],
            ).lex_mode("LexerMode::Normal"),
            ExternFunctionBody => ruleset(
                "FunctionBody",
                [
                    rule([ term(KwExtern).discard(), term(IdentifierToken).with_location() ], "function_body_extern_body_token"),
                ]
            ),
            RecordDeclarationStmtRest => ruleset(
                "Box<dyn FnOnce(Vec<WithLocation<Modifier>>) -> RecordDeclarationStmt>",
                [
                    rule(
                        [
                            term(KwRecord).discard(),
                            nonterm(Identifier).with_location(),
                            nonterm(MethodParameters),
                            nonterm(TypeDeclarationTypeAnnotation),
                            nonterm(StatementSeparator).discard(),
                            nonterm(RecordBody),
                            term(KwEnd).discard(),
                        ],
                        "record_declaration_stmt_rest_builder"
                    ),
                ],
            ),
            RecordBody => ruleset(
                "VecDeque<WithLocation<RecordBodyStmt>>",
                [
                    rule([ nonterm(RecordBodyStmt).with_location() ], "seq1"),
                    rule([ nonterm(RecordBodyStmt).with_location(), nonterm(StatementSeparator).discard(), nonterm(RecordBody) ], "(move |h, t| prepend(h, t))"),
                    rule([ nonterm(StatementSeparator).discard(), nonterm(RecordBody) ], "identity"),
                    rule([], "empty_seq"),
                ],
            ),
            RecordBodyStmt => ruleset(
                "RecordBodyStmt",
                [
                    rule([ nonterm(RecordField) ], "record_body_stmt_record_field"),
                    rule([ nonterm(Modifiers), nonterm(MethodPurity), nonterm(MethodOrFunctionDeclarationStmtRest) ], "record_body_stmt_from_declaration_builder"),
                ],
            ),
            RecordField => ruleset(
                "RecordField",
                [
                    rule(
                        [
                            nonterm(VariableMutSpec),
                            nonterm(Identifier).with_location(),
                            nonterm(NewLines).discard(),
                            term(SymColon).discard(),
                            nonterm(NewLines).discard(),
                            nonterm(TypeBinding).with_location(),
                        ],
                        "record_field"
                    )
                ],
            ),
            EnumDeclarationStmtRest => ruleset(
                "Box<dyn FnOnce(Vec<WithLocation<Modifier>>) -> Stmt>",
                [
                    rule(
                        [
                            term(KwEnum).discard(),
                            nonterm(Identifier).with_location(),
                            nonterm(MethodParameters),
                            nonterm(TypeDeclarationTypeAnnotation),
                            nonterm(StatementSeparator).discard(),
                            nonterm(EnumBody),
                            term(KwEnd).discard(),
                        ],
                        "enum_declaration_stmt_rest_builder"
                    ),
                ],
            ),
            EnumBody => ruleset(
                "VecDeque<WithLocation<EnumBodyStmt>>",
                [
                    rule([ nonterm(EnumBodyStmt).with_location() ], "seq1"),
                    rule([ nonterm(EnumBodyStmt).with_location(), nonterm(StatementSeparator).discard(), nonterm(EnumBody) ], "(move |h, t| prepend(h, t))"),
                    rule([ nonterm(StatementSeparator).discard(), nonterm(EnumBody) ], "identity"),
                    rule([], "empty_seq"),
                ],
            ),
            EnumBodyStmt => ruleset(
                "EnumBodyStmt",
                [
                    rule([ nonterm(Modifiers), nonterm(EnumConstructorVariant) ], "enum_body_stmt_from_variant_builder"),
                    rule([ nonterm(Modifiers), nonterm(RecordDeclarationStmtRest) ], "enum_body_stmt_from_record_builder"),
                    rule([ nonterm(Modifiers), nonterm(MethodPurity), nonterm(MethodOrFunctionDeclarationStmtRest) ], "enum_body_stmt_from_declaration_builder"),
                ],
            ),
            EnumConstructorVariant => ruleset(
                "Box<dyn FnOnce(Vec<WithLocation<Modifier>>) -> EnumBodyStmt>",
                [
                    rule(
                        [
                            nonterm(Identifier).with_location(),
                            nonterm(MethodParameters),
                            nonterm(TypeDeclarationTypeAnnotation),
                        ],
                        "enum_constructor_variant_builder",
                    ),
                ],
            ),
            TraitDeclarationStmtRest => ruleset(
                "Box<dyn FnOnce(Vec<WithLocation<Modifier>>) -> Stmt>",
                [
                    rule(
                        [
                            term(KwTrait).discard(),
                            nonterm(Identifier).with_location(),
                            nonterm(MethodParameters),
                            nonterm(TypeDeclarationTypeAnnotation),
                            nonterm(StatementSeparator).discard(),
                            nonterm(TraitBody),
                            term(KwEnd).discard(),
                        ],
                        "trait_declaration_stmt_rest_builder"
                    ),
                ],
            ),
            TraitBody => ruleset(
                "VecDeque<WithLocation<TraitBodyStmt>>",
                [
                    rule([ nonterm(TraitBodyStmt).with_location() ], "seq1"),
                    rule([ nonterm(TraitBodyStmt).with_location(), nonterm(StatementSeparator).discard(), nonterm(TraitBody) ], "(move |h, t| prepend(h, t))"),
                    rule([ nonterm(StatementSeparator).discard(), nonterm(TraitBody) ], "identity"),
                    rule([], "empty_seq"),
                ],
            ),
            TraitBodyStmt => ruleset(
                "TraitBodyStmt",
                [
                    rule([ nonterm(Modifiers), nonterm(MethodPurity), nonterm(MethodOrFunctionDeclarationStmtRest) ], "trait_body_stmt_from_declaration_builder"),
                ],
            ),
            NewTraitObjectBody => ruleset(
                "VecDeque<WithLocation<NewTraitObjectBodyStmt>>",
                [
                    rule([ nonterm(NewTraitObjectBodyStmt).with_location() ], "seq1"),
                    rule([ nonterm(NewTraitObjectBodyStmt).with_location(), nonterm(StatementSeparator).discard(), nonterm(NewTraitObjectBody) ], "(move |h, t| prepend(h, t))"),
                    rule([ nonterm(StatementSeparator).discard(), nonterm(NewTraitObjectBody) ], "identity"),
                    rule([], "empty_seq"),
                ],
            ),
            NewTraitObjectBodyStmt => ruleset(
                "NewTraitObjectBodyStmt",
                [
                    rule([ nonterm(Modifiers), nonterm(MethodPurity), nonterm(MethodOrFunctionDeclarationStmtRest) ], "new_trait_object_body_stmt_from_declaration_builder"),
                ],
            ),
            TypeDeclarationTypeAnnotation => ruleset(
                "Option<WithLocation<Expr>>",
                [
                    rule([], "(|| None)"),
                    rule([ term(SymColon).discard(), nonterm(NewLines).discard(), nonterm(TypeBinding).with_location() ], "Some"),
                ],
            ),
            InstanceDeclarationStmtRest => ruleset(
                "Box<dyn FnOnce(Vec<WithLocation<Modifier>>) -> Stmt>",
                [
                    rule(
                        [
                            term(KwInstance).discard(),
                            nonterm(Identifier).with_location(),
                            nonterm(MethodParameters),
                            nonterm(TypeDeclarationTypeAnnotation),
                            nonterm(StatementSeparator).discard(),
                            nonterm(TraitBody),
                            term(KwEnd).discard(),
                        ],
                        "instance_declaration_stmt_rest_builder"
                    ),
                ],
            ),


            ImportStmt => ruleset(
                "ImportStmt",
                [
                    rule([ term(KwImport).discard(), nonterm(ImportPath) ], "identity"),
                ],
            ),

            TubeName => ruleset(
                "NonEmptyVec<String>",
                [
                    rule([ term(IdentifierToken) ], "tube_name_single"),
                    rule([ term(IdentifierToken), term(SymDot).discard(), nonterm(TubeName) ], "tube_name_prepend"),
                ],
            ),

            ExportStmt => ruleset(
                "ExportStmt",
                [
                    rule([ term(KwExport).discard(), nonterm(ImportPath).with_location() ], "export_stmt"),
                ],
            ),

            ImportPath => ruleset(
                "ImportStmt",
                [
                    rule([ nonterm(TubeName), term(SymColonColon).discard(), nonterm(ImportPathSegment) ], "import_stmt_tube"),
                    rule([ term(SymColonColon).discard(), nonterm(ImportPathSegment) ], "import_stmt_absolute"),
                    rule([ term(KwAs).discard(), term(SymColonColon).discard(), nonterm(ImportPathRelativePrefix), nonterm(ImportPathSegment) ], "import_stmt_relative"),
                ],
            ),

            ImportPathRelativePrefix => ruleset(
                "usize",
                [
                    rule([ term(OpDotDot).discard(), term(SymColonColon).discard(), nonterm(ImportPathRelativePrefix) ], "(move |n| n + 1)"),
                    rule([], "(|| 0)"),
                ],
            ),

            ImportPathSegment => ruleset(
                "ImportPathSegment",
                [
                    rule([ term(IdentifierToken).with_location() ], "import_path_segment_imported_token"),
                    rule([ nonterm(ComplexIdentifier).with_location() ], "import_path_segment_imported"),
                    rule([ term(IdentifierToken).with_location(), term(KwAs).discard(), nonterm(IdentifierOptional).with_location() ], "import_path_segment_renaming_token"),
                    rule([ nonterm(ComplexIdentifier).with_location(), term(KwAs).discard(), nonterm(IdentifierOptional).with_location() ], "import_path_segment_renaming"),
                    rule([ term(IdentifierToken).with_location(), term(SymColonColon).discard(), nonterm(ImportPathSegment) ], "import_path_segment_cons_token"),
                    rule([ term(SymOpenCurly).discard(), nonterm(ImportPathMulti), term(SymCloseCurly).discard() ], "import_path_segment_many"),
                    rule([ term(OpStar).with_location()], "import_path_segment_wildcard_token"),
                ],
            ),

            ImportPathMulti => ruleset(
                "VecDeque<ImportPathSegment>",
                [
                    rule([ nonterm(StatementSeparator).discard(), nonterm(ImportPathMulti) ], "identity"),
                    rule([ nonterm(ImportPathSegment) ], "seq1"),
                    rule([ nonterm(ImportPathSegment), nonterm(StatementSeparator).discard(), nonterm(ImportPathMulti) ], "(move |h, t| prepend(h, t))"),
                    rule([], "empty_seq"),
                ],
            ),

            Statement => ruleset(
                "Stmt",
                [
                    rule([ nonterm(Expression).with_location() ], "stmt_expr"),
                    rule([ nonterm(ImportStmt) ], "stmt_import"),
                    rule([ nonterm(ExportStmt) ], "stmt_export"),
                    rule([ nonterm(Modifiers), nonterm(VariableDeclarationRest) ], "statement_apply_builder"),
                    rule([ nonterm(Modifiers), nonterm(MethodPurity), nonterm(MethodOrFunctionDeclarationStmtRest) ], "statement_declaration_builder"),
                    rule([ nonterm(Modifiers), nonterm(RecordDeclarationStmtRest) ], "statement_record_builder"),
                    rule([ nonterm(Modifiers), nonterm(EnumDeclarationStmtRest) ], "statement_apply_builder"),
                    rule([ nonterm(Modifiers), nonterm(TraitDeclarationStmtRest) ], "statement_apply_builder"),
                    rule([ nonterm(Modifiers), nonterm(InstanceDeclarationStmtRest) ], "statement_apply_builder"),
                    stmt_error(),
                ],
            ),

            StatementList => ruleset(
                "VecDeque<WithLocation<Stmt>>",
                [
                    rule([ nonterm(Statement).with_location() ], "seq1"),
                    rule([ nonterm(Statement).with_location(), nonterm(StatementSeparator).discard(), nonterm(StatementList) ], "(move |h, t| prepend(h, t))"),
                    rule([ nonterm(StatementSeparator).discard(), nonterm(StatementList) ], "identity"),
                    rule([], "empty_seq"),
                ],
            ),

            BlockBodyWithEnd => ruleset(
                "WithLocation<Expr>",
                [
                    rule([ nonterm(BlockBody).with_location(), term(KwEnd).discard() ], "identity"),
                ],
            ).lex_mode("LexerMode::Normal"),

            BlockBody => ruleset(
                "Expr",
                [
                    rule([ nonterm(StatementList).with_location() ], "(move |body| expr_block(body, None))"),
                    rule([ nonterm(StatementList).with_location(), term(KwFinally).discard(), nonterm(StatementList).with_location() ], "(move |body, finally_body| expr_block(body, Some(finally_body)))"),
                ],
            ),

            ModuleDeclaration => ruleset(
                "VecDeque<String>",
                [
                    rule([ term(KwModule).discard(), nonterm(ModulePath) ], "identity"),
                ],
            ),

            ModulePath => ruleset(
                "VecDeque<String>",
                [
                    rule([ term(KwUnderscore).discard() ], "empty_seq"),
                    rule([ term(IdentifierToken), nonterm(ModulePath1) ], "module_path_prepend"),
                ],
            ),

            ModulePath1 => ruleset(
                "VecDeque<String>",
                [
                    rule([], "empty_seq"),
                    rule([ term(SymColonColon).discard(), term(IdentifierToken), nonterm(ModulePath1) ], "module_path_prepend"),
                ],
            ),
        }
    }
}

fn ruleset(
    rule_type: impl Into<String>,
    rules: impl Into<Vec<RuleInfo<ParserFactory>>>,
) -> RuleSetInfo<ParserFactory> {
    base_ruleset(rule_type, rules)
}

fn rule(
    symbols: impl Into<Vec<SymbolInfo<ParserFactory>>>,
    function: impl Into<String>,
) -> RuleInfo<ParserFactory> {
    let symbols = symbols.into();
    base_rule(symbols, function)
}

fn binary_operator_name_rule(token: Token, op: &'static str) -> RuleInfo<ParserFactory> {
    rule(
        [term(token).discard()],
        format!("(|| BinaryOperatorIdentifier::{op})"),
    )
}

fn unary_operator_name_rule(token: Token, op: &'static str) -> RuleInfo<ParserFactory> {
    rule(
        [term(token).discard()],
        format!("(|| UnaryOperatorIdentifier::{op})"),
    )
}

fn unary_operator_expr_rule(token: Token, op: &'static str) -> RuleInfo<ParserFactory> {
    rule(
        [
            term(token).with_location(),
            nonterm(UnaryExpr).with_location(),
        ],
        format!(
            "(move |op: WithLocation<_>, expr| unary_op(WithLocation::new(UnaryOperator::{op}, op.location), expr))"
        ),
    )
}

fn binary_operator_expr_rule(
    left_rule: Rule,
    token: Token,
    right_rule: Rule,
    op: &'static str,
) -> RuleInfo<ParserFactory> {
    rule(
        [
            nonterm(left_rule).with_location(),
            term(token).with_location(),
            nonterm(right_rule).with_location(),
        ],
        format!(
            "(move |left, op: WithLocation<_>, right| binary_op(left, WithLocation::new(BinaryOperator::{op}, op.location), right))"
        ),
    )
}

fn expr_error() -> RuleInfo<ParserFactory> {
    rule([error()], "(|| expr_error())")
}

fn stmt_error() -> RuleInfo<ParserFactory> {
    rule([error()], "stmt_error")
}

fn build_grammar() -> Grammar<ParserFactory> {
    Grammar::make(ParserFactory)
}

pub fn emit_rust_parser<W: Write>(w: &mut W) -> io::Result<()> {
    let grammar = build_grammar();

    let rust_settings = RustSettings {
        module: None,
        visibility: "pub ".to_owned(),
        impl_group:
            "impl <'a, L: TokenReader, ER: ErrorReporter<CompileError> + ?Sized> ArgonParser<'a, L, ER>"
                .to_owned(),
        token_type: "Token".to_owned(),
        result_type: "ParseResult".to_owned(),
        location_type: "WithLocation".to_owned(),
        location_ctor: "WithLocation::new".to_owned(),
        merge_locations_fn: "merge_locations".to_owned(),
        header_stmts: vec![],
    };

    emit_rust(w, grammar, &rust_settings)
}
