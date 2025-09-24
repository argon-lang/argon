use parse18_ll_gen::codegen::scala::{emit_scala, ScalaSettings};
use parse18_ll_gen::grammar::{nonterm, rule, ruleset, term, Grammar, GrammarFactory, GrammarTypes, RuleSetInfo};


#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, strum::Display)]
pub enum Token {
    // String lexing (interpolation-capable)
    #[strum(serialize = "Token.StringStart.type")]
    StringStart,
    #[strum(serialize = "Token.StringEnd.type")]
    StringEnd,
    #[strum(serialize = "Token.StringText")]
    StringText,
    #[strum(serialize = "Token.StringInterpolationStart.type")]
    StringInterpolationStart,

    // Literals and identifiers
    #[strum(serialize = "Token.IntToken")]
    IntToken,
    #[strum(serialize = "Token.Identifier")]
    IdentifierToken,

    // Layout
    #[strum(serialize = "Token.NewLine.type")]
    NewLine,
    #[strum(serialize = "Token.Semicolon.type")]
    Semicolon,

    // Keywords
    #[strum(serialize = "Token.KW_ARGON_BUILTIN.type")]
    KwArgonBuiltin,
    #[strum(serialize = "Token.KW_DEF.type")]
    KwDef,
    #[strum(serialize = "Token.KW_PROC.type")]
    KwProc,
    #[strum(serialize = "Token.KW_DO.type")]
    KwDo,
    #[strum(serialize = "Token.KW_END.type")]
    KwEnd,
    #[strum(serialize = "Token.KW_LET.type")]
    KwLet,
    #[strum(serialize = "Token.KW_VAL.type")]
    KwVal,
    #[strum(serialize = "Token.KW_MUT.type")]
    KwMut,
    #[strum(serialize = "Token.KW_MODULE.type")]
    KwModule,
    #[strum(serialize = "Token.KW_RECORD.type")]
    KwRecord,
    #[strum(serialize = "Token.KW_ENUM.type")]
    KwEnum,
    #[strum(serialize = "Token.KW_TRAIT.type")]
    KwTrait,
    #[strum(serialize = "Token.KW_INSTANCE.type")]
    KwInstance,
    #[strum(serialize = "Token.KW_NEW.type")]
    KwNew,
    #[strum(serialize = "Token.KW_WITH.type")]
    KwWith,
    #[strum(serialize = "Token.KW_TRUE.type")]
    KwTrue,
    #[strum(serialize = "Token.KW_FALSE.type")]
    KwFalse,
    #[strum(serialize = "Token.KW_AS.type")]
    KwAs,
    #[strum(serialize = "Token.KW_IS.type")]
    KwIs,
    #[strum(serialize = "Token.KW_IMPORT.type")]
    KwImport,
    #[strum(serialize = "Token.KW_EXPORT.type")]
    KwExport,
    #[strum(serialize = "Token.KW_PUBLIC.type")]
    KwPublic,
    #[strum(serialize = "Token.KW_PROTECTED.type")]
    KwProtected,
    #[strum(serialize = "Token.KW_PRIVATE.type")]
    KwPrivate,
    #[strum(serialize = "Token.KW_INTERNAL.type")]
    KwInternal,
    #[strum(serialize = "Token.KW_ABSTRACT.type")]
    KwAbstract,
    #[strum(serialize = "Token.KW_FINAL.type")]
    KwFinal,
    #[strum(serialize = "Token.KW_VIRTUAL.type")]
    KwVirtual,
    #[strum(serialize = "Token.KW_OVERRIDE.type")]
    KwOverride,
    #[strum(serialize = "Token.KW_IF.type")]
    KwIf,
    #[strum(serialize = "Token.KW_THEN.type")]
    KwThen,
    #[strum(serialize = "Token.KW_ELSE.type")]
    KwElse,
    #[strum(serialize = "Token.KW_ELSIF.type")]
    KwElsif,
    #[strum(serialize = "Token.KW_MATCH.type")]
    KwMatch,
    #[strum(serialize = "Token.KW_CASE.type")]
    KwCase,
    #[strum(serialize = "Token.KW_AND.type")]
    KwAnd,
    #[strum(serialize = "Token.KW_OR.type")]
    KwOr,
    #[strum(serialize = "Token.KW_TYPE.type")]
    KwType,
    #[strum(serialize = "Token.KW_BIGTYPE.type")]
    KwBigType,
    #[strum(serialize = "Token.KW_UNDERSCORE.type")]
    KwUnderscore,
    #[strum(serialize = "Token.KW_EXTERN.type")]
    KwExtern,
    #[strum(serialize = "Token.KW_RAISE.type")]
    KwRaise,
    #[strum(serialize = "Token.KW_BEGIN.type")]
    KwBegin,
    #[strum(serialize = "Token.KW_RESCUE.type")]
    KwRescue,
    #[strum(serialize = "Token.KW_FINALLY.type")]
    KwFinally,
    #[strum(serialize = "Token.KW_ERASED.type")]
    KwErased,
    #[strum(serialize = "Token.KW_REQUIRES.type")]
    KwRequires,
    #[strum(serialize = "Token.KW_ENSURES.type")]
    KwEnsures,
    #[strum(serialize = "Token.KW_MAINTAINS.type")]
    KwMaintains,
    #[strum(serialize = "Token.KW_ASSERT.type")]
    KwAssert,
    #[strum(serialize = "Token.KW_SUMMON.type")]
    KwSummon,
    #[strum(serialize = "Token.KW_WITNESS.type")]
    KwWitness,
    #[strum(serialize = "Token.KW_INLINE.type")]
    KwInline,
    #[strum(serialize = "Token.KW_EXTENSION.type")]
    KwExtension,
    #[strum(serialize = "Token.KW_INVERSE.type")]
    KwInverse,
    #[strum(serialize = "Token.KW_UPDATE.type")]
    KwUpdate,
    #[strum(serialize = "Token.KW_OPERATOR.type")]
    KwOperator,
    #[strum(serialize = "Token.KW_UNARY.type")]
    KwUnary,
    #[strum(serialize = "Token.KW_BOXED.type")]
    KwBoxed,
    #[strum(serialize = "Token.KW_BOX.type")]
    KwBox,
    #[strum(serialize = "Token.KW_UNBOX.type")]
    KwUnbox,
    #[strum(serialize = "Token.KW_FN.type")]
    KwFn,

    // Operators
    #[strum(serialize = "Token.OP_LOGICAL_AND.type")]
    OpLogicalAnd,
    #[strum(serialize = "Token.OP_LOGICAL_OR.type")]
    OpLogicalOr,
    #[strum(serialize = "Token.OP_EQUALS.type")]
    OpEquals,
    #[strum(serialize = "Token.OP_NOTEQUALS.type")]
    OpNotEquals,
    #[strum(serialize = "Token.OP_LESSTHANEQ.type")]
    OpLessThanEq,
    #[strum(serialize = "Token.OP_GREATERTHANEQ.type")]
    OpGreaterThanEq,
    #[strum(serialize = "Token.OP_SHIFTLEFT.type")]
    OpShiftLeft,
    #[strum(serialize = "Token.OP_SHIFTRIGHT.type")]
    OpShiftRight,
    #[strum(serialize = "Token.OP_ASSIGN.type")]
    OpAssign,
    #[strum(serialize = "Token.OP_LOGICAL_NOT.type")]
    OpLogicalNot,
    #[strum(serialize = "Token.OP_PLUS.type")]
    OpPlus,
    #[strum(serialize = "Token.OP_MINUS.type")]
    OpMinus,
    #[strum(serialize = "Token.OP_STAR.type")]
    OpStar,
    #[strum(serialize = "Token.OP_MUL.type")]
    OpMul,
    #[strum(serialize = "Token.OP_SLASH.type")]
    OpSlash,
    #[strum(serialize = "Token.OP_DIV.type")]
    OpDiv,
    #[strum(serialize = "Token.OP_BITAND.type")]
    OpBitAnd,
    #[strum(serialize = "Token.OP_BITOR.type")]
    OpBitOr,
    #[strum(serialize = "Token.OP_BITXOR.type")]
    OpBitXor,
    #[strum(serialize = "Token.OP_BITNOT.type")]
    OpBitNot,
    #[strum(serialize = "Token.OP_LESSTHAN.type")]
    OpLessThan,
    #[strum(serialize = "Token.OP_GREATERTHAN.type")]
    OpGreaterThan,
    #[strum(serialize = "Token.OP_UNION.type")]
    OpUnion,
    #[strum(serialize = "Token.OP_INTERSECTION.type")]
    OpIntersection,
    #[strum(serialize = "Token.OP_CONCAT.type")]
    OpConcat,
    #[strum(serialize = "Token.OP_STAR_STAR.type")]
    OpStarStar,
    #[strum(serialize = "Token.OP_ARROW.type")]
    OpArrow,
    #[strum(serialize = "Token.OP_FAT_ARROW.type")]
    OpFatArrow,
    #[strum(serialize = "Token.OP_PROP_EQUAL.type")]
    OpPropEqual,
    #[strum(serialize = "Token.OP_PROP_DISJUNCTION.type")]
    OpPropDisjunction,
    #[strum(serialize = "Token.OP_PROP_CONJUNCTION.type")]
    OpPropConjunction,
    #[strum(serialize = "Token.OP_DOTDOT.type")]
    OpDotDot,

    // Symbols
    #[strum(serialize = "Token.SYM_DOT.type")]
    SymDot,
    #[strum(serialize = "Token.SYM_COMMA.type")]
    SymComma,
    #[strum(serialize = "Token.SYM_COLON.type")]
    SymColon,
    #[strum(serialize = "Token.SYM_COLONCOLON.type")]
    SymColonColon,
    #[strum(serialize = "Token.SYM_OPENPAREN.type")]
    SymOpenParen,
    #[strum(serialize = "Token.SYM_CLOSEPAREN.type")]
    SymCloseParen,
    #[strum(serialize = "Token.SYM_OPENBRACKET.type")]
    SymOpenBracket,
    #[strum(serialize = "Token.SYM_CLOSEBRACKET.type")]
    SymCloseBracket,
    #[strum(serialize = "Token.SYM_OPENCURLY.type")]
    SymOpenCurly,
    #[strum(serialize = "Token.SYM_CLOSECURLY.type")]
    SymCloseCurly,
    #[strum(serialize = "Token.SYM_PIPE.type")]
    SymPipe,
    #[strum(serialize = "Token.SYM_AT.type")]
    SymAt,
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
    CurryCallExpr,
    CurryCallExprArgs1,
    CurryCallExprArgs2,
    EnclosedArgList,
    EnclosedArgListParen,
    EnclosedArgListSquare,
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
    RelationalExpr,
    EqualityExpr,
    LogicalAndExpr,
    LogicalOrExpr,
    PropConjunctionExpr,
    PropDisjunctionExpr,
    PropEqualityExpr,
    AsExpr,
    ClosureExpr,
    TupleExpr,
    TupleExprRest,
    AssignExpr,
    AssertExpr,
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
    MethodParameterListErasedFlag,
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


use Token::*;
use Rule::*;

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
                        "ModuleDeclaration"
                    )
                ],
            ),

            IdentifierOptional => ruleset(
                "Option[IdentifierExpr]",
                [
                    rule([ term(KwUnderscore) ], "const(None)"),
                    rule([ nonterm(Identifier) ], "Some"),
                ],
            ),
            Identifier => ruleset(
                "IdentifierExpr",
                [
                    rule([ term(IdentifierToken) ], "((id: Token.Identifier) => IdentifierExpr.Named(id.name))"),
                    rule([ nonterm(ComplexIdentifier) ], "identity"),
                ],
            ),
            ComplexIdentifier => ruleset(
                "IdentifierExpr",
                [
                    rule([ term(KwExtension).discard(), nonterm(NewLines).discard(), nonterm(Identifier) ], "((id: IdentifierExpr) => IdentifierExpr.Extension(id))"),
                    rule([ term(KwInverse).discard(), nonterm(NewLines).discard(), nonterm(Identifier) ], "((id: IdentifierExpr) => IdentifierExpr.Inverse(id))"),
                    rule([ term(KwUpdate).discard(), nonterm(NewLines).discard(), nonterm(Identifier) ], "((id: IdentifierExpr) => IdentifierExpr.Update(id))"),
                    rule([ term(KwUnary).discard(), nonterm(NewLines).discard(), term(KwOperator).discard(), nonterm(NewLines).discard(), nonterm(UnaryOperatorName) ], "((op: Token.UnaryOperatorToken[UnaryOperator & Operator.ValidIdentifier]) => IdentifierExpr.Op(op.unOperator))"),
                    rule([ term(KwOperator).discard(), nonterm(NewLines).discard(), nonterm(BinaryOperatorName) ], "((op: Token.BinaryOperatorToken[BinaryOperator & Operator.ValidIdentifier]) => IdentifierExpr.Op(op.binOperator))"),
                ],
            ),
            MethodName => ruleset(
                "IdentifierExpr",
                [
                    rule([ nonterm(Identifier) ], "identity"),
                    rule([ nonterm(Identifier), term(OpAssign).discard() ], "IdentifierExpr.Update"),
                ],
            ),
            
            BinaryOperatorName => ruleset(
                "Token.BinaryOperatorToken[BinaryOperator & Operator.ValidIdentifier]",
                [
                    rule([ term(OpPlus) ], "identity"),
                    rule([ term(OpMinus) ], "identity"),
                    rule([ term(OpStar) ], "identity"),
                    rule([ term(OpMul) ], "identity"),
                    rule([ term(OpSlash) ], "identity"),
                    rule([ term(OpDiv) ], "identity"),
                    rule([ term(OpConcat) ], "identity"),
                    rule([ term(OpEquals) ], "identity"),
                    rule([ term(OpNotEquals) ], "identity"),
                    rule([ term(OpLessThan) ], "identity"),
                    rule([ term(OpLessThanEq) ], "identity"),
                    rule([ term(OpGreaterThan) ], "identity"),
                    rule([ term(OpGreaterThanEq) ], "identity"),
                    rule([ term(OpBitAnd) ], "identity"),
                    rule([ term(OpBitOr) ], "identity"),
                    rule([ term(OpBitXor) ], "identity"),
                    rule([ term(OpShiftLeft) ], "identity"),
                    rule([ term(OpShiftRight) ], "identity"),
                ],
            ),
            UnaryOperatorName => ruleset(
                "Token.UnaryOperatorToken[UnaryOperator & Operator.ValidIdentifier]",
                [
                    rule([ term(OpPlus) ], "identity"),
                    rule([ term(OpMinus) ], "identity"),
                    rule([ term(OpLogicalNot) ], "identity"),
                    rule([ term(OpBitNot) ], "identity"),
                ],
            ),

            NewLines => ruleset(
                "Unit",
                [
                    rule([ term(NewLine).discard(), nonterm(NewLines).discard() ], "const(())"),
                    rule([], "const(())"),
                ],
            ),
            StatementSeparator => ruleset(
                "Unit",
                [
                    rule([ term(NewLine).discard() ], "const(())"),
                    rule([ term(Semicolon).discard() ], "const(())"),
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
                        "((cond: WithSource[Expr], rest: WithSource[Expr] => Expr) => rest(cond))",
                    ),
                ],
            ).lex_mode("LexerMode.SkipNewLines"),
            IfExprAfterThen => ruleset(
                "WithSource[Expr] => Expr",
                [
                    rule(
                        [
                            nonterm(StatementList).with_location(),
                            term(KwEnd).with_location(),
                        ],
                        "((thenBody: WithSource[Seq[WithSource[Stmt]]], endKeyword: WithSource[?]) => (cond: WithSource[Expr]) => Expr.IfElse(cond, thenBody, WithLocation(Seq.empty, thenBody.location)))"
                    ),
                    rule(
                        [
                            nonterm(StatementList).with_location(),
                            term(KwElse).discard(),
                            nonterm(StatementList).with_location(),
                            term(KwEnd).discard(),
                        ],
                        "((thenBody: WithSource[Seq[WithSource[Stmt]]], elseBody: WithSource[Seq[WithSource[Stmt]]]) => (cond: WithSource[Expr]) => Expr.IfElse(cond, thenBody, elseBody))"
                    ),
                    rule(
                        [
                            nonterm(StatementList).with_location(),
                            term(KwElsif).discard(),
                            nonterm(IfExprFromCond).with_location(),
                        ],
                        "((thenBody: WithSource[Seq[WithSource[Stmt]]], elseExpr: WithSource[Expr]) => (cond: WithSource[Expr]) => Expr.IfElse(cond, thenBody, WithLocation(Seq(elseExpr), elseExpr.location)))"
                    ),
                ],
            ).lex_mode("LexerMode.Normal"),

            StringExpr => ruleset(
                "Expr.StringLiteral",
                [
                    rule([ term(StringStart).discard(), nonterm(StringExprRest) ], "identity"),
                ]
            ).lex_mode("LexerMode.StringText"),
            StringExprRest => ruleset(
                "Expr.StringLiteral",
                [
                    rule([ nonterm(StringExprContent), term(StringEnd).discard() ], "((fragments: List[StringFragment]) => Expr.StringLiteral(simplifyFragments(fragments)))"),
                ]
            ),
            StringExprContent => ruleset(
                "List[StringFragment]",
                [
                    rule([], "const(Nil)"),
                    rule([ nonterm(StringFragment), nonterm(StringExprContent) ], "((h: StringFragment, t: List[StringFragment]) => h :: t)"),
                ],
            ),
            StringFragment => ruleset(
                "StringFragment",
                [
                    rule([ term(StringText) ], "((s: Token.StringText) => StringFragment.Text(s.text))"),
                    rule([ term(StringInterpolationStart).discard(), nonterm(StringInterpolationRest) ], "identity"),
                ],
            ),
            StringInterpolationRest => ruleset(
                "StringFragment",
                [
                    rule([ nonterm(Expression).with_location(), term(SymCloseCurly).discard() ], "StringFragment.Interpolate"),
                ],
            ).lex_mode("LexerMode.SkipNewLines"),

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
                        "Expr.Match",
                    )
                ]
            ).lex_mode("LexerMode.SkipNewLines"),
            MatchCases => ruleset(
                "Seq[WithSource[MatchCase]]",
                [
                    rule([], "const(Seq.empty)"),
                    rule(
                        [
                            nonterm(MatchCase).with_location(),
                            nonterm(MatchCases),
                        ],
                        "((h: WithSource[MatchCase], t: Seq[WithSource[MatchCase]]) => h +: t)",
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
                        "MatchCase"
                    ),
                ],
            ),
            MatchCaseBody => ruleset(
                "Expr",
                [
                    rule([ nonterm(BlockBody) ], "identity"),
                ],
            ).lex_mode("LexerMode.Normal"),

            PrimaryExpr(ParenAllowedState::NotAllowed) => ruleset(
                "Expr",
                [
                    rule([ nonterm(Identifier) ], "identity"),
                    rule([ nonterm(StringExpr) ], "identity"),
                    rule([ nonterm(MatchExpr) ], "identity"),
                    rule([ term(IntToken) ], "((token: Token.IntToken) => Expr.IntLiteral(token.value))"),
                    rule([ term(KwTrue) ], "const(Expr.BoolLiteral(true))"),
                    rule([ term(KwFalse) ], "const(Expr.BoolLiteral(false))"),
                    rule([ nonterm(IfExpr) ], "identity"),
                    rule([ term(KwBegin).discard(), nonterm(BlockBodyWithEnd) ], "((e: WithSource[Expr]) => e.value)"),
                    // function result value
                    rule([ term(KwArgonBuiltin).discard(), term(IdentifierToken) ], "((id: Token.Identifier) => Expr.Builtin(id.name))"),
                ],
            ),
            PrimaryExpr(ParenAllowedState::Allowed) => ruleset(
                "Expr",
                [
                    rule([ nonterm(PrimaryExpr(ParenAllowedState::NotAllowed)) ], "identity"),
                    rule([ term(SymOpenParen).discard(), term(SymCloseParen).discard() ], "const(Expr.Tuple(Seq.empty))"),
                    rule([ term(SymOpenParen).discard(), nonterm(Expression), term(SymCloseParen).discard() ], "identity"),
                ],
            ),
            PostfixExpr(paren_allowed) => {
                let mut rules = vec![
                    rule([ nonterm(PrimaryExpr(paren_allowed)) ], "identity"),
                    rule(
                        [
                            nonterm(PostfixExpr(paren_allowed)).with_location(),
                            term(SymDot).discard(),
                            nonterm(Identifier).with_location(),
                        ],
                        "Expr.Dot",
                    ),
                    rule(
                        [
                            nonterm(PostfixExpr(paren_allowed)).with_location(),
                            term(SymDot).discard(),
                            term(KwBoxed).discard(),
                        ],
                        "Expr.BoxedType",
                    ),
                    rule(
                        [
                            nonterm(PostfixExpr(paren_allowed)).with_location(),
                            term(SymDot).discard(),
                            term(KwBox).discard(),
                        ],
                        "Expr.Box",
                    ),
                    rule(
                        [
                            nonterm(PostfixExpr(paren_allowed)).with_location(),
                            term(SymDot).discard(),
                            term(KwUnbox).discard(),
                        ],
                        "Expr.Unbox",
                    ),
                    rule(
                        [
                            nonterm(PostfixExpr(paren_allowed)).with_location(),
                            term(SymDot).discard(),
                            term(KwNew).discard(),
                            nonterm(NewTraitObjectBody),
                            term(KwEnd).discard(),
                        ],
                        "Expr.NewTraitObject"
                    ),
                    rule(
                        [
                            nonterm(PostfixExpr(paren_allowed)).with_location(),
                            term(SymOpenCurly).discard(),
                            nonterm(RecordLiteralFieldsWithBlockEnd),
                        ],
                        "Expr.RecordLiteral",
                    ),
                ];

                if paren_allowed == ParenAllowedState::Allowed {
                    rules.push(rule(
                        [
                            nonterm(PostfixExpr(paren_allowed)).with_location(),
                            nonterm(EnclosedArgList),
                        ],
                        "({ case (funcExpr: WithSource[Expr], (listType: FunctionParameterListType, argExpr: WithSource[Expr])) => Expr.FunctionCall(funcExpr, listType, argExpr) } : ((WithSource[Expr], (FunctionParameterListType, WithSource[Expr])) => Expr))",
                    ));
                }

                ruleset(
                    "Expr",
                    rules,
                )
            },
            CurryCallExpr => ruleset(
                "Expr",
                [
                    rule(
                        [
                            nonterm(PostfixExpr(ParenAllowedState::Allowed)).with_location(),
                            nonterm(CurryCallExprArgs1),
                        ],
                        "((funcExpr: WithSource[Expr], args: Seq[(FunctionParameterListType, WithSource[Expr])]) => curriedCall(funcExpr, args))",
                    ),
                ],
            ),
            CurryCallExprArgs1 => ruleset(
                "Seq[(FunctionParameterListType, WithSource[Expr])]",
                [
                    rule([], "const(Seq.empty)"),
                    rule(
                        [
                            nonterm(PostfixExpr(ParenAllowedState::NotAllowed)).with_location(),
                            nonterm(CurryCallExprArgs2),
                        ],
                        "((h: WithSource[Expr], t: Seq[(FunctionParameterListType, WithSource[Expr])]) => (FunctionParameterListType.NormalList, h) +: t)",
                    ),
                ],
            ),
            CurryCallExprArgs2 => ruleset(
                "Seq[(FunctionParameterListType, WithSource[Expr])]",
                [
                    rule([], "const(Seq.empty)"),
                    rule(
                        [
                            nonterm(EnclosedArgList),
                            nonterm(CurryCallExprArgs2),
                        ],
                        "((h: (FunctionParameterListType, WithSource[Expr]), t: Seq[(FunctionParameterListType, WithSource[Expr])]) => h +: t)",
                    ),
                    rule(
                        [
                            nonterm(PostfixExpr(ParenAllowedState::NotAllowed)).with_location(),
                            nonterm(CurryCallExprArgs2),
                        ],
                        "((h: WithSource[Expr], t: Seq[(FunctionParameterListType, WithSource[Expr])]) => (FunctionParameterListType.NormalList, h) +: t)",
                    ),
                ],
            ),
            EnclosedArgList => ruleset(
                "(FunctionParameterListType, WithSource[Expr])",
                [
                    rule([ term(SymOpenParen).discard(), nonterm(EnclosedArgListParen) ], "identity"),
                    rule([ term(SymOpenBracket).discard(), nonterm(EnclosedArgListSquare) ], "identity"),
                ],
            ),
            EnclosedArgListParen => ruleset(
                "(FunctionParameterListType, WithSource[Expr])",
                [
                    rule([ term(KwRequires).discard(), nonterm(Expression).with_location(), term(SymCloseParen).discard() ], "((argExpr: WithSource[Expr]) => (FunctionParameterListType.RequiresList, argExpr))"),
                    rule([ term(SymCloseParen).with_location() ], "((closeParen: WithSource[?]) => (FunctionParameterListType.NormalList, WithLocation(Expr.Tuple(Seq.empty), closeParen.location)))"),
                    rule([ nonterm(Expression).with_location(), term(SymCloseParen).discard() ], "((argExpr: WithSource[Expr]) => (FunctionParameterListType.NormalList, argExpr))"),
                ],
            ),
            EnclosedArgListSquare => ruleset(
                "(FunctionParameterListType, WithSource[Expr])",
                [
                    rule([ term(SymCloseBracket).with_location() ], "((closeBracket: WithSource[?]) => (FunctionParameterListType.InferrableList, WithLocation(Expr.Tuple(Seq.empty), closeBracket.location)))"),
                    rule([ nonterm(Expression).with_location(), term(SymCloseBracket).discard() ], "((argExpr: WithSource[Expr]) => (FunctionParameterListType.InferrableList, argExpr))"),
                ],
            ),
            RecordLiteralFieldsWithBlockEnd => ruleset(
                "WithSource[Seq[WithSource[RecordFieldLiteral]]]",
                [
                    rule([ nonterm(RecordLiteralFields).with_location(), term(SymCloseCurly).discard() ], "identity"),
                ],
            ),
            RecordLiteralFields => ruleset(
                "Seq[WithSource[RecordFieldLiteral]]",
                [
                    rule([], "const(Seq.empty)"),
                    rule([ nonterm(StatementSeparator).discard(), nonterm(RecordLiteralFields) ], "identity" ),
                    rule([ nonterm(RecordLiteralField).with_location(), nonterm(StatementSeparator).discard(), nonterm(RecordLiteralFields) ], "((h: WithSource[RecordFieldLiteral], t: Seq[WithSource[RecordFieldLiteral]]) => h +: t)"),
                    rule([ nonterm(RecordLiteralField).with_location() ], "((f: WithSource[RecordFieldLiteral]) => Seq(f))"),
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
                        "RecordFieldLiteral"),
                ],
            ),

            UnaryExpr => ruleset(
                "Expr",
                [
                    rule([ nonterm(CurryCallExpr) ], "identity"),
                    rule([ nonterm(TypeExpr) ], "identity"),
                    rule([ term(OpBitNot), nonterm(UnaryExpr).with_location() ], "unaryOp"),
                    rule([ term(OpLogicalNot), nonterm(UnaryExpr).with_location() ], "unaryOp"),
                    rule([ term(OpPlus), nonterm(UnaryExpr).with_location() ], "unaryOp"),
                    rule([ term(OpMinus), nonterm(UnaryExpr).with_location() ], "unaryOp"),
                ],
            ),
            TypeExpr => ruleset(
                "Expr",
                [
                    rule([ term(KwType).discard() ], "const(Expr.Type)"),
                    rule([ term(KwBigType).discard(), term(SymOpenBracket).discard(), term(IntToken), term(SymCloseBracket).discard() ], "((n: Token.IntToken) => Expr.BigType(n.value))"),
                ],
            ),
            MultiplicativeExpr => ruleset(
                "Expr",
                [
                    rule([ nonterm(UnaryExpr) ], "identity"),
                    rule([ nonterm(MultiplicativeExpr).with_location(), term(OpStar), nonterm(UnaryExpr).with_location() ], "binaryOp"),
                    rule([ nonterm(MultiplicativeExpr).with_location(), term(OpMul), nonterm(UnaryExpr).with_location() ], "binaryOp"),
                    rule([ nonterm(MultiplicativeExpr).with_location(), term(OpSlash), nonterm(UnaryExpr).with_location() ], "binaryOp"),
                    rule([ nonterm(MultiplicativeExpr).with_location(), term(OpDiv), nonterm(UnaryExpr).with_location() ], "binaryOp"),
                ],
            ),
            AdditiveExpr => ruleset(
                "Expr",
                [
                    rule([ nonterm(MultiplicativeExpr) ], "identity"),
                    rule([ nonterm(AdditiveExpr).with_location(), term(OpPlus), nonterm(MultiplicativeExpr).with_location() ], "binaryOp"),
                    rule([ nonterm(AdditiveExpr).with_location(), term(OpMinus), nonterm(MultiplicativeExpr).with_location() ], "binaryOp"),
                    rule([ nonterm(AdditiveExpr).with_location(), term(OpConcat), nonterm(MultiplicativeExpr).with_location() ], "binaryOp"),
                ],
            ),
            ShiftExpr => ruleset(
                "Expr",
                [
                    rule([ nonterm(AdditiveExpr) ], "identity"),
                    rule([ nonterm(ShiftExpr).with_location(), term(OpShiftLeft), nonterm(AdditiveExpr).with_location() ], "binaryOp"),
                    rule([ nonterm(ShiftExpr).with_location(), term(OpShiftRight), nonterm(AdditiveExpr).with_location() ], "binaryOp"),
                ],
            ),
            BitwiseAndExpr => ruleset(
                "Expr",
                [
                    rule([ nonterm(ShiftExpr) ], "identity"),
                    rule([ nonterm(BitwiseAndExpr).with_location(), term(OpBitAnd), nonterm(ShiftExpr).with_location() ], "binaryOp"),
                ],
            ),
            BitwiseXorExpr => ruleset(
                "Expr",
                [
                    rule([ nonterm(BitwiseAndExpr) ], "identity"),
                    rule([ nonterm(BitwiseXorExpr).with_location(), term(OpBitXor), nonterm(BitwiseAndExpr).with_location() ], "binaryOp"),
                ],
            ),
            BitwiseOrExpr => ruleset(
                "Expr",
                [
                    rule([ nonterm(BitwiseXorExpr) ], "identity"),
                    rule([ nonterm(BitwiseOrExpr).with_location(), term(OpBitOr), nonterm(BitwiseXorExpr).with_location() ], "binaryOp"),
                ],
            ),
            FunctionTypeExpr => ruleset(
                "Expr",
                [
                    rule([ nonterm(BitwiseOrExpr).with_location() ], "((e: WithSource[Expr]) => e.value)"),
                    rule(
                        [
                            term(KwFn).discard(),
                            nonterm(BitwiseOrExpr).with_location(),
                            term(OpArrow).discard(),
                            nonterm(FunctionTypeExpr).with_location(),
                        ],
                        "Expr.FunctionType"
                    ),
                    rule(
                        [
                            nonterm(BitwiseOrExpr).with_location(),
                            term(OpArrow).discard(),
                            nonterm(FunctionTypeExpr).with_location(),
                        ],
                        "Expr.FunctionType"
                    ),
                ],
            ),
            RelationalExpr => ruleset(
                "Expr",
                [
                    rule([ nonterm(FunctionTypeExpr) ], "identity"),
                    rule([ nonterm(RelationalExpr).with_location(), term(OpLessThan), nonterm(FunctionTypeExpr).with_location() ], "binaryOp"),
                    rule([ nonterm(RelationalExpr).with_location(), term(OpLessThanEq), nonterm(FunctionTypeExpr).with_location() ], "binaryOp"),
                    rule([ nonterm(RelationalExpr).with_location(), term(OpGreaterThan), nonterm(FunctionTypeExpr).with_location() ], "binaryOp"),
                    rule([ nonterm(RelationalExpr).with_location(), term(OpGreaterThanEq), nonterm(FunctionTypeExpr).with_location() ], "binaryOp"),
                ],
            ),
            EqualityExpr => ruleset(
                "Expr",
                [
                    rule([ nonterm(RelationalExpr) ], "identity"),
                    rule([ nonterm(EqualityExpr).with_location(), term(OpEquals), nonterm(RelationalExpr).with_location() ], "binaryOp"),
                    rule([ nonterm(EqualityExpr).with_location(), term(OpNotEquals), nonterm(RelationalExpr).with_location() ], "binaryOp"),
                ],
            ),
            LogicalAndExpr => ruleset(
                "Expr",
                [
                    rule([ nonterm(EqualityExpr) ], "identity"),
                    rule([ nonterm(LogicalAndExpr).with_location(), term(OpLogicalAnd), nonterm(EqualityExpr).with_location() ], "binaryOp"),
                ],
            ),
            LogicalOrExpr => ruleset(
                "Expr",
                [
                    rule([ nonterm(LogicalAndExpr) ], "identity"),
                    rule([ nonterm(LogicalOrExpr).with_location(), term(OpLogicalOr), nonterm(LogicalAndExpr).with_location() ], "binaryOp"),
                ],
            ),
            PropConjunctionExpr => ruleset(
                "Expr",
                [
                    rule([ nonterm(LogicalOrExpr) ], "identity"),
                    rule([ nonterm(PropConjunctionExpr).with_location(), term(OpPropConjunction), nonterm(LogicalOrExpr).with_location() ], "binaryOp"),
                ],
            ),
            PropDisjunctionExpr => ruleset(
                "Expr",
                [
                    rule([ nonterm(PropConjunctionExpr) ], "identity"),
                    rule([ nonterm(PropDisjunctionExpr).with_location(), term(OpPropDisjunction), nonterm(PropConjunctionExpr).with_location() ], "binaryOp"),
                ],
            ),
            PropEqualityExpr => ruleset(
                "Expr",
                [
                    rule([ nonterm(PropDisjunctionExpr) ], "identity"),
                    rule([ nonterm(PropEqualityExpr).with_location(), term(OpPropEqual), nonterm(PropDisjunctionExpr).with_location() ], "binaryOp"),
                ],
            ),
            AsExpr => ruleset(
                "Expr",
                [
                    rule([ nonterm(PropEqualityExpr).with_location() ], "((e: WithSource[Expr]) => e.value)"),
                    rule([ nonterm(AsExpr).with_location(), term(KwAs).discard(), nonterm(PropEqualityExpr).with_location() ], "Expr.As"),
                    rule([ nonterm(AsExpr).with_location(), term(KwIs).discard(), nonterm(NonTuplePattern).with_location() ], "Expr.Is"),
                ],
            ),
            ClosureExpr => ruleset(
                "Expr",
                [
                    rule([ nonterm(AsExpr) ], "identity"),
                    rule(
                        [
                            term(SymPipe).discard(),
                            nonterm(IdentifierOptional),
                            term(SymPipe).discard(),
                            nonterm(ClosureExpr).with_location(),
                        ],
                        "Expr.FunctionLiteral",
                    ),
                ],
            ),
            TupleExpr => ruleset(
                "Expr",
                [
                    rule([ nonterm(ClosureExpr).with_location() ], "((e: WithSource[Expr]) => e.value)"),
                    rule([ nonterm(ClosureExpr).with_location(), term(SymComma).discard(), nonterm(TupleExprRest) ], "((h: WithSource[Expr], t: Seq[WithSource[Expr]]) => Expr.Tuple(h +: t))"),
                ],
            ),
            TupleExprRest => ruleset(
                "Seq[WithSource[Expr]]",
                [
                    rule([ nonterm(ClosureExpr).with_location() ], "((e: WithSource[Expr]) => Seq(e))"),
                    rule([ nonterm(ClosureExpr).with_location(), term(SymComma).discard(), nonterm(TupleExprRest) ], "((h: WithSource[Expr], t: Seq[WithSource[Expr]]) => h +: t)"),
                ],
            ),
            AssignExpr => ruleset(
                "Expr",
                [
                    rule([ nonterm(TupleExpr).with_location() ], "((e: WithSource[Expr]) => e.value)"),
                    rule([ nonterm(TupleExpr).with_location(), term(OpAssign).discard(), nonterm(TupleExpr).with_location() ], "((left: WithSource[Expr], right: WithSource[Expr]) => Expr.BinaryOperation(left, BinaryOperator.Assign, right))"),
                ],
            ),
            AssertExpr => ruleset(
                "Expr",
                [
                    rule([ term(KwAssert).discard(), nonterm(NewLines).discard(), nonterm(Expression).with_location() ], "Expr.Assert"),
                ]
            ),
            Expression => ruleset(
                "Expr",
                [
                    rule([ nonterm(AssertExpr) ], "identity"),
                    rule([ nonterm(AssignExpr) ], "identity"),
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
                    rule([ term(KwUnderscore).discard() ], "const(Pattern.Discard)"),

                    rule([ nonterm(StringExpr) ], "Pattern.String"),
                    rule([ term(IntToken) ], "((token: Token.IntToken) => Pattern.Int(token.value))"),
                    rule([ term(KwTrue) ], "const(Pattern.Bool(true))"),
                    rule([ term(KwFalse) ], "const(Pattern.Bool(false))"),
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
                    rule([], "const(Pattern.Tuple(Seq.empty))"),
                    rule([ nonterm(NonTuplePattern).with_location() ], "((p: WithSource[Pattern]) => p.value)"),
                    rule([ nonterm(NonTuplePattern).with_location(), term(SymComma).discard(), nonterm(NonEmptyTuplePattern) ], "((h: WithSource[Pattern], t: Seq[WithSource[Pattern]]) => Pattern.Tuple(h +: t))"),
                ],
            ),
            NonEmptyTuplePattern => ruleset(
                "Seq[WithSource[Pattern]]",
                [
                    rule([], "const(Seq.empty)"),
                    rule([ nonterm(NonTuplePattern).with_location() ], "Seq"),
                    rule(
                        [ nonterm(NonTuplePattern).with_location(), term(SymComma).discard(), nonterm(NonEmptyTuplePattern) ],
                        "((h: WithSource[Pattern], t: Seq[WithSource[Pattern]]) => h +: t)",
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
                        "Pattern.Constructor",
                    ),
                    rule(
                        [
                            nonterm(MutSpec),
                            nonterm(Identifier).with_location(),
                            term(SymAt).discard(),
                            nonterm(SimplePattern(ParenAllowedState::Allowed)).with_location()
                        ],
                        "((mutSpec: Boolean, id: WithSource[IdentifierExpr], pattern: WithSource[Pattern]) => Pattern.Binding(mutSpec, id, pattern))",
                    ),
                    rule(
                        [
                            nonterm(MutSpec),
                            nonterm(Identifier).with_location(),
                        ],
                        "((mutSpec: Boolean, id: WithSource[IdentifierExpr]) => Pattern.Binding(mutSpec, id, WithLocation(Pattern.Discard, id.location)))",
                    ),
                ],
            ),
            PatternPath => ruleset(
                "PatternPath",
                [
                    rule(
                        [ nonterm(Identifier).with_location() ],
                        "PatternPath.Base",
                    ),
                    rule(
                        [ nonterm(PatternPath).with_location(), term(SymDot).discard(), nonterm(Identifier).with_location() ],
                        "PatternPath.Member",
                    ),
                ],
            ),
            ConstructorArgsPattern => ruleset(
                "Seq[PatternArgument]",
                [
                    rule([], "const(Seq.empty)"),
                    rule(
                        [
                            nonterm(ConstructorArgPattern),
                            nonterm(ConstructorArgsPattern),
                        ],
                        "((h: PatternArgument, t: Seq[PatternArgument]) => h +: t)",
                    ),

                ],
            ),
            ConstructorArgPattern => ruleset(
                "PatternArgument",
                [
                    rule(
                        [ nonterm(PatternPath).with_location() ],
                        "((path: WithSource[PatternPath]) => PatternArgument(FunctionParameterListType.NormalList, WithLocation(Pattern.Constructor(path, Seq.empty), path.location)))",
                    ),
                    rule(
                        [ nonterm(SimplePattern(ParenAllowedState::NotAllowed)).with_location() ],
                        "((pattern: WithSource[Pattern]) => PatternArgument(FunctionParameterListType.NormalList, pattern))",
                    ),
                    rule(
                        [
                            term(SymOpenParen).discard(),
                            nonterm(Pattern).with_location(),
                            term(SymCloseParen).discard(),
                        ],
                        "((pattern: WithSource[Pattern]) => PatternArgument(FunctionParameterListType.NormalList, pattern))",
                    ),
                    rule(
                        [
                            term(SymOpenBracket).discard(),
                            term(KwRequires).discard(),
                            nonterm(Pattern).with_location(),
                            term(SymCloseBracket).discard(),
                        ],
                        "((pattern: WithSource[Pattern]) => PatternArgument(FunctionParameterListType.InferrableList, pattern))",
                    ),
                ],
            ),


            Modifiers => ruleset(
                "Seq[WithSource[Modifier]]",
                [
                    rule([], "const(Seq.empty)"),
                    rule([ nonterm(Modifier).with_location(), nonterm(Modifiers1) ], "((h: WithSource[Modifier], t: Seq[WithSource[Modifier]]) => h +: t)" ),
                ],
            ),
            Modifiers1 => ruleset(
                "Seq[WithSource[Modifier]]",
                [
                    rule([], "const(Seq.empty)"),
                    rule([ term(NewLine).discard(), nonterm(Modifiers1) ], "identity"),
                    rule([ nonterm(Modifier).with_location(), nonterm(Modifiers1) ], "((h: WithSource[Modifier], t: Seq[WithSource[Modifier]]) => h +: t)" ),
                ],
            ),
            Modifier => ruleset(
                "Modifier",
                [
                    rule([ term(KwPublic).discard() ], "const(Modifier.Public)"),
                    rule([ term(KwPrivate).discard() ], "const(Modifier.Private)"),
                    rule([ term(KwProtected).discard() ], "const(Modifier.Protected)"),
                    rule([ term(KwInternal).discard() ], "const(Modifier.Internal)"),
                    rule([ term(KwErased).discard() ], "const(Modifier.Erased)"),
                    rule([ term(KwWitness).discard() ], "const(Modifier.Witness)"),
                    rule([ term(KwInline).discard() ], "const(Modifier.Inline)"),
                    rule([ term(KwFinal).discard() ], "const(Modifier.Final)"),
                    rule([ term(KwVirtual).discard() ], "const(Modifier.Virtual)"),
                    rule([ term(KwOverride).discard() ], "const(Modifier.Override)"),
                ],
            ),

            VariableDeclarationRest => ruleset(
                "Seq[WithSource[Modifier]] => Stmt",
                [
                    rule(
                        [
                            term(KwLet).discard(),
                            nonterm(VariableDeclarationBinding),
                            nonterm(VariableDeclarationValue).with_location(),
                        ],
                        "({ case ((isMutable, id, typeAnnotation), value) => modifiers => VariableDeclarationStmt(modifiers, isMutable, id, typeAnnotation, value)} : ((Boolean, Option[IdentifierExpr], Option[WithSource[Expr]]), WithSource[Expr]) => Seq[WithSource[Modifier]] => Stmt)"
                    ),
                ],
            ),
            VariableDeclarationBinding => ruleset(
                "(Boolean, Option[IdentifierExpr], Option[WithSource[Expr]])",
                [
                    rule(
                        [
                            nonterm(VariableMutSpec),
                            nonterm(IdentifierOptional),
                            nonterm(VariableDeclarationTypeSpec),
                            term(OpEquals).discard(),
                            nonterm(NewLines).discard(),
                        ],
                        "((isMutable: Boolean, id: Option[IdentifierExpr], typeAnnotation: Option[WithSource[Expr]]) => (isMutable, id, typeAnnotation))",
                    ),
                ]
            ).lex_mode("LexerMode.SkipNewLines"),
            VariableMutSpec => ruleset(
                "Boolean",
                [
                    rule([], "const(false)"),
                    rule([ nonterm(MutSpec) ], "identity"),
                ]
            ),
            MutSpec => ruleset(
                "Boolean",
                [
                    rule([ term(KwVal) ], "const(false)"),
                    rule([ term(KwMut) ], "const(true)"),
                ]
            ),
            VariableDeclarationTypeSpec => ruleset(
                "Option[WithSource[Expr]]",
                [
                    rule([], "const(None)"),
                    rule([ term(SymColon).discard(), nonterm(TypeBinding).with_location() ], "Some"),
                ]
            ),
            VariableDeclarationValue => ruleset(
                "Expr",
                [
                    rule([ nonterm(Expression) ], "identity"),
                ],
            ).lex_mode("LexerMode.Normal"),

            MethodOrFunctionDeclarationStmtRest => ruleset(
                "(Seq[WithSource[Modifier]], Boolean) => (MethodDeclarationStmt | FunctionDeclarationStmt)",
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
                        "((instanceName: WithSource[IdentifierExpr], name: WithSource[IdentifierExpr], parameters: Seq[WithSource[FunctionParameterList]], returnType: WithSource[ReturnTypeSpecifier], body: Option[FunctionBody]) => (modifiers, purity) => MethodDeclarationStmt(modifiers, purity, instanceName.map(Some.apply), None, name, parameters, returnType, body))"
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
                        "((instanceName: WithSource[?], name: WithSource[IdentifierExpr], parameters: Seq[WithSource[FunctionParameterList]], returnType: WithSource[ReturnTypeSpecifier], body: Option[FunctionBody]) => (modifiers, purity) => MethodDeclarationStmt(modifiers, purity, instanceName.map(_ => None), None, name, parameters, returnType, body))"
                    ),
                    rule(
                        [
                            nonterm(Identifier).with_location(),
                            nonterm(MethodParameters),
                            term(SymColon).discard(),
                            nonterm(MethodReturnType).with_location(),
                            nonterm(FunctionBody),
                        ],
                        "((name: WithSource[IdentifierExpr], parameters: Seq[WithSource[FunctionParameterList]], returnType: WithSource[ReturnTypeSpecifier], body: FunctionBody) => (modifiers, purity) => FunctionDeclarationStmt(modifiers, purity, name, parameters, returnType, body))"
                    ),
                ],
            ).lex_mode("LexerMode.SkipNewLines"),
            MethodPurity => ruleset(
                "Boolean",
                [
                    rule([ term(KwDef) ], "const(true)"),
                    rule([ term(KwProc) ], "const(false)"),
                ],
            ),
            MethodParameters => ruleset(
                "Seq[WithSource[FunctionParameterList]]",
                [
                    rule([], "const(Seq.empty)"),
                    rule([ nonterm(MethodParameterList).with_location(), nonterm(MethodParameters) ], "((h: WithSource[FunctionParameterList], t: Seq[WithSource[FunctionParameterList]]) => h +: t)"),
                ],
            ),
            MethodParameterList => ruleset(
                "FunctionParameterList",
                [
                    rule(
                        [
                            term(SymOpenParen).discard(),
                            term(KwRequires).discard(),
                            nonterm(MethodParameterListErasedFlag),
                            nonterm(MethodParameterListContents),
                            term(SymCloseParen).discard(),
                        ],
                        "({ case (isErased, (parameters, hasTrailingComma)) => FunctionParameterList(FunctionParameterListType.RequiresList, isErased, parameters, hasTrailingComma) } : (Boolean, (Seq[WithSource[FunctionParameter]], Boolean)) => FunctionParameterList)"
                    ),
                    rule(
                        [
                            term(SymOpenParen).discard(),
                            nonterm(MethodParameterListErasedFlag),
                            nonterm(MethodParameterListContents),
                            term(SymCloseParen).discard(),
                        ],
                        "({ case (isErased, (parameters, hasTrailingComma)) => FunctionParameterList(FunctionParameterListType.NormalList, isErased, parameters, hasTrailingComma) } : (Boolean, (Seq[WithSource[FunctionParameter]], Boolean)) => FunctionParameterList)"
                    ),
                    rule(
                        [
                            term(SymOpenBracket).discard(),
                            nonterm(MethodParameterListErasedFlag),
                            nonterm(MethodParameterListContents),
                            term(SymCloseBracket).discard(),
                        ],
                        "({ case (isErased, (parameters, hasTrailingComma)) => FunctionParameterList(FunctionParameterListType.InferrableList, isErased, parameters, hasTrailingComma) } : (Boolean, (Seq[WithSource[FunctionParameter]], Boolean)) => FunctionParameterList)"
                    ),
                ],
            ).lex_mode("LexerMode.SkipNewLines"),
            MethodParameterListErasedFlag => ruleset(
                "Boolean",
                [
                    rule([], "const(false)"),
                    rule([ term(KwErased) ], "const(true)"),
                ],
            ),
            MethodParameterListContents => ruleset(
                "(Seq[WithSource[FunctionParameter]], Boolean)",
                [
                    rule([], "const(Seq.empty, false)"),
                    rule([ nonterm(MethodParameter).with_location(), nonterm(MethodParameterListContents1) ], "({ case (h, (t, hasTrailingComma)) => (h +: t, hasTrailingComma) } : (WithSource[FunctionParameter], (Seq[WithSource[FunctionParameter]], Boolean)) => (Seq[WithSource[FunctionParameter]], Boolean))"),
                ],
            ),
            MethodParameterListContents1 => ruleset(
                "(Seq[WithSource[FunctionParameter]], Boolean)",
                [
                    rule([], "const(Seq.empty, false)"),
                    rule([ term(SymComma).discard() ], "const(Seq.empty, true)"),
                    rule(
                        [
                            term(SymComma).discard(),
                            nonterm(MethodParameter).with_location(),
                            nonterm(MethodParameterListContents1),
                        ],
                        "({ case (h, (t, hasTrailingComma)) => (h +: t, hasTrailingComma) } : (WithSource[FunctionParameter], (Seq[WithSource[FunctionParameter]], Boolean)) => (Seq[WithSource[FunctionParameter]], Boolean))"
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
                        "((name, paramType) => FunctionParameter(paramType, name))"
                    ),
                ],
            ),
            MethodReturnType => ruleset(
                "ReturnTypeSpecifier",
                [
                    rule([ nonterm(TypeBinding).with_location(), nonterm(MethodEnsuresClause) ], "ReturnTypeSpecifier"),
                ],
            ),
            MethodEnsuresClause => ruleset(
                "Seq[WithSource[Expr]]",
                [
                    rule([], "const(Seq.empty)"),
                    rule([ term(KwEnsures).discard(), nonterm(TypeBinding).with_location(), nonterm(MethodEnsuresClause) ], "((h: WithSource[Expr], t: Seq[WithSource[Expr]]) => h +: t)"),
                ],
            ),
            MethodBody => ruleset(
                "Option[FunctionBody]",
                [
                    rule([ term(KwDo).discard(), nonterm(BlockBodyWithEnd) ], "((e: WithSource[Expr]) => Some(FunctionBody.ExprBody(e)))"),
                    rule([ term(OpEquals).discard(), nonterm(NewLines).discard(), nonterm(Expression).with_location() ], "((e: WithSource[Expr]) => Some(FunctionBody.ExprBody(e)))"),
                    rule([ term(OpEquals).discard(), nonterm(NewLines).discard(), nonterm(ExternFunctionBody) ], "Some"),
                    rule([ term(OpEquals).discard(), nonterm(NewLines).discard(), term(KwAbstract).discard() ], "const(None)"),
                ],
            ).lex_mode("LexerMode.Normal"),
            FunctionBody => ruleset(
                "FunctionBody",
                [
                    rule([ term(KwDo).discard(), nonterm(BlockBodyWithEnd) ], "FunctionBody.ExprBody"),
                    rule([ term(OpEquals).discard(), nonterm(NewLines).discard(), nonterm(Expression).with_location() ], "FunctionBody.ExprBody"),
                    rule([ term(OpEquals).discard(), nonterm(NewLines).discard(), nonterm(ExternFunctionBody) ], "identity"),
                ],
            ).lex_mode("LexerMode.Normal"),
            ExternFunctionBody => ruleset(
                "FunctionBody",
                [
                    rule([ term(KwExtern).discard(), term(IdentifierToken).with_location() ], "((id: WithSource[Token.Identifier]) => FunctionBody.ExternBody(id.map(_.name)))"),
                ]
            ),
            RecordDeclarationStmtRest => ruleset(
                "Seq[WithSource[Modifier]] => RecordDeclarationStmt",
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
                        "((name: WithSource[IdentifierExpr], parameters: Seq[WithSource[FunctionParameterList]], typeAnnotation: Option[WithSource[Expr]], body: Seq[WithSource[RecordBodyStmt]]) => modifiers => RecordDeclarationStmt(modifiers, name, parameters, typeAnnotation, body))"
                    ),
                ],
            ),
            RecordBody => ruleset(
                "Seq[WithSource[RecordBodyStmt]]",
                [
                    rule([ nonterm(RecordBodyStmt).with_location() ], "((s: WithSource[RecordBodyStmt]) => Seq(s))"),
                    rule([ nonterm(RecordBodyStmt).with_location(), nonterm(StatementSeparator).discard(), nonterm(RecordBody) ], "((h: WithSource[RecordBodyStmt], t: Seq[WithSource[RecordBodyStmt]]) => h +: t)"),
                    rule([ nonterm(StatementSeparator).discard(), nonterm(RecordBody) ], "identity"),
                    rule([], "const(Seq.empty)"),
                ],
            ),
            RecordBodyStmt => ruleset(
                "RecordBodyStmt",
                [
                    rule([ nonterm(RecordField) ], "identity"),
                    rule([ nonterm(Modifiers), nonterm(MethodPurity), nonterm(MethodOrFunctionDeclarationStmtRest) ], "((modifiers: Seq[WithSource[Modifier]], methodPurity: Boolean, buildDecl: (Seq[WithSource[Modifier]], Boolean) => RecordBodyStmt) => buildDecl(modifiers, methodPurity))"),
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
                        "RecordField"
                    )
                ],
            ),
            EnumDeclarationStmtRest => ruleset(
                "Seq[WithSource[Modifier]] => Stmt",
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
                        "((name: WithSource[IdentifierExpr], parameters: Seq[WithSource[FunctionParameterList]], typeAnnotation: Option[WithSource[Expr]], body: Seq[WithSource[EnumBodyStmt]]) => modifiers => EnumDeclarationStmt(modifiers, name, parameters, typeAnnotation, body))"
                    ),
                ],
            ),
            EnumBody => ruleset(
                "Seq[WithSource[EnumBodyStmt]]",
                [
                    rule([ nonterm(EnumBodyStmt).with_location() ], "((s: WithSource[EnumBodyStmt]) => Seq(s))"),
                    rule([ nonterm(EnumBodyStmt).with_location(), nonterm(StatementSeparator).discard(), nonterm(EnumBody) ], "((h: WithSource[EnumBodyStmt], t: Seq[WithSource[EnumBodyStmt]]) => h +: t)"),
                    rule([ nonterm(StatementSeparator).discard(), nonterm(EnumBody) ], "identity"),
                    rule([], "const(Seq.empty)"),
                ],
            ),
            EnumBodyStmt => ruleset(
                "EnumBodyStmt",
                [
                    rule([ nonterm(Modifiers), nonterm(EnumConstructorVariant) ], "((modifiers: Seq[WithSource[Modifier]], f: Seq[WithSource[Modifier]] => EnumBodyStmt) => f(modifiers))"),
                    rule([ nonterm(Modifiers), nonterm(RecordDeclarationStmtRest) ], "((modifiers: Seq[WithSource[Modifier]], buildDecl: Seq[WithSource[Modifier]] => RecordDeclarationStmt) => EnumVariant.Record(buildDecl(modifiers)))"),
                    rule([ nonterm(Modifiers), nonterm(MethodPurity), nonterm(MethodOrFunctionDeclarationStmtRest) ], "((modifiers: Seq[WithSource[Modifier]], methodPurity: Boolean, buildDecl: (Seq[WithSource[Modifier]], Boolean) => EnumBodyStmt) => buildDecl(modifiers, methodPurity))"),
                ],
            ),
            EnumConstructorVariant => ruleset(
                "Seq[WithSource[Modifier]] => EnumBodyStmt",
                [
                    rule(
                        [
                            nonterm(Identifier).with_location(),
                            nonterm(MethodParameters),
                            nonterm(TypeDeclarationTypeAnnotation),
                        ],
                        "((name: WithSource[IdentifierExpr], parameters: Seq[WithSource[FunctionParameterList]], typeAnnotation: Option[WithSource[Expr]]) => (modifiers: Seq[WithSource[Modifier]]) => EnumVariant.Constructor(modifiers, name, parameters, typeAnnotation))",
                    ),
                ],
            ),
            TraitDeclarationStmtRest => ruleset(
                "Seq[WithSource[Modifier]] => Stmt",
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
                        "((name: WithSource[IdentifierExpr], parameters: Seq[WithSource[FunctionParameterList]], typeAnnotation: Option[WithSource[Expr]], body: Seq[WithSource[TraitBodyStmt]]) => modifiers => TraitDeclarationStmt(modifiers, name, parameters, typeAnnotation, body))"
                    ),
                ],
            ),
            TraitBody => ruleset(
                "Seq[WithSource[TraitBodyStmt]]",
                [
                    rule([ nonterm(TraitBodyStmt).with_location() ], "((s: WithSource[TraitBodyStmt]) => Seq(s))"),
                    rule([ nonterm(TraitBodyStmt).with_location(), nonterm(StatementSeparator).discard(), nonterm(TraitBody) ], "((h: WithSource[TraitBodyStmt], t: Seq[WithSource[TraitBodyStmt]]) => h +: t)"),
                    rule([ nonterm(StatementSeparator).discard(), nonterm(TraitBody) ], "identity"),
                    rule([], "const(Seq.empty)"),
                ],
            ),
            TraitBodyStmt => ruleset(
                "TraitBodyStmt",
                [
                    rule([ nonterm(Modifiers), nonterm(MethodPurity), nonterm(MethodOrFunctionDeclarationStmtRest) ], "((modifiers: Seq[WithSource[Modifier]], methodPurity: Boolean, buildDecl: (Seq[WithSource[Modifier]], Boolean) => TraitBodyStmt) => buildDecl(modifiers, methodPurity))"),
                ],
            ),
            NewTraitObjectBody => ruleset(
                "Seq[WithSource[NewTraitObjectBodyStmt]]",
                [
                    rule([ nonterm(NewTraitObjectBodyStmt).with_location() ], "((s: WithSource[NewTraitObjectBodyStmt]) => Seq(s))"),
                    rule([ nonterm(NewTraitObjectBodyStmt).with_location(), nonterm(StatementSeparator).discard(), nonterm(NewTraitObjectBody) ], "((h: WithSource[NewTraitObjectBodyStmt], t: Seq[WithSource[NewTraitObjectBodyStmt]]) => h +: t)"),
                    rule([ nonterm(StatementSeparator).discard(), nonterm(NewTraitObjectBody) ], "identity"),
                    rule([], "const(Seq.empty)"),
                ],
            ),
            NewTraitObjectBodyStmt => ruleset(
                "NewTraitObjectBodyStmt",
                [
                    rule([ nonterm(Modifiers), nonterm(MethodPurity), nonterm(MethodOrFunctionDeclarationStmtRest) ], "((modifiers: Seq[WithSource[Modifier]], methodPurity: Boolean, buildDecl: (Seq[WithSource[Modifier]], Boolean) => NewTraitObjectBodyStmt) => buildDecl(modifiers, methodPurity))"),
                ],
            ),
            TypeDeclarationTypeAnnotation => ruleset(
                "Option[WithSource[Expr]]",
                [
                    rule([], "const(None)"),
                    rule([ term(SymColon).discard(), nonterm(NewLines).discard(), nonterm(TypeBinding).with_location() ], "Some"),
                ],
            ),
            InstanceDeclarationStmtRest => ruleset(
                "Seq[WithSource[Modifier]] => Stmt",
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
                        "((name: WithSource[IdentifierExpr], parameters: Seq[WithSource[FunctionParameterList]], typeAnnotation: Option[WithSource[Expr]], body: Seq[WithSource[TraitBodyStmt]]) => modifiers => InstanceDeclarationStmt(modifiers, name, parameters, typeAnnotation, body))"
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
                "NonEmptySeq[String]",
                [
                    rule([ term(IdentifierToken) ], "((id: Token.Identifier) => NonEmptySeq.of(id.name))"),
                    rule([ term(IdentifierToken), term(SymDot).discard(), nonterm(TubeName) ], "((h: Token.Identifier, t: NonEmptySeq[String]) => h.name +: t)"),
                ],
            ),

            ExportStmt => ruleset(
                "ExportStmt",
                [
                    rule([ term(KwExport).discard(), nonterm(ImportPath).with_location() ], "ExportStmt"),
                ],
            ),

            ImportPath => ruleset(
                "ImportStmt",
                [
                    rule([ nonterm(TubeName), term(SymColonColon).discard(), nonterm(ImportPathSegment) ], "ImportStmt.Tube"),
                    rule([ term(SymColonColon).discard(), nonterm(ImportPathSegment) ], "ImportStmt.Absolute"),
                    rule([ term(SymAt).discard(), term(SymColonColon).discard(), nonterm(ImportPathRelativePrefix), nonterm(ImportPathSegment) ], "ImportStmt.Relative"),
                ],
            ),

            ImportPathRelativePrefix => ruleset(
                "Int",
                [
                    rule([ term(OpDotDot).discard(), term(SymColonColon).discard(), nonterm(ImportPathRelativePrefix) ], "((n: Int) => n + 1)"),
                    rule([], "const(0)"),
                ],
            ),

            ImportPathSegment => ruleset(
                "ImportPathSegment",
                [
                    rule([ term(IdentifierToken).with_location() ], "((id: WithSource[Token.Identifier]) => ImportPathSegment.Imported(id.map(id => IdentifierExpr.Named(id.name))))"),
                    rule([ nonterm(ComplexIdentifier).with_location() ], "ImportPathSegment.Imported"),
                    rule([ term(IdentifierToken).with_location(), term(SymAt).discard(), nonterm(IdentifierOptional).with_location() ], "((id: WithSource[Token.Identifier], viewedName: WithSource[Option[IdentifierExpr]]) => ImportPathSegment.Renaming(id.map(id => IdentifierExpr.Named(id.name)), viewedName))"),
                    rule([ nonterm(ComplexIdentifier).with_location(), term(SymAt).discard(), nonterm(IdentifierOptional).with_location() ], "ImportPathSegment.Renaming"),
                    rule([ term(IdentifierToken).with_location(), term(SymColonColon).discard(), nonterm(ImportPathSegment) ], "((id: WithSource[Token.Identifier], path: ImportPathSegment) => ImportPathSegment.Cons(id.value.name, path))"),
                    rule([ term(SymOpenCurly).discard(), nonterm(ImportPathMulti), term(SymCloseCurly).discard() ], "ImportPathSegment.Many"),
                    rule([ term(OpStar).with_location()], "((star: WithSource[?]) => ImportPathSegment.Wildcard(star.location))"),
                ],
            ),

            ImportPathMulti => ruleset(
                "Seq[ImportPathSegment]",
                [
                    rule([ nonterm(StatementSeparator).discard(), nonterm(ImportPathMulti) ], "identity"),
                    rule([ nonterm(ImportPathSegment) ], "Seq"),
                    rule([ nonterm(ImportPathSegment), nonterm(StatementSeparator).discard(), nonterm(ImportPathMulti) ], "((h: ImportPathSegment, t: Seq[ImportPathSegment]) => h +: t)"),
                    rule([], "const(Seq.empty)"),
                ],
            ),

            Statement => ruleset(
                "Stmt",
                [
                    rule([ nonterm(Expression) ], "identity"),
                    rule([ nonterm(ImportStmt) ], "identity"),
                    rule([ nonterm(ExportStmt) ], "identity"),
                    rule([ nonterm(Modifiers), nonterm(VariableDeclarationRest) ], "((modifiers: Seq[WithSource[Modifier]], buildDecl: Seq[WithSource[Modifier]] => Stmt) => buildDecl(modifiers))"),
                    rule([ nonterm(Modifiers), nonterm(MethodPurity), nonterm(MethodOrFunctionDeclarationStmtRest) ], "((modifiers: Seq[WithSource[Modifier]], methodPurity: Boolean, buildDecl: (Seq[WithSource[Modifier]], Boolean) => Stmt) => buildDecl(modifiers, methodPurity))"),
                    rule([ nonterm(Modifiers), nonterm(RecordDeclarationStmtRest) ], "((modifiers: Seq[WithSource[Modifier]], buildDecl: Seq[WithSource[Modifier]] => Stmt) => buildDecl(modifiers))"),
                    rule([ nonterm(Modifiers), nonterm(EnumDeclarationStmtRest) ], "((modifiers: Seq[WithSource[Modifier]], buildDecl: Seq[WithSource[Modifier]] => Stmt) => buildDecl(modifiers))"),
                    rule([ nonterm(Modifiers), nonterm(TraitDeclarationStmtRest) ], "((modifiers: Seq[WithSource[Modifier]], buildDecl: Seq[WithSource[Modifier]] => Stmt) => buildDecl(modifiers))"),
                    rule([ nonterm(Modifiers), nonterm(InstanceDeclarationStmtRest) ], "((modifiers: Seq[WithSource[Modifier]], buildDecl: Seq[WithSource[Modifier]] => Stmt) => buildDecl(modifiers))"),
                ],
            ),

            StatementList => ruleset(
                "Seq[WithSource[Stmt]]",
                [
                    rule([ nonterm(Statement).with_location() ], "((s: WithSource[Stmt]) => Seq(s))"),
                    rule([ nonterm(Statement).with_location(), nonterm(StatementSeparator).discard(), nonterm(StatementList) ], "((h: WithSource[Stmt], t: Seq[WithSource[Stmt]]) => h +: t)"),
                    rule([ nonterm(StatementSeparator).discard(), nonterm(StatementList) ], "identity"),
                    rule([], "const(Seq.empty)"),
                ],
            ),

            BlockBodyWithEnd => ruleset(
                "WithSource[Expr]",
                [
                    rule([ nonterm(BlockBody).with_location(), term(KwEnd).discard() ], "identity"),
                ],
            ).lex_mode("LexerMode.Normal"),

            BlockBody => ruleset(
                "Expr",
                [
                    rule([ nonterm(StatementList).with_location() ], "((body: WithSource[Seq[WithSource[Stmt]]]) => Expr.Block(body, None))"),
                    rule([ nonterm(StatementList).with_location(), term(KwFinally).discard(), nonterm(StatementList).with_location() ], "((body: WithSource[Seq[WithSource[Stmt]]], finallyBody: WithSource[Seq[WithSource[Stmt]]]) => Expr.Block(body, Some(finallyBody)))"),
                ],
            ),

            ModuleDeclaration => ruleset(
                "Seq[String]",
                [
                    rule([ term(KwModule).discard(), nonterm(ModulePath) ], "identity"),
                ],
            ),

            ModulePath => ruleset(
                "Seq[String]",
                [
                    rule([ term(KwUnderscore) ], "const(Seq.empty)"),
                    rule([ term(IdentifierToken), nonterm(ModulePath1) ], "((h: Token.Identifier, t: Seq[String]) => h.name +: t)"),
                ],
            ),

            ModulePath1 => ruleset(
                "Seq[String]",
                [
                    rule([], "const(Seq.empty)"),
                    rule([ term(SymColonColon).discard(), term(IdentifierToken), nonterm(ModulePath1) ], "((h: Token.Identifier, t: Seq[String]) => h.name +: t)"),
                ],
            ),
        }
    }
}




pub fn main() {

    let grammar = Grammar::make(ParserFactory);
    
    
    let scala_settings = ScalaSettings {
        package: "dev.argon.parser".to_owned(),
        access_modifier: "private[parser] ".to_owned(),
        class_name: "ArgonParserImpl".to_owned(),
        base_class: "ArgonParserBase".to_owned(),
        token_type: "Token".to_owned(),
        header_stmts: vec![
            "import dev.argon.ast.*".to_owned(),
            "import dev.argon.util.{WithSource, WithLocation}".to_owned(),
            "import cats.data.NonEmptySeq".to_owned(),
        ],
    };
    
    emit_scala(&mut std::io::stdout(), grammar, &scala_settings).unwrap();
}
