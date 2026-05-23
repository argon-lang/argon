use crate::ast;
use crate::ast::*;
use crate::lexer::{LexedToken, LexerMode, TokenReader};
use crate::token::{Token, TokenCategory};
use argon_util::{CompileError, ErrorReporter};
use num_bigint::BigInt;
use parse18_runtime::{
    FilePosition, FilePositionRange, Location, ParseResult, ParserRuntime, WithLocation, WithRange,
};
use std::path::{Path, PathBuf};

fn merge_locations(first: Location, second: Location) -> Location {
    Location {
        file: first.file,
        start: first.start,
        end: second.end,
    }
}

fn identity<A, B>(value: A) -> B
where
    A: Into<B>,
{
    value.into()
}

fn with_location_value<T>(value: WithLocation<T>) -> T {
    value.value
}

fn seq1<T>(head: T) -> Vec<T> {
    vec![head]
}

fn prepend<T>(head: T, mut tail: Vec<T>) -> Vec<T> {
    let mut values = Vec::with_capacity(tail.len() + 1);
    values.push(head);
    values.append(&mut tail);
    values
}

fn non_empty_single<T>(head: T) -> NonEmptyVec<T> {
    NonEmptyVec::new(head, Vec::new())
}

fn non_empty_prepend<T>(head: T, tail: NonEmptyVec<T>) -> NonEmptyVec<T> {
    let NonEmptyVec {
        head: tail_head,
        mut tail,
    } = tail;
    let mut values = Vec::with_capacity(tail.len() + 1);
    values.push(tail_head);
    values.append(&mut tail);
    NonEmptyVec::new(head, values)
}

fn declaration_stmt_to_stmt(decl: DeclarationStmt) -> Stmt {
    match decl {
        DeclarationStmt::Function(stmt) => Stmt::FunctionDeclaration(stmt),
        DeclarationStmt::Record(stmt) => Stmt::RecordDeclaration(stmt),
        DeclarationStmt::Enum(stmt) => Stmt::EnumDeclaration(stmt),
        DeclarationStmt::Trait(stmt) => Stmt::TraitDeclaration(stmt),
        DeclarationStmt::Method(stmt) => Stmt::MethodDeclaration(stmt),
        DeclarationStmt::Instance(stmt) => Stmt::InstanceDeclaration(stmt),
    }
}

fn declaration_stmt_to_record_body_stmt(decl: DeclarationStmt) -> RecordBodyStmt {
    match decl {
        DeclarationStmt::Function(stmt) => RecordBodyStmt::FunctionDeclaration(stmt),
        DeclarationStmt::Method(stmt) => RecordBodyStmt::MethodDeclaration(stmt),
        _ => panic!("unsupported declaration in record body"),
    }
}

fn declaration_stmt_to_enum_body_stmt(decl: DeclarationStmt) -> EnumBodyStmt {
    match decl {
        DeclarationStmt::Function(stmt) => EnumBodyStmt::FunctionDeclaration(stmt),
        DeclarationStmt::Method(stmt) => EnumBodyStmt::MethodDeclaration(stmt),
        _ => panic!("unsupported declaration in enum body"),
    }
}

fn declaration_stmt_to_trait_body_stmt(decl: DeclarationStmt) -> TraitBodyStmt {
    match decl {
        DeclarationStmt::Function(stmt) => TraitBodyStmt::FunctionDeclaration(stmt),
        DeclarationStmt::Method(stmt) => TraitBodyStmt::MethodDeclaration(stmt),
        _ => panic!("unsupported declaration in trait body"),
    }
}

fn declaration_stmt_to_new_trait_object_body_stmt(decl: DeclarationStmt) -> NewTraitObjectBodyStmt {
    match decl {
        DeclarationStmt::Function(stmt) => NewTraitObjectBodyStmt::FunctionDeclaration(stmt),
        DeclarationStmt::Method(stmt) => NewTraitObjectBodyStmt::MethodDeclaration(stmt),
        _ => panic!("unsupported declaration in new-trait-object body"),
    }
}

fn binary_op(
    a: WithLocation<Expr>,
    op: WithLocation<BinaryOperator>,
    b: WithLocation<Expr>,
) -> Expr {
    Expr::BinaryOperation {
        a: Box::new(a),
        op,
        b: Box::new(b),
    }
}

fn unary_op(op: WithLocation<UnaryOperator>, a: WithLocation<Expr>) -> Expr {
    Expr::UnaryOperation { op, a: Box::new(a) }
}

fn curried_call(
    func_expr: WithLocation<Expr>,
    args: Vec<(FunctionParameterListType, WithLocation<Expr>)>,
) -> Expr {
    let mut value = func_expr;

    for (list_type, arg_expr) in args {
        let call_location = merge_locations(value.location.clone(), arg_expr.location.clone());
        let call = expr_function_call(value, list_type, arg_expr);
        value = WithLocation::new(call, call_location);
    }

    value.value
}

fn simplify_fragments(fragments: Vec<StringFragment>) -> StringLiteral {
    let mut parts = Vec::new();

    for fragment in fragments {
        match fragment {
            StringFragment::Text(text) => {
                if text.is_empty() {
                    continue;
                }

                if let Some(StringFragment::Text(prev)) = parts.last_mut() {
                    prev.push_str(&text);
                } else {
                    parts.push(StringFragment::Text(text));
                }
            }
            fragment => parts.push(fragment),
        }
    }

    StringLiteral { parts }
}

fn token_identifier_name(token: Token) -> String {
    match token {
        Token::Identifier { name } => name.into(),
        _ => panic!("expected identifier token"),
    }
}

fn token_int_bigint(token: Token) -> BigInt {
    match token {
        Token::IntToken { value } => value.into(),
        _ => panic!("expected int token"),
    }
}

fn token_string_text(token: Token) -> String {
    match token {
        Token::StringText { text } => text.into(),
        _ => panic!("expected string text token"),
    }
}

fn identifier_expr_named_token(token: Token) -> Identifier {
    identifier_expr_named(token_identifier_name(token))
}

fn if_expr_from_cond_apply(
    cond: WithLocation<Expr>,
    rest: Box<dyn FnOnce(WithLocation<Expr>) -> Expr>,
) -> Expr {
    rest(cond)
}

fn if_expr_after_then_end(
    then_body: WithLocation<Vec<WithLocation<Stmt>>>,
) -> Box<dyn FnOnce(WithLocation<Expr>) -> Expr> {
    Box::new(move |cond| {
        let else_body = WithLocation::new(Vec::new(), then_body.location.clone());
        expr_if_else(cond, then_body, else_body)
    })
}

fn if_expr_after_then_else(
    then_body: WithLocation<Vec<WithLocation<Stmt>>>,
    else_body: WithLocation<Vec<WithLocation<Stmt>>>,
) -> Box<dyn FnOnce(WithLocation<Expr>) -> Expr> {
    Box::new(move |cond| expr_if_else(cond, then_body, else_body))
}

fn if_expr_after_then_elsif(
    then_body: WithLocation<Vec<WithLocation<Stmt>>>,
    else_expr: WithLocation<Expr>,
) -> Box<dyn FnOnce(WithLocation<Expr>) -> Expr> {
    Box::new(move |cond| {
        let else_location = else_expr.location.clone();
        let else_stmt = WithLocation::new(stmt_expr(else_expr), else_location.clone());
        let else_body = WithLocation::new(vec![else_stmt], else_location);
        expr_if_else(cond, then_body, else_body)
    })
}

fn expr_while_no_body(
    label: Option<WithLocation<Identifier>>,
    cond: WithLocation<Vec<WithLocation<Stmt>>>,
    end_keyword: WithLocation<Token>,
) -> Expr {
    expr_while(
        label,
        cond,
        WithLocation::new(Vec::new(), end_keyword.location),
    )
}

fn string_fragment_text_token(token: Token) -> StringFragment {
    string_fragment_text(token_string_text(token))
}

fn expr_int_literal_token(token: Token) -> Expr {
    expr_int_literal(token_int_bigint(token))
}

fn expr_big_type_token(token: Token) -> Expr {
    expr_big_type(token_int_bigint(token))
}

fn expr_builtin_token(token: Token) -> Expr {
    expr_builtin(token_identifier_name(token))
}

fn enclosed_arg_list_paren_empty(
    close_paren: WithLocation<Token>,
) -> (FunctionParameterListType, WithLocation<Expr>) {
    (
        FunctionParameterListType::NormalList,
        WithLocation::new(expr_tuple(Vec::new()), close_paren.location),
    )
}

fn enclosed_arg_list_square_empty(
    close_bracket: WithLocation<Token>,
) -> (FunctionParameterListType, WithLocation<Expr>) {
    (
        FunctionParameterListType::InferrableList,
        WithLocation::new(expr_tuple(Vec::new()), close_bracket.location),
    )
}

fn pattern_int_token(token: Token) -> Pattern {
    pattern_int(token_int_bigint(token))
}

fn pattern_binding_discard(mut_spec: bool, id: WithLocation<Identifier>) -> Pattern {
    let location = id.location.clone();
    pattern_binding(mut_spec, id, WithLocation::new(pattern_discard(), location))
}

fn pattern_argument_from_path(path: WithLocation<PatternPath>) -> PatternArgument {
    let location = path.location.clone();
    let pattern = WithLocation::new(pattern_constructor(path, Vec::new()), location);
    pattern_argument(FunctionParameterListType::NormalList, pattern)
}

fn variable_declaration_rest_builder(
    binding: (bool, Option<Identifier>, Option<WithLocation<Expr>>),
    value: WithLocation<Expr>,
) -> Box<dyn FnOnce(Vec<WithLocation<Modifier>>) -> Stmt> {
    let (is_mutable, id, type_annotation) = binding;
    Box::new(move |modifiers| {
        variable_declaration_stmt(modifiers, is_mutable, id, type_annotation, value)
    })
}

fn method_declaration_stmt_rest_from_named_instance(
    instance_name: WithLocation<Identifier>,
    name: WithLocation<Identifier>,
    parameters: Vec<WithLocation<FunctionParameterList>>,
    return_type: WithLocation<ReturnTypeSpecifier>,
    body: Option<FunctionBody>,
) -> Box<dyn FnOnce((Vec<WithLocation<Modifier>>, bool)) -> DeclarationStmt> {
    Box::new(move |(modifiers, purity)| {
        method_declaration_stmt(
            modifiers,
            purity,
            instance_name.map(Some),
            None,
            name,
            parameters,
            return_type,
            body,
        )
    })
}

fn method_declaration_stmt_rest_from_discard_instance(
    instance_name: WithLocation<Token>,
    name: WithLocation<Identifier>,
    parameters: Vec<WithLocation<FunctionParameterList>>,
    return_type: WithLocation<ReturnTypeSpecifier>,
    body: Option<FunctionBody>,
) -> Box<dyn FnOnce((Vec<WithLocation<Modifier>>, bool)) -> DeclarationStmt> {
    Box::new(move |(modifiers, purity)| {
        method_declaration_stmt(
            modifiers,
            purity,
            instance_name.map(|_| None),
            None,
            name,
            parameters,
            return_type,
            body,
        )
    })
}

fn method_declaration_stmt_rest_function(
    name: WithLocation<Identifier>,
    parameters: Vec<WithLocation<FunctionParameterList>>,
    return_type: WithLocation<ReturnTypeSpecifier>,
    body: FunctionBody,
) -> Box<dyn FnOnce((Vec<WithLocation<Modifier>>, bool)) -> DeclarationStmt> {
    Box::new(move |(modifiers, purity)| {
        function_declaration_stmt(modifiers, purity, name, parameters, return_type, body)
    })
}

fn function_body_extern_body_token(id: WithLocation<Token>) -> FunctionBody {
    function_body_extern_body(id.map(token_identifier_name))
}

fn record_declaration_stmt_rest_builder(
    name: WithLocation<Identifier>,
    parameters: Vec<WithLocation<FunctionParameterList>>,
    type_annotation: Option<WithLocation<Expr>>,
    body: Vec<WithLocation<RecordBodyStmt>>,
) -> Box<dyn FnOnce(Vec<WithLocation<Modifier>>) -> RecordDeclarationStmt> {
    Box::new(move |modifiers| {
        record_declaration_stmt(modifiers, name, parameters, type_annotation, body)
    })
}

fn record_body_stmt_record_field(field: RecordField) -> RecordBodyStmt {
    RecordBodyStmt::RecordField(Box::new(field))
}

fn enum_declaration_stmt_rest_builder(
    name: WithLocation<Identifier>,
    parameters: Vec<WithLocation<FunctionParameterList>>,
    type_annotation: Option<WithLocation<Expr>>,
    body: Vec<WithLocation<EnumBodyStmt>>,
) -> Box<dyn FnOnce(Vec<WithLocation<Modifier>>) -> Stmt> {
    Box::new(move |modifiers| {
        enum_declaration_stmt(modifiers, name, parameters, type_annotation, body)
    })
}

fn enum_body_stmt_from_variant_builder(
    modifiers: Vec<WithLocation<Modifier>>,
    builder: Box<dyn FnOnce(Vec<WithLocation<Modifier>>) -> EnumBodyStmt>,
) -> EnumBodyStmt {
    builder(modifiers)
}

fn enum_body_stmt_from_record_builder(
    modifiers: Vec<WithLocation<Modifier>>,
    build_decl: Box<dyn FnOnce(Vec<WithLocation<Modifier>>) -> RecordDeclarationStmt>,
) -> EnumBodyStmt {
    enum_variant_record(build_decl(modifiers))
}

fn enum_body_stmt_from_declaration_builder(
    modifiers: Vec<WithLocation<Modifier>>,
    method_purity: bool,
    build_decl: Box<dyn FnOnce((Vec<WithLocation<Modifier>>, bool)) -> DeclarationStmt>,
) -> EnumBodyStmt {
    declaration_stmt_to_enum_body_stmt(build_decl((modifiers, method_purity)))
}

fn enum_constructor_variant_builder(
    name: WithLocation<Identifier>,
    parameters: Vec<WithLocation<FunctionParameterList>>,
    type_annotation: Option<WithLocation<Expr>>,
) -> Box<dyn FnOnce(Vec<WithLocation<Modifier>>) -> EnumBodyStmt> {
    Box::new(move |modifiers| {
        enum_variant_constructor(modifiers, name, parameters, type_annotation)
    })
}

fn trait_declaration_stmt_rest_builder(
    name: WithLocation<Identifier>,
    parameters: Vec<WithLocation<FunctionParameterList>>,
    type_annotation: Option<WithLocation<Expr>>,
    body: Vec<WithLocation<TraitBodyStmt>>,
) -> Box<dyn FnOnce(Vec<WithLocation<Modifier>>) -> Stmt> {
    Box::new(move |modifiers| {
        trait_declaration_stmt(modifiers, name, parameters, type_annotation, body)
    })
}

fn instance_declaration_stmt_rest_builder(
    name: WithLocation<Identifier>,
    parameters: Vec<WithLocation<FunctionParameterList>>,
    type_annotation: Option<WithLocation<Expr>>,
    body: Vec<WithLocation<TraitBodyStmt>>,
) -> Box<dyn FnOnce(Vec<WithLocation<Modifier>>) -> Stmt> {
    Box::new(move |modifiers| {
        instance_declaration_stmt(modifiers, name, parameters, type_annotation, body)
    })
}

fn tube_name_single(token: Token) -> NonEmptyVec<String> {
    non_empty_single(token_identifier_name(token))
}

fn tube_name_prepend(token: Token, tail: NonEmptyVec<String>) -> NonEmptyVec<String> {
    non_empty_prepend(token_identifier_name(token), tail)
}

fn import_path_segment_imported_token(id: WithLocation<Token>) -> ImportPathSegment {
    import_path_segment_imported(id.map(identifier_expr_named_token))
}

fn import_path_segment_renaming_token(
    id: WithLocation<Token>,
    viewed_name: WithLocation<Option<Identifier>>,
) -> ImportPathSegment {
    import_path_segment_renaming(id.map(identifier_expr_named_token), viewed_name)
}

fn import_path_segment_cons_token(
    id: WithLocation<Token>,
    path: ImportPathSegment,
) -> ImportPathSegment {
    import_path_segment_cons(token_identifier_name(id.value), path)
}

fn import_path_segment_wildcard_token(star: WithLocation<Token>) -> ImportPathSegment {
    import_path_segment_wildcard(star.location)
}

fn stmt_expr(expr: WithLocation<Expr>) -> Stmt {
    Stmt::Expr(expr)
}

fn stmt_expr_error() -> Stmt {
    let position = FilePosition { line: 0, column: 0 };
    Stmt::Expr(WithLocation::new(
        expr_error(),
        Location {
            file: PathBuf::new(),
            start: position,
            end: position,
        },
    ))
}

fn stmt_import(import_stmt: ImportStmt) -> Stmt {
    Stmt::Import(Box::new(import_stmt))
}

fn stmt_export(export_stmt: ExportStmt) -> Stmt {
    Stmt::Export(Box::new(export_stmt))
}

fn statement_apply_builder(
    modifiers: Vec<WithLocation<Modifier>>,
    build_decl: Box<dyn FnOnce(Vec<WithLocation<Modifier>>) -> Stmt>,
) -> Stmt {
    build_decl(modifiers)
}

fn statement_declaration_builder(
    modifiers: Vec<WithLocation<Modifier>>,
    method_purity: bool,
    build_decl: Box<dyn FnOnce((Vec<WithLocation<Modifier>>, bool)) -> DeclarationStmt>,
) -> Stmt {
    declaration_stmt_to_stmt(build_decl((modifiers, method_purity)))
}

fn statement_record_builder(
    modifiers: Vec<WithLocation<Modifier>>,
    build_decl: Box<dyn FnOnce(Vec<WithLocation<Modifier>>) -> RecordDeclarationStmt>,
) -> Stmt {
    Stmt::RecordDeclaration(Box::new(build_decl(modifiers)))
}

fn record_body_stmt_from_declaration_builder(
    modifiers: Vec<WithLocation<Modifier>>,
    method_purity: bool,
    build_decl: Box<dyn FnOnce((Vec<WithLocation<Modifier>>, bool)) -> DeclarationStmt>,
) -> RecordBodyStmt {
    declaration_stmt_to_record_body_stmt(build_decl((modifiers, method_purity)))
}

fn trait_body_stmt_from_declaration_builder(
    modifiers: Vec<WithLocation<Modifier>>,
    method_purity: bool,
    build_decl: Box<dyn FnOnce((Vec<WithLocation<Modifier>>, bool)) -> DeclarationStmt>,
) -> TraitBodyStmt {
    declaration_stmt_to_trait_body_stmt(build_decl((modifiers, method_purity)))
}

fn new_trait_object_body_stmt_from_declaration_builder(
    modifiers: Vec<WithLocation<Modifier>>,
    method_purity: bool,
    build_decl: Box<dyn FnOnce((Vec<WithLocation<Modifier>>, bool)) -> DeclarationStmt>,
) -> NewTraitObjectBodyStmt {
    declaration_stmt_to_new_trait_object_body_stmt(build_decl((modifiers, method_purity)))
}

fn module_path_prepend(token: Token, tail: Vec<String>) -> Vec<String> {
    prepend(token_identifier_name(token), tail)
}

fn expr_error() -> Expr {
    Expr::Error
}

fn expr_as(value: WithLocation<Expr>, value_type: WithLocation<Expr>) -> Expr {
    Expr::As {
        value: Box::new(value),
        value_type: Box::new(value_type),
    }
}

fn expr_assert(t: WithLocation<Expr>) -> Expr {
    Expr::Assert { t: Box::new(t) }
}

fn expr_block(
    body: WithLocation<Vec<WithLocation<Stmt>>>,
    finally_body: Option<WithLocation<Vec<WithLocation<Stmt>>>>,
) -> Expr {
    Expr::Block { body, finally_body }
}

fn expr_bool_literal(value: bool) -> Expr {
    Expr::BoolLiteral(value)
}

fn expr_builtin(value: impl Into<String>) -> Expr {
    Expr::Builtin(value.into())
}

fn expr_identifier(id: Identifier) -> Expr {
    Expr::Identifier(id)
}

fn expr_dot(o: WithLocation<Expr>, member: WithLocation<Identifier>) -> Expr {
    Expr::Dot {
        o: Box::new(o),
        member,
    }
}

fn expr_function_literal(parameter_name: Option<Identifier>, body: WithLocation<Expr>) -> Expr {
    Expr::FunctionLiteral {
        parameter_name,
        body: Box::new(body),
    }
}

fn expr_function_call(
    func: WithLocation<Expr>,
    list_type: FunctionParameterListType,
    arg: WithLocation<Expr>,
) -> Expr {
    Expr::FunctionCall {
        func: Box::new(func),
        list_type,
        arg: Box::new(arg),
    }
}

fn expr_function_type(a: WithLocation<Expr>, r: WithLocation<Expr>) -> Expr {
    Expr::FunctionType {
        a: Box::new(a),
        r: Box::new(r),
    }
}

fn expr_if_else(
    condition: WithLocation<Expr>,
    when_true: WithLocation<Vec<WithLocation<Stmt>>>,
    when_false: WithLocation<Vec<WithLocation<Stmt>>>,
) -> Expr {
    Expr::IfElse {
        condition: Box::new(condition),
        when_true,
        when_false,
    }
}

fn expr_int_literal<T>(value: T) -> Expr
where
    T: Into<BigInt>,
{
    Expr::IntLiteral(value.into())
}

fn expr_is(value: WithLocation<Expr>, pattern: WithLocation<Pattern>) -> Expr {
    Expr::Is {
        value: Box::new(value),
        pattern,
    }
}

fn expr_loop(
    label: Option<WithLocation<Identifier>>,
    body: WithLocation<Vec<WithLocation<Stmt>>>,
) -> Expr {
    Expr::Loop { label, body }
}

fn expr_match(value: WithLocation<Expr>, cases: Vec<WithLocation<MatchCase>>) -> Expr {
    Expr::Match {
        value: Box::new(value),
        cases,
    }
}

fn expr_new_trait_object(
    trait_expr: WithLocation<Expr>,
    body: Vec<WithLocation<NewTraitObjectBodyStmt>>,
) -> Expr {
    Expr::NewTraitObject {
        trait_expr: Box::new(trait_expr),
        body,
    }
}

fn expr_next(label: Option<WithLocation<Identifier>>) -> Expr {
    Expr::Next { label }
}

fn expr_raise(ex: WithLocation<Expr>) -> Expr {
    Expr::Raise { ex: Box::new(ex) }
}

fn expr_record_literal(
    record_expr: WithLocation<Expr>,
    fields: WithLocation<Vec<WithLocation<ast::RecordFieldLiteral>>>,
) -> Expr {
    Expr::RecordLiteral {
        record_expr: Box::new(record_expr),
        fields,
    }
}

fn expr_redo(label: Option<WithLocation<Identifier>>) -> Expr {
    Expr::Redo { label }
}

fn expr_string_literal(value: StringLiteral) -> Expr {
    Expr::StringLiteral(value)
}

fn expr_tuple(items: Vec<WithLocation<Expr>>) -> Expr {
    Expr::Tuple { items }
}

fn expr_type() -> Expr {
    Expr::Type
}

fn expr_big_type<T>(value: T) -> Expr
where
    T: Into<BigInt>,
{
    Expr::BigType(value.into())
}

fn expr_while(
    label: Option<WithLocation<Identifier>>,
    condition: WithLocation<Vec<WithLocation<Stmt>>>,
    body: WithLocation<Vec<WithLocation<Stmt>>>,
) -> Expr {
    Expr::While {
        label,
        condition,
        body,
    }
}

fn expr_boxed_type(t: WithLocation<Expr>) -> Expr {
    Expr::BoxedType { t: Box::new(t) }
}

fn expr_box(value: WithLocation<Expr>) -> Expr {
    Expr::Box {
        value: Box::new(value),
    }
}

fn expr_unbox(value: WithLocation<Expr>) -> Expr {
    Expr::Unbox {
        value: Box::new(value),
    }
}

fn expr_break(label: Option<WithLocation<Identifier>>) -> Expr {
    Expr::Break { label }
}

fn identifier_expr_named(name: impl Into<String>) -> Identifier {
    Identifier::Named(name.into())
}

fn identifier_expr_extension(id: Identifier) -> Identifier {
    Identifier::Extension(Box::new(id))
}

fn identifier_expr_inverse(id: Identifier) -> Identifier {
    Identifier::Inverse(Box::new(id))
}

fn identifier_expr_update(id: Identifier) -> Identifier {
    Identifier::Update(Box::new(id))
}

fn function_body_expr_body(body: WithLocation<Expr>) -> FunctionBody {
    FunctionBody::ExprBody(Box::new(body))
}

fn function_body_extern_body(body: WithLocation<String>) -> FunctionBody {
    FunctionBody::ExternBody(body)
}

fn string_fragment_text(text: impl Into<String>) -> StringFragment {
    StringFragment::Text(text.into())
}

fn string_fragment_interpolate(value: WithLocation<Expr>) -> StringFragment {
    StringFragment::Interpolate {
        value: Box::new(value),
    }
}

fn pattern_discard() -> Pattern {
    Pattern::Discard
}

fn pattern_tuple(elements: Vec<WithLocation<Pattern>>) -> Pattern {
    Pattern::Tuple { elements }
}

fn pattern_binding(
    is_mutable: bool,
    name: WithLocation<Identifier>,
    pattern: WithLocation<Pattern>,
) -> Pattern {
    Pattern::Binding {
        is_mutable,
        name,
        pattern: Box::new(pattern),
    }
}

fn pattern_constructor(path: WithLocation<PatternPath>, args: Vec<PatternArgument>) -> Pattern {
    Pattern::Constructor { path, args }
}

fn pattern_string(value: StringLiteral) -> Pattern {
    Pattern::String(value)
}

fn pattern_int<T>(value: T) -> Pattern
where
    T: Into<BigInt>,
{
    Pattern::Int(value.into())
}

fn pattern_bool(value: bool) -> Pattern {
    Pattern::Bool(value)
}

fn pattern_path_base(name: WithLocation<Identifier>) -> PatternPath {
    PatternPath::Base { name }
}

fn pattern_path_member(
    base: WithLocation<PatternPath>,
    member: WithLocation<Identifier>,
) -> PatternPath {
    PatternPath::Member {
        base: Box::new(base),
        member,
    }
}

fn pattern_argument(
    function_parameter_list_type: FunctionParameterListType,
    arg: WithLocation<Pattern>,
) -> PatternArgument {
    PatternArgument {
        function_parameter_list_type,
        arg,
    }
}

fn function_parameter(param_type: WithLocation<Expr>, name: Identifier) -> FunctionParameter {
    FunctionParameter { param_type, name }
}

fn function_parameter_list(
    list_type: FunctionParameterListType,
    modifiers: Vec<WithLocation<Modifier>>,
    parameters: Vec<WithLocation<FunctionParameter>>,
    has_trailing_comma: bool,
) -> FunctionParameterList {
    FunctionParameterList {
        list_type,
        modifiers,
        parameters,
        has_trailing_comma,
    }
}

fn return_type_specifier(
    return_type: WithLocation<Expr>,
    ensures_clauses: Vec<WithLocation<Expr>>,
) -> ReturnTypeSpecifier {
    ReturnTypeSpecifier {
        return_type,
        ensures_clauses,
    }
}

fn variable_declaration_stmt(
    modifiers: Vec<WithLocation<Modifier>>,
    is_mutable: bool,
    name: Option<Identifier>,
    var_type: Option<WithLocation<Expr>>,
    value: WithLocation<Expr>,
) -> Stmt {
    Stmt::VariableDeclaration(Box::new(ast::VariableDeclarationStmt {
        modifiers,
        is_mutable,
        name,
        var_type,
        value,
    }))
}

fn function_declaration_stmt(
    modifiers: Vec<WithLocation<Modifier>>,
    purity: bool,
    name: WithLocation<Identifier>,
    parameters: Vec<WithLocation<FunctionParameterList>>,
    return_type: WithLocation<ReturnTypeSpecifier>,
    body: FunctionBody,
) -> DeclarationStmt {
    DeclarationStmt::Function(Box::new(ast::FunctionDeclarationStmt {
        modifiers,
        purity,
        name,
        parameters,
        return_type,
        body,
    }))
}

fn method_declaration_stmt(
    modifiers: Vec<WithLocation<Modifier>>,
    purity: bool,
    instance_name: WithLocation<Option<Identifier>>,
    instance_type: Option<WithLocation<Expr>>,
    name: WithLocation<Identifier>,
    parameters: Vec<WithLocation<FunctionParameterList>>,
    return_type: WithLocation<ReturnTypeSpecifier>,
    body: Option<FunctionBody>,
) -> DeclarationStmt {
    DeclarationStmt::Method(Box::new(ast::MethodDeclarationStmt {
        modifiers,
        purity,
        instance_name,
        instance_type,
        name,
        parameters,
        return_type,
        body,
    }))
}

fn record_declaration_stmt(
    modifiers: Vec<WithLocation<Modifier>>,
    name: WithLocation<Identifier>,
    parameters: Vec<WithLocation<FunctionParameterList>>,
    return_type: Option<WithLocation<Expr>>,
    body: Vec<WithLocation<RecordBodyStmt>>,
) -> RecordDeclarationStmt {
    ast::RecordDeclarationStmt {
        modifiers,
        name,
        parameters,
        return_type,
        body,
    }
}

fn enum_declaration_stmt(
    modifiers: Vec<WithLocation<Modifier>>,
    name: WithLocation<Identifier>,
    parameters: Vec<WithLocation<FunctionParameterList>>,
    return_type: Option<WithLocation<Expr>>,
    body: Vec<WithLocation<EnumBodyStmt>>,
) -> Stmt {
    Stmt::EnumDeclaration(Box::new(ast::EnumDeclarationStmt {
        modifiers,
        name,
        parameters,
        return_type,
        body,
    }))
}

fn trait_declaration_stmt(
    modifiers: Vec<WithLocation<Modifier>>,
    name: WithLocation<Identifier>,
    parameters: Vec<WithLocation<FunctionParameterList>>,
    return_type: Option<WithLocation<Expr>>,
    body: Vec<WithLocation<TraitBodyStmt>>,
) -> Stmt {
    Stmt::TraitDeclaration(Box::new(ast::TraitDeclarationStmt {
        modifiers,
        name,
        parameters,
        return_type,
        body,
    }))
}

fn instance_declaration_stmt(
    modifiers: Vec<WithLocation<Modifier>>,
    name: WithLocation<Identifier>,
    parameters: Vec<WithLocation<FunctionParameterList>>,
    return_type: Option<WithLocation<Expr>>,
    body: Vec<WithLocation<TraitBodyStmt>>,
) -> Stmt {
    Stmt::InstanceDeclaration(Box::new(ast::InstanceDeclarationStmt {
        modifiers,
        name,
        parameters,
        return_type,
        body,
    }))
}

fn record_field(
    is_mutable: bool,
    name: WithLocation<Identifier>,
    field_type: WithLocation<Expr>,
) -> RecordField {
    ast::RecordField {
        is_mutable,
        name,
        field_type,
    }
}

fn enum_variant_constructor(
    modifiers: Vec<WithLocation<Modifier>>,
    name: WithLocation<Identifier>,
    parameters: Vec<WithLocation<FunctionParameterList>>,
    return_type: Option<WithLocation<Expr>>,
) -> EnumBodyStmt {
    EnumBodyStmt::EnumVariant(Box::new(EnumVariant::Constructor {
        modifiers,
        name,
        parameters,
        return_type,
    }))
}

fn enum_variant_record(record: RecordDeclarationStmt) -> EnumBodyStmt {
    EnumBodyStmt::EnumVariant(Box::new(EnumVariant::Record(record)))
}

fn record_field_literal(
    name: WithLocation<Identifier>,
    value: WithLocation<Expr>,
) -> RecordFieldLiteral {
    ast::RecordFieldLiteral { name, value }
}

fn match_case(pattern: WithLocation<Pattern>, body: WithLocation<Expr>) -> MatchCase {
    ast::MatchCase { pattern, body }
}

fn module_declaration(
    module_path: Vec<String>,
    stmts: Vec<WithLocation<Stmt>>,
) -> ModuleDeclaration {
    ast::ModuleDeclaration { module_path, stmts }
}

fn export_stmt(from_import: WithLocation<ImportStmt>) -> ExportStmt {
    ast::ExportStmt { from_import }
}

fn import_stmt_absolute(path: ImportPathSegment) -> ImportStmt {
    ImportStmt::Absolute(path)
}

fn import_stmt_relative(up_count: usize, path: ImportPathSegment) -> ImportStmt {
    ImportStmt::Relative { up_count, path }
}

fn import_stmt_tube(tube_name: NonEmptyVec<String>, path: ImportPathSegment) -> ImportStmt {
    ImportStmt::Tube { tube_name, path }
}

fn import_path_segment_cons(
    id: impl Into<String>,
    sub_path: ImportPathSegment,
) -> ImportPathSegment {
    ImportPathSegment::Cons {
        id: id.into(),
        sub_path: Box::new(sub_path),
    }
}

fn import_path_segment_many(segments: Vec<ImportPathSegment>) -> ImportPathSegment {
    ImportPathSegment::Many { segments }
}

fn import_path_segment_renaming(
    importing: WithLocation<Identifier>,
    viewed_name: WithLocation<Option<Identifier>>,
) -> ImportPathSegment {
    ImportPathSegment::Renaming {
        importing,
        viewed_name,
    }
}

fn import_path_segment_imported(id: WithLocation<Identifier>) -> ImportPathSegment {
    ImportPathSegment::Imported { id }
}

fn import_path_segment_wildcard(location: Location) -> ImportPathSegment {
    ImportPathSegment::Wildcard { location }
}

include!(concat!(env!("OUT_DIR"), "/argon_parser.rs"));

pub struct ArgonParser<'a, L, ER: ?Sized> {
    file_name: &'a Path,
    lex_mode: LexerMode,

    lexer: L,
    error_reporter: &'a ER,

    last_token_end_position: FilePosition,
    peek_token: Option<WithRange<Token>>,
}

impl<'a, L: TokenReader, ER: ErrorReporter<CompileError> + ?Sized> ArgonParser<'a, L, ER> {
    pub fn new(file_name: &'a Path, lexer: L, error_reporter: &'a ER) -> ArgonParser<'a, L, ER> {
        ArgonParser {
            file_name,
            lex_mode: LexerMode::Normal,
            lexer,
            error_reporter,
            last_token_end_position: FilePosition { line: 1, column: 1 },
            peek_token: None,
        }
    }
}

impl<'a, L: TokenReader, ER: ErrorReporter<CompileError> + ?Sized> ParserRuntime
    for ArgonParser<'a, L, ER>
{
    type Token = Token;
    type TokenCategory = TokenCategory;
    type LexMode = LexerMode;

    fn peek(&mut self) -> ParseResult<Self::Token> {
        if let Some(token) = &self.peek_token {
            ParseResult::Success(token.clone())
        } else {
            match self.lexer.next_token(self.lex_mode) {
                LexedToken::Token(token) => {
                    self.peek_token = Some(token.clone());
                    ParseResult::Success(token)
                }
                LexedToken::EndOfFile(pos) => ParseResult::Success(WithRange {
                    value: Token::EndOfFile,
                    range: FilePositionRange {
                        start: pos,
                        end: pos,
                    },
                }),
            }
        }
    }

    fn parser_next(&mut self) -> ParseResult<Self::Token> {
        if let Some(token) = &self.peek_token
            && token.value == Token::EndOfFile
        {
            ParseResult::Success(token.clone())
        } else if let Some(token) = self.peek_token.take() {
            self.last_token_end_position = token.range.end;
            ParseResult::Success(token)
        } else {
            let token = match self.lexer.next_token(self.lex_mode) {
                LexedToken::Token(token) => {
                    self.last_token_end_position = token.range.end;
                    token
                }
                LexedToken::EndOfFile(pos) => {
                    self.last_token_end_position = pos;

                    let token = WithRange {
                        value: Token::EndOfFile,
                        range: FilePositionRange {
                            start: pos,
                            end: pos,
                        },
                    };

                    self.peek_token = Some(token.clone());

                    token
                }
            };

            ParseResult::Success(token)
        }
    }

    fn error<T>(&mut self, rule_name: &str, categories: &[Self::TokenCategory]) -> ParseResult<T> {
        if let ParseResult::Success(token) = self.peek() {
            self.error_reporter.report_error(CompileError::parse_error(
                Location::from_range(self.file_name.to_owned(), token.range),
                rule_name,
                format!("{:?}", token.value),
                categories.iter().map(|category| format!("{category:?}")),
            ));
        }

        ParseResult::Failure
    }

    fn with_lex_mode<T>(
        &mut self,
        lex_mode: Self::LexMode,
        f: impl FnOnce(&mut Self) -> ParseResult<T>,
    ) -> ParseResult<T> {
        let old_lex_mode = self.lex_mode;
        self.lex_mode = lex_mode;
        let res = f(self);
        self.lex_mode = old_lex_mode;
        res
    }

    fn recover_with<T>(
        &mut self,
        parse_res: ParseResult<T>,
        fallback: impl FnOnce() -> T,
    ) -> ParseResult<T> {
        match parse_res {
            ParseResult::Success(_) => parse_res,
            ParseResult::Failure => {
                let range = FilePositionRange {
                    start: self.last_token_end_position,
                    end: self.last_token_end_position,
                };
                ParseResult::Success(WithRange::new(fallback(), range))
            }
        }
    }

    fn with_location<T>(&self, parse_res: ParseResult<T>) -> ParseResult<WithLocation<T>> {
        match parse_res {
            ParseResult::Success(value) => {
                let location = Location::from_range(self.file_name.to_owned(), value.range);
                ParseResult::Success(value.map(move |value| WithLocation::new(value, location)))
            }
            ParseResult::Failure => ParseResult::Failure,
        }
    }
}
