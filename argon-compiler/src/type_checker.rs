use crate::scope::{LocalScope, Lookup, Scope};
use crate::{Context, DefaultExprContext};
use argon_expr::{Builtin, Expr, ExprContext, Variable};
use argon_parser::ast;
use argon_parser::ast::{IdentifierExpr, StringFragment};
use argon_util::{CompileError, ErrorReporter, UniqueIdentifier};
use parse18_runtime::{Location, WithLocation};
use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};

#[derive(Debug, Clone)]
struct TypeCheckExprContext {}

impl ExprContext for TypeCheckExprContext {
    type Hole = Hole;
    type Function = <DefaultExprContext as ExprContext>::Function;
    type Method = <DefaultExprContext as ExprContext>::Method;
    type Record = <DefaultExprContext as ExprContext>::Record;
    type Enum = <DefaultExprContext as ExprContext>::Enum;
    type EnumCase = <DefaultExprContext as ExprContext>::EnumCase;
    type Trait = <DefaultExprContext as ExprContext>::Trait;
}

#[derive(Debug, Clone)]
struct Hole(UniqueIdentifier, Box<Expr<TypeCheckExprContext>>);

impl PartialEq for Hole {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl Eq for Hole {}

impl Hash for Hole {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

trait TypeCheckContext {
    type Scope: LocalScope;

    fn scope(&self) -> &Self::Scope;
    fn scope_mut(&mut self) -> &mut Self::Scope;

    fn create_hole(&mut self) -> Hole;
    fn resolve_hole(&mut self, hole: &Hole) -> Expr<TypeCheckExprContext>;

    fn get_known_hole(&self, hole: &Hole) -> Option<&Expr<TypeCheckExprContext>>;
}

struct RecordingTypeCheckContext<'a, S> {
    scope: RecordingScope<'a, S>,
    known_holes: ReferencedHoles,
}

impl<'a, S: LocalScope> RecordingTypeCheckContext<'a, S> {
    fn into_captures(self) -> CapturedReferences {
        CapturedReferences {
            referenced_variables: self.scope.referenced_variables,
            referenced_holes: self.known_holes,
        }
    }
}

impl<'a, S: LocalScope> TypeCheckContext for RecordingTypeCheckContext<'a, S> {
    type Scope = RecordingScope<'a, S>;

    fn scope(&self) -> &Self::Scope {
        &self.scope
    }

    fn scope_mut(&mut self) -> &mut Self::Scope {
        &mut self.scope
    }

    fn create_hole(&mut self) -> Hole {
        todo!()
    }

    fn resolve_hole(&mut self, hole: &Hole) -> Expr<TypeCheckExprContext> {
        let _ = hole;
        todo!()
    }

    fn get_known_hole(&self, hole: &Hole) -> Option<&Expr<TypeCheckExprContext>> {
        let _ = hole;
        todo!()
    }
}

struct RecordingScope<'a, S> {
    scope: &'a mut S,

    referenced_variables: ReferencedVariables,
    declared_variables: HashSet<Variable>,
}

impl<'a, S: LocalScope> RecordingScope<'a, S> {
    fn new(scope: &'a mut S) -> Self {
        Self {
            scope,
            referenced_variables: HashSet::new(),
            declared_variables: HashSet::new(),
        }
    }
}

impl<'a, S: Scope> Scope for RecordingScope<'a, S> {
    fn lookup(&mut self, name: &IdentifierExpr) -> Lookup {
        let lookup = self.scope.lookup(name);

        if let Lookup::Variable(variable) = &lookup
            && self.declared_variables.contains(variable)
        {
            self.referenced_variables.insert(variable.clone());
        }

        lookup
    }

    fn lookup_assign(&mut self, name: &IdentifierExpr) -> Lookup {
        let lookup = self.scope.lookup_assign(name);

        if let Lookup::Variable(variable) = &lookup
            && self.declared_variables.contains(variable)
        {
            self.declared_variables.insert(variable.clone());
        }

        lookup
    }
}

impl<'a, S: LocalScope> LocalScope for RecordingScope<'a, S> {
    fn add_variable(&mut self, variable: Variable) {
        self.declared_variables.insert(variable);
    }

    fn has_variable(&self, variable: &Variable) -> bool {
        self.declared_variables.contains(variable) || self.scope.has_variable(variable)
    }
}

struct RecordingTypeChecker<C, EC> {
    context: C,
    expr_context: EC,
    known_variables: ReferencedVariables,
    known_holes: ReferencedHoles,
    check_cache: HashMap<*const WithLocation<ast::Expr>, Vec<RecordedTypeCheck>>,
    infer_cache: HashMap<*const WithLocation<ast::Expr>, Vec<RecordedTypeInfer>>,
}

type ReferencedVariables = HashSet<Variable>;
type ReferencedHoles = HashMap<Hole, Expr<TypeCheckExprContext>>;

struct CapturedReferences {
    referenced_variables: ReferencedVariables,
    referenced_holes: ReferencedHoles,
}

impl CapturedReferences {
    fn is_valid_in_context(&self, context: &impl TypeCheckContext) -> bool {
        self.referenced_variables
            .iter()
            .all(|v| context.scope().has_variable(v))
            && {
                self.referenced_holes.iter().all(|(hole, recorded_value)| {
                    context
                        .get_known_hole(hole)
                        .is_some_and(|current_value| unify(recorded_value, current_value))
                })
            }
    }
}

struct RecordedTypeCheck {
    expected_type: Expr<TypeCheckExprContext>,
    checked_expr: Expr<TypeCheckExprContext>,
    captures: CapturedReferences,
}

struct RecordedTypeInfer {
    inferred_type: Expr<TypeCheckExprContext>,
    checked_expr: Expr<TypeCheckExprContext>,
    captures: CapturedReferences,
}

struct TypeInferResult {
    inferred_type: Expr<TypeCheckExprContext>,
    checked_expr: Expr<TypeCheckExprContext>,
}

impl TypeInferResult {
    fn new(
        checked_expr: Expr<TypeCheckExprContext>,
        inferred_type: Expr<TypeCheckExprContext>,
    ) -> Self {
        Self {
            inferred_type,
            checked_expr,
        }
    }

    fn error() -> Self {
        Self::new(Expr::Error, Expr::Error)
    }
}

fn unify(_a: &Expr<TypeCheckExprContext>, _b: &Expr<TypeCheckExprContext>) -> bool {
    todo!()
}

impl<C: Context, EC> RecordingTypeChecker<C, EC> {
    fn new(context: C, expr_context: EC) -> Self {
        Self {
            context,
            expr_context,
            known_variables: HashSet::new(),
            known_holes: HashMap::new(),
            check_cache: HashMap::new(),
            infer_cache: HashMap::new(),
        }
    }

    fn check<S: LocalScope>(
        &mut self,
        tc_context: &mut impl TypeCheckContext<Scope = S>,
        expr: &WithLocation<ast::Expr>,
        expected_type: Expr<TypeCheckExprContext>,
    ) -> Expr<TypeCheckExprContext> {
        let key = std::ptr::from_ref(expr);
        let cached_expr = self.check_cache.get(&key).and_then(|records| {
            records
                .iter()
                .find(|recorded| {
                    unify(&recorded.expected_type, &expected_type)
                        && recorded.captures.is_valid_in_context(tc_context)
                })
                .map(|recorded| recorded.checked_expr.clone())
        });

        if let Some(checked_expr) = cached_expr {
            return checked_expr;
        }

        let rec_context = RecordingTypeCheckContext {
            scope: RecordingScope::new(tc_context.scope_mut()),
            known_holes: HashMap::new(),
        };

        let checked_expr = self.check_uncached(expr, &rec_context, &expected_type);
        let recorded = RecordedTypeCheck {
            expected_type,
            checked_expr: checked_expr.clone(),
            captures: rec_context.into_captures(),
        };

        self.check_cache.entry(key).or_default().push(recorded);

        checked_expr
    }

    fn infer<S: LocalScope>(
        &mut self,
        tc_context: &mut impl TypeCheckContext<Scope = S>,
        expr: &WithLocation<ast::Expr>,
    ) -> TypeInferResult {
        let key = std::ptr::from_ref(expr);
        let cached_result = self.infer_cache.get(&key).and_then(|records| {
            records
                .iter()
                .find(|recorded| recorded.captures.is_valid_in_context(tc_context))
                .map(|recorded| {
                    TypeInferResult::new(
                        recorded.checked_expr.clone(),
                        recorded.inferred_type.clone(),
                    )
                })
        });

        if let Some(result) = cached_result {
            return result;
        }

        let mut rec_context = RecordingTypeCheckContext {
            scope: RecordingScope::new(tc_context.scope_mut()),
            known_holes: HashMap::new(),
        };

        let result = self.infer_uncached(&mut rec_context, expr);
        let recorded = RecordedTypeInfer {
            inferred_type: result.inferred_type.clone(),
            checked_expr: result.checked_expr.clone(),
            captures: rec_context.into_captures(),
        };

        self.infer_cache.entry(key).or_default().push(recorded);

        result
    }

    fn check_uncached<S: LocalScope>(
        &mut self,
        _expr: &WithLocation<ast::Expr>,
        _tc_context: &impl TypeCheckContext<Scope = S>,
        _expected_type: &Expr<TypeCheckExprContext>,
    ) -> Expr<TypeCheckExprContext> {
        Expr::Error
    }

    fn infer_uncached<S: LocalScope>(
        &mut self,
        tc_context: &mut impl TypeCheckContext<Scope = S>,
        expr: &WithLocation<ast::Expr>,
    ) -> TypeInferResult {
        match &expr.value {
            ast::Expr::Error => TypeInferResult::error(),
            ast::Expr::As { value, value_type } => {
                let t = self.check(tc_context, value_type, Expr::AnyType);
                let e = self.check(tc_context, value, t.clone());

                TypeInferResult::new(e, t)
            }
            ast::Expr::BoolLiteral(value) => {
                TypeInferResult::new(Expr::BoolLiteral(*value), Expr::bool_type())
            }
            ast::Expr::Break { label: Some(_) } => todo!(),
            ast::Expr::Break { label: None } => TypeInferResult::new(
                Expr::Break { label: None },
                Expr::Hole(tc_context.create_hole()),
            ),
            ast::Expr::Builtin(value) => self.infer_builtin(tc_context, &expr.location, value, &[]),
            ast::Expr::FunctionResultValue => {
                todo!()
            }
            ast::Expr::IntLiteral(value) => {
                TypeInferResult::new(Expr::IntLiteral(value.clone()), Expr::int_type())
            }
            ast::Expr::Next { label: Some(_) } => todo!(),
            ast::Expr::Next { label: None } => TypeInferResult::new(
                Expr::Next { label: None },
                Expr::Hole(tc_context.create_hole()),
            ),
            ast::Expr::Redo { label: Some(_) } => todo!(),
            ast::Expr::Redo { label: None } => TypeInferResult::new(
                Expr::Redo { label: None },
                Expr::Hole(tc_context.create_hole()),
            ),
            ast::Expr::StringLiteral(value) => {
                let str_expr = if let [part] = &value.parts[..]
                    && let StringFragment::Text(text) = part
                {
                    Expr::StringLiteral(Box::from(text.as_str()))
                } else {
                    let parts = value
                        .parts
                        .iter()
                        .map(|part| match part {
                            StringFragment::Text(text) => {
                                Expr::StringLiteral(Box::from(text.as_str()))
                            }
                            StringFragment::Interpolate { value: e } => {
                                self.check(tc_context, e, Expr::string_type())
                            }
                        })
                        .collect::<Vec<_>>();
                    Expr::Builtin {
                        builtin: Builtin::StringConcat,
                        arguments: parts,
                    }
                };

                TypeInferResult::new(str_expr, Expr::string_type())
            }
            ast::Expr::Type => TypeInferResult::new(Expr::type_n(0), Expr::type_n(1)),
            ast::Expr::BigType(value) => {
                TypeInferResult::new(Expr::BigType(value.clone()), Expr::BigType(value + 1))
            }
            ast::Expr::Assert { .. } => todo!("infer assert expressions"),
            ast::Expr::BinaryOperation { .. } => todo!("infer binary operations"),
            ast::Expr::Block { .. } => todo!("infer blocks"),
            ast::Expr::Dot { .. } => todo!("infer member access"),
            ast::Expr::FunctionLiteral { .. } => todo!("infer function literals"),
            ast::Expr::FunctionCall { .. } => todo!("infer function calls"),
            ast::Expr::FunctionType { .. } => todo!("infer function types"),
            ast::Expr::IfElse { .. } => todo!("infer if/else expressions"),
            ast::Expr::Is { .. } => todo!("infer is expressions"),
            ast::Expr::Loop { .. } => todo!("infer loops"),
            ast::Expr::Match { .. } => todo!("infer match expressions"),
            ast::Expr::NewTraitObject { .. } => todo!("infer trait object construction"),
            ast::Expr::Paren(_) => todo!("infer parenthesized expressions"),
            ast::Expr::Raise { .. } => todo!("infer raise expressions"),
            ast::Expr::RecordLiteral { .. } => todo!("infer record literals"),
            ast::Expr::Summon { .. } => todo!("infer summon expressions"),
            ast::Expr::Tuple { .. } => todo!("infer tuple expressions"),
            ast::Expr::UnaryOperation { .. } => todo!("infer unary operations"),
            ast::Expr::While { .. } => todo!("infer while expressions"),
            ast::Expr::BoxedType { .. } => todo!("infer boxed types"),
            ast::Expr::Box { .. } => todo!("infer box expressions"),
            ast::Expr::Unbox { .. } => todo!("infer unbox expressions"),
            ast::Expr::Identifier(_) => todo!("infer identifiers"),
        }
    }

    fn infer_builtin<S: LocalScope>(
        &mut self,
        tc_context: &mut impl TypeCheckContext<Scope = S>,
        location: &Location,
        builtin_name: &str,
        args: &[WithLocation<ast::Expr>],
    ) -> TypeInferResult {
        let Ok(builtin) = builtin_name.parse::<Builtin>() else {
            self.report_invalid_builtin(location, builtin_name);
            return TypeInferResult::error();
        };

        match builtin {
            Builtin::IntType | Builtin::BoolType | Builtin::StringType | Builtin::NeverType => self
                .infer_fixed_builtin(tc_context, location, builtin, args, vec![], Expr::type_n(0)),

            Builtin::ArrayType => self.infer_parameterized_builtin(
                tc_context,
                location,
                builtin,
                args,
                |_| [],
                |_, element_type_type| element_type_type,
            ),

            Builtin::IntNegate | Builtin::IntBitNot => self.infer_fixed_builtin(
                tc_context,
                location,
                builtin,
                args,
                vec![Expr::int_type()],
                Expr::int_type(),
            ),

            Builtin::IntAdd
            | Builtin::IntSub
            | Builtin::IntMul
            | Builtin::IntBitAnd
            | Builtin::IntBitOr
            | Builtin::IntBitXor
            | Builtin::IntBitShiftLeft
            | Builtin::IntBitShiftRight => self.infer_fixed_builtin(
                tc_context,
                location,
                builtin,
                args,
                vec![Expr::int_type(), Expr::int_type()],
                Expr::int_type(),
            ),

            Builtin::IntEq
            | Builtin::IntNe
            | Builtin::IntLt
            | Builtin::IntLe
            | Builtin::IntGt
            | Builtin::IntGe => self.infer_fixed_builtin(
                tc_context,
                location,
                builtin,
                args,
                vec![Expr::int_type(), Expr::int_type()],
                Expr::bool_type(),
            ),

            Builtin::StringConcat => self.infer_fixed_builtin(
                tc_context,
                location,
                builtin,
                args,
                vec![Expr::string_type(), Expr::string_type()],
                Expr::string_type(),
            ),

            Builtin::StringEq | Builtin::StringNe => self.infer_fixed_builtin(
                tc_context,
                location,
                builtin,
                args,
                vec![Expr::string_type(), Expr::string_type()],
                Expr::bool_type(),
            ),

            Builtin::BoolNot => self.infer_fixed_builtin(
                tc_context,
                location,
                builtin,
                args,
                vec![Expr::bool_type()],
                Expr::bool_type(),
            ),

            Builtin::BoolEq | Builtin::BoolNe => self.infer_fixed_builtin(
                tc_context,
                location,
                builtin,
                args,
                vec![Expr::bool_type(), Expr::bool_type()],
                Expr::bool_type(),
            ),

            Builtin::ArrayCreateUnsafeUninitialized => self.infer_parameterized_builtin(
                tc_context,
                location,
                builtin,
                args,
                |_| [Expr::int_type()],
                |element_type, _| Expr::array_type(element_type.clone()),
            ),

            Builtin::ArrayLength => self.infer_parameterized_builtin(
                tc_context,
                location,
                builtin,
                args,
                |element_type| [Expr::array_type(element_type.clone())],
                |_, _| Expr::int_type(),
            ),

            Builtin::ArrayGet => self.infer_parameterized_builtin(
                tc_context,
                location,
                builtin,
                args,
                |element_type| [Expr::array_type(element_type.clone()), Expr::int_type()],
                |element_type, _| element_type.clone(),
            ),

            Builtin::ArraySet => self.infer_parameterized_builtin(
                tc_context,
                location,
                builtin,
                args,
                |element_type| {
                    [
                        Expr::array_type(element_type.clone()),
                        Expr::int_type(),
                        element_type.clone(),
                    ]
                },
                |_, _| Expr::unit_type(),
            ),

            Builtin::ConjunctionType | Builtin::DisjunctionType | Builtin::EqualToType => {
                self.report_invalid_builtin(
                    location,
                    format!("{} (not implemented)", builtin.as_str()),
                );
                TypeInferResult::error()
            }
        }
    }

    fn infer_fixed_builtin<S: LocalScope>(
        &mut self,
        tc_context: &mut impl TypeCheckContext<Scope = S>,
        location: &Location,
        builtin: Builtin,
        args: &[WithLocation<ast::Expr>],
        expected_arg_types: Vec<Expr<TypeCheckExprContext>>,
        inferred_type: Expr<TypeCheckExprContext>,
    ) -> TypeInferResult {
        if args.len() != expected_arg_types.len() {
            self.report_builtin_arity_error(
                location,
                builtin,
                expected_arg_types.len(),
                args.len(),
            );
            return TypeInferResult::error();
        }

        let checked_args = args
            .iter()
            .zip(expected_arg_types)
            .map(|(arg, expected_type)| self.check(tc_context, arg, expected_type))
            .collect();

        TypeInferResult::new(
            Expr::Builtin {
                builtin,
                arguments: checked_args,
            },
            inferred_type,
        )
    }

    fn infer_parameterized_builtin<S: LocalScope, const REST_ARG_COUNT: usize>(
        &mut self,
        tc_context: &mut impl TypeCheckContext<Scope = S>,
        location: &Location,
        builtin: Builtin,
        args: &[WithLocation<ast::Expr>],
        create_rest_arg_types: impl FnOnce(
            &Expr<TypeCheckExprContext>,
        ) -> [Expr<TypeCheckExprContext>; REST_ARG_COUNT],
        create_result_type: impl FnOnce(
            &Expr<TypeCheckExprContext>,
            Expr<TypeCheckExprContext>,
        ) -> Expr<TypeCheckExprContext>,
    ) -> TypeInferResult {
        let expected_arg_count = REST_ARG_COUNT + 1;
        let Some((element_type_arg, rest_args)) = args.split_first() else {
            self.report_builtin_arity_error(location, builtin, expected_arg_count, 0);
            return TypeInferResult::error();
        };

        let TypeInferResult {
            checked_expr: element_type,
            inferred_type: element_type_type,
        } = self.infer_uncached(tc_context, element_type_arg);

        let expected_rest_arg_types = create_rest_arg_types(&element_type);
        if rest_args.len() != expected_rest_arg_types.len() {
            self.report_builtin_arity_error(location, builtin, expected_arg_count, args.len());
            return TypeInferResult::error();
        }

        let mut checked_args = Vec::with_capacity(args.len());
        checked_args.push(element_type.clone());
        checked_args.extend(
            rest_args
                .iter()
                .zip(expected_rest_arg_types)
                .map(|(arg, expected_type)| self.check(tc_context, arg, expected_type)),
        );

        TypeInferResult::new(
            Expr::Builtin {
                builtin,
                arguments: checked_args,
            },
            create_result_type(&element_type, element_type_type),
        )
    }

    fn report_invalid_builtin(&self, location: &Location, name: impl AsRef<str>) {
        self.context
            .reporter()
            .report_error(CompileError::invalid_builtin(Some(location.clone()), name));
    }

    fn report_builtin_arity_error(
        &self,
        location: &Location,
        builtin: Builtin,
        expected: usize,
        actual: usize,
    ) {
        let argument_word = if expected == 1 {
            "argument"
        } else {
            "arguments"
        };
        self.report_invalid_builtin(
            location,
            format!(
                "{} (expected {expected} {argument_word}, got {actual})",
                builtin.as_str()
            ),
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use argon_util::ErrorCode;
    use num_bigint::BigInt;
    use parse18_runtime::FilePosition;
    use std::cell::RefCell;
    use std::path::PathBuf;
    use std::rc::Rc;

    #[derive(Clone, Default)]
    struct TestReporter {
        errors: Rc<RefCell<Vec<CompileError>>>,
    }

    impl ErrorReporter<CompileError> for TestReporter {
        fn report_error(&self, error: CompileError) {
            self.errors.borrow_mut().push(error);
        }
    }

    impl ErrorReporter<std::io::Error> for TestReporter {
        fn report_error(&self, _error: std::io::Error) {}
    }

    struct TestContext {
        reporter: TestReporter,
    }

    impl Context for TestContext {
        type Reporter = TestReporter;

        fn reporter(&self) -> &Self::Reporter {
            &self.reporter
        }
    }

    struct TestScope;

    impl Scope for TestScope {
        fn lookup(&mut self, _name: &IdentifierExpr) -> Lookup {
            panic!("lookup is not needed for builtin tests")
        }

        fn lookup_assign(&mut self, _name: &IdentifierExpr) -> Lookup {
            panic!("lookup_assign is not needed for builtin tests")
        }
    }

    impl LocalScope for TestScope {
        fn add_variable(&mut self, _variable: Variable) {}

        fn has_variable(&self, _variable: &Variable) -> bool {
            false
        }
    }

    struct TestTypeCheckContext {
        scope: TestScope,
    }

    impl TypeCheckContext for TestTypeCheckContext {
        type Scope = TestScope;

        fn scope(&self) -> &Self::Scope {
            &self.scope
        }

        fn scope_mut(&mut self) -> &mut Self::Scope {
            &mut self.scope
        }

        fn create_hole(&mut self) -> Hole {
            panic!("holes are not needed for builtin tests")
        }

        fn resolve_hole(&mut self, _hole: &Hole) -> Expr<TypeCheckExprContext> {
            panic!("holes are not needed for builtin tests")
        }

        fn get_known_hole(&self, _hole: &Hole) -> Option<&Expr<TypeCheckExprContext>> {
            None
        }
    }

    fn test_location() -> Location {
        Location {
            file: PathBuf::from("test.argon"),
            start: FilePosition { line: 1, column: 1 },
            end: FilePosition {
                line: 1,
                column: 16,
            },
        }
    }

    fn test_checker() -> (
        RecordingTypeChecker<TestContext, ()>,
        TestTypeCheckContext,
        TestReporter,
    ) {
        let reporter = TestReporter::default();
        let context = TestContext {
            reporter: reporter.clone(),
        };
        (
            RecordingTypeChecker::new(context, ()),
            TestTypeCheckContext { scope: TestScope },
            reporter,
        )
    }

    #[test]
    fn invalid_builtin_name_is_reported() {
        let (mut checker, mut tc_context, reporter) = test_checker();

        let result = checker.infer_builtin(&mut tc_context, &test_location(), "missing", &[]);

        assert!(matches!(result.checked_expr, Expr::Error));
        assert!(matches!(result.inferred_type, Expr::Error));

        let errors = reporter.errors.borrow();
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].code, ErrorCode::InvalidBuiltin);
        assert!(errors[0].message.contains("missing"));
        assert_eq!(errors[0].location, Some(test_location()));
    }

    #[test]
    fn invalid_builtin_arity_is_reported() {
        let (mut checker, mut tc_context, reporter) = test_checker();

        let result = checker.infer_builtin(&mut tc_context, &test_location(), "int_add", &[]);

        assert!(matches!(result.checked_expr, Expr::Error));
        assert!(matches!(result.inferred_type, Expr::Error));

        let errors = reporter.errors.borrow();
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].code, ErrorCode::InvalidBuiltin);
        assert!(errors[0].message.contains("int_add"));
        assert!(errors[0].message.contains("expected 2 arguments, got 0"));
        assert_eq!(errors[0].location, Some(test_location()));
    }

    #[test]
    fn builtin_type_infers_type_zero_without_errors() {
        let (mut checker, mut tc_context, reporter) = test_checker();

        let result = checker.infer_builtin(&mut tc_context, &test_location(), "int_type", &[]);

        match result.checked_expr {
            Expr::Builtin { builtin, arguments } => {
                assert_eq!(builtin, Builtin::IntType);
                assert!(arguments.is_empty());
            }
            other => panic!("expected builtin expression, got {other:?}"),
        }

        match result.inferred_type {
            Expr::Type(t) => match *t {
                Expr::IntLiteral(value) => assert_eq!(value, BigInt::ZERO),
                other => panic!("expected type level zero, got {other:?}"),
            },
            other => panic!("expected type expression, got {other:?}"),
        }

        assert!(reporter.errors.borrow().is_empty());
    }
}
