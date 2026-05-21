use argon_compiler::scope::{LocalScope, Lookup, OverloadLookup, Overloadable, Scope};
use argon_compiler::{Context, DefaultExprContext};
use argon_expr::{Builtin, Expr, ExprContext, Variable};
use argon_parser::ast;
use argon_parser::ast::{FunctionParameterListType, IdentifierExpr, StringFragment};
use argon_util::{CompileError, ErrorReporter, UniqueIdentifier};
use parse18_runtime::{Location, WithLocation};
use std::collections::{HashMap, HashSet, VecDeque};
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
    fn scope(&self) -> &dyn LocalScope<ExprContext = TypeCheckExprContext>;
    fn scope_mut(&mut self) -> &mut dyn LocalScope<ExprContext = TypeCheckExprContext>;

    fn create_hole(&mut self) -> Hole;
    fn resolve_hole(&mut self, hole: &Hole) -> Expr<TypeCheckExprContext>;

    fn get_known_hole(&self, hole: &Hole) -> Option<&Expr<TypeCheckExprContext>>;
}

struct RecordingTypeCheckContext<'a> {
    scope: RecordingScope<'a>,
    known_holes: ReferencedHoles,
}

impl<'a> RecordingTypeCheckContext<'a> {
    fn into_captures(self) -> CapturedReferences {
        CapturedReferences {
            referenced_variables: self.scope.referenced_variables,
            referenced_holes: self.known_holes,
        }
    }
}

impl<'a> TypeCheckContext for RecordingTypeCheckContext<'a> {
    fn scope(&self) -> &dyn LocalScope<ExprContext = TypeCheckExprContext> {
        &self.scope
    }

    fn scope_mut(&mut self) -> &mut dyn LocalScope<ExprContext = TypeCheckExprContext> {
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

struct RecordingScope<'a> {
    scope: &'a mut dyn LocalScope<ExprContext = TypeCheckExprContext>,

    referenced_variables: ReferencedVariables,
    declared_variables: HashSet<Variable<TypeCheckExprContext>>,
}

impl<'a> RecordingScope<'a> {
    fn new(scope: &'a mut dyn LocalScope<ExprContext = TypeCheckExprContext>) -> Self {
        Self {
            scope,
            referenced_variables: HashSet::new(),
            declared_variables: HashSet::new(),
        }
    }
}

impl<'a> Scope for RecordingScope<'a> {
    type ExprContext = TypeCheckExprContext;

    fn lookup(&mut self, name: &IdentifierExpr) -> Lookup<TypeCheckExprContext> {
        let lookup = self.scope.lookup(name);

        if let Lookup::Variable(variable) = &lookup
            && self.declared_variables.contains(variable)
        {
            self.referenced_variables.insert(variable.clone());
        }

        lookup
    }

    fn lookup_assign(&mut self, name: &IdentifierExpr) -> Lookup<TypeCheckExprContext> {
        let lookup = self.scope.lookup_assign(name);

        if let Lookup::Variable(variable) = &lookup
            && self.declared_variables.contains(variable)
        {
            self.declared_variables.insert(variable.clone());
        }

        lookup
    }
}

impl<'a> LocalScope for RecordingScope<'a> {
    fn add_variable(&mut self, variable: Variable<TypeCheckExprContext>) {
        self.declared_variables.insert(variable);
    }

    fn has_variable(&self, variable: &Variable<TypeCheckExprContext>) -> bool {
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

type ReferencedVariables = HashSet<Variable<TypeCheckExprContext>>;
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

    fn check(
        &mut self,
        tc_context: &mut impl TypeCheckContext,
        expr: &WithLocation<ast::Expr>,
        expected_type: &Expr<TypeCheckExprContext>,
    ) -> Expr<TypeCheckExprContext> {
        let key = std::ptr::from_ref(expr);
        if !skip_cache(expr) {
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
        }

        let mut rec_context = RecordingTypeCheckContext {
            scope: RecordingScope::new(tc_context.scope_mut()),
            known_holes: HashMap::new(),
        };

        let checked_expr = self.check_uncached(expr, &mut rec_context, expected_type);
        let recorded = RecordedTypeCheck {
            expected_type: expected_type.clone(),
            checked_expr: checked_expr.clone(),
            captures: rec_context.into_captures(),
        };

        self.check_cache.entry(key).or_default().push(recorded);

        checked_expr
    }

    fn infer(
        &mut self,
        tc_context: &mut impl TypeCheckContext,
        expr: &WithLocation<ast::Expr>,
    ) -> TypeInferResult {
        let key = std::ptr::from_ref(expr);
        if !skip_cache(expr) {
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

    fn check_uncached(
        &mut self,
        expr: &WithLocation<ast::Expr>,
        tc_context: &mut impl TypeCheckContext,
        expected_type: &Expr<TypeCheckExprContext>,
    ) -> Expr<TypeCheckExprContext> {
        match &expr.value {
            ast::Expr::Error => Expr::Error,

            // Inferred expressions.
            ast::Expr::As { .. }
            | ast::Expr::Assert { .. }
            | ast::Expr::BoolLiteral(_)
            | ast::Expr::IntLiteral(_)
            | ast::Expr::Is { .. }
            | ast::Expr::StringLiteral(_) => {
                let infer = self.infer(tc_context, expr);
                self.check_inferred_type(&expr.location, infer, expected_type)
            }

            // Branch expressions.
            ast::Expr::Break { .. } => todo!(),
            ast::Expr::Raise { .. } => todo!(),
            ast::Expr::Next { .. } => todo!(),
            ast::Expr::Redo { .. } => todo!(),

            ast::Expr::Paren(_) => self.check(tc_context, expr, expected_type),

            ast::Expr::BinaryOperation { .. } => todo!(),
            ast::Expr::Block { .. } => todo!(),
            ast::Expr::Builtin(_) => todo!(),
            ast::Expr::Dot { .. } => todo!(),
            ast::Expr::FunctionLiteral { .. } => todo!(),
            ast::Expr::FunctionCall { .. } => todo!(),
            ast::Expr::FunctionResultValue => todo!(),
            ast::Expr::FunctionType { .. } => todo!(),
            ast::Expr::IfElse { .. } => todo!(),
            ast::Expr::Loop { .. } => todo!(),
            ast::Expr::Match { .. } => todo!(),
            ast::Expr::NewTraitObject { .. } => todo!(),
            ast::Expr::RecordLiteral { .. } => todo!(),
            ast::Expr::Summon { .. } => todo!(),
            ast::Expr::Tuple { .. } => todo!(),
            ast::Expr::Type => todo!(),
            ast::Expr::BigType(_) => todo!(),
            ast::Expr::UnaryOperation { .. } => todo!(),
            ast::Expr::While { .. } => todo!(),
            ast::Expr::BoxedType { .. } => todo!(),
            ast::Expr::Box { .. } => todo!(),
            ast::Expr::Unbox { .. } => todo!(),
            ast::Expr::Identifier(_) => todo!(),
        }
    }

    fn infer_uncached(
        &mut self,
        tc_context: &mut impl TypeCheckContext,
        expr: &WithLocation<ast::Expr>,
    ) -> TypeInferResult {
        match &expr.value {
            ast::Expr::Error => TypeInferResult::error(),

            ast::Expr::As { value, value_type } => {
                let t = self.check(tc_context, value_type, &Expr::AnyType);
                let e = self.check(tc_context, value, &t);

                TypeInferResult::new(e, t)
            }

            ast::Expr::BoolLiteral(value) => {
                TypeInferResult::new(Expr::BoolLiteral(*value), Expr::bool_type())
            }
            ast::Expr::IntLiteral(value) => {
                TypeInferResult::new(Expr::IntLiteral(value.clone()), Expr::int_type())
            }

            ast::Expr::Break { label: Some(_) } => todo!(),
            ast::Expr::Break { label: None } => TypeInferResult::new(
                Expr::Break { label: None },
                Expr::Hole(tc_context.create_hole()),
            ),
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

            ast::Expr::Builtin(_) | ast::Expr::FunctionCall { .. } | ast::Expr::Identifier(_) => {
                let call = self.process_call(tc_context, expr);
                self.infer_call(tc_context, call)
            }

            ast::Expr::BinaryOperation { a, op, b } => {
                let call = self.process_binary_operator(tc_context, &expr.location, a, op, b);
                self.infer_call(tc_context, call)
            }

            ast::Expr::UnaryOperation { op, a } => {
                let call = self.process_unary_operator(tc_context, &expr.location, op, a);
                self.infer_call(tc_context, call)
            }

            ast::Expr::FunctionResultValue => {
                todo!()
            }

            ast::Expr::Paren(inner) => self.infer(tc_context, inner),

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
                                self.check(tc_context, e, &Expr::string_type())
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
            ast::Expr::Block { .. } => todo!("infer blocks"),
            ast::Expr::Dot { .. } => todo!("infer member access"),
            ast::Expr::FunctionLiteral { .. } => todo!("infer function literals"),
            ast::Expr::FunctionType { .. } => todo!("infer function types"),
            ast::Expr::IfElse { .. } => todo!("infer if/else expressions"),
            ast::Expr::Is { .. } => todo!("infer is expressions"),
            ast::Expr::Loop { .. } => todo!("infer loops"),
            ast::Expr::Match { .. } => todo!("infer match expressions"),
            ast::Expr::NewTraitObject { .. } => todo!("infer trait object construction"),
            ast::Expr::Raise { .. } => todo!("infer raise expressions"),
            ast::Expr::RecordLiteral { .. } => todo!("infer record literals"),
            ast::Expr::Summon { .. } => todo!("infer summon expressions"),
            ast::Expr::Tuple { .. } => todo!("infer tuple expressions"),
            ast::Expr::While { .. } => todo!("infer while expressions"),
            ast::Expr::BoxedType { .. } => todo!("infer boxed types"),
            ast::Expr::Box { .. } => todo!("infer box expressions"),
            ast::Expr::Unbox { .. } => todo!("infer unbox expressions"),
        }
    }

    fn check_call(
        &mut self,
        tc_context: &mut impl TypeCheckContext,
        call: CallInfo<'_>,
        expected_type: &Expr<TypeCheckExprContext>,
    ) -> Expr<TypeCheckExprContext> {
        match call.callee {
            CalleeInfo::Error => Expr::Error,
            CalleeInfo::Builtin(builtin) => {
                let infer = self.infer_builtin(tc_context, call.location, builtin, call.arguments);
                self.check_inferred_type(call.location, infer, expected_type)
            }
            CalleeInfo::Variable(v) => {
                let infer = self.infer_variable(tc_context, v, call.arguments);
                self.check_inferred_type(call.location, infer, expected_type)
            }
            CalleeInfo::Overloadable(_) => todo!(),
            CalleeInfo::Expr(_) => todo!(),
        }
    }

    fn infer_call(
        &mut self,
        tc_context: &mut impl TypeCheckContext,
        call: CallInfo<'_>,
    ) -> TypeInferResult {
        match call.callee {
            CalleeInfo::Error => TypeInferResult::error(),
            CalleeInfo::Builtin(builtin) => {
                self.infer_builtin(tc_context, call.location, builtin, call.arguments)
            }
            CalleeInfo::Variable(v) => self.infer_variable(tc_context, v, call.arguments),
            CalleeInfo::Overloadable(_) => todo!(),
            CalleeInfo::Expr(_) => todo!(),
        }
    }

    fn infer_builtin(
        &mut self,
        tc_context: &mut impl TypeCheckContext,
        location: &Location,
        builtin: Builtin,
        args: VecDeque<ArgumentInfo<'_>>,
    ) -> TypeInferResult {
        match builtin {
            Builtin::IntType | Builtin::BoolType | Builtin::StringType | Builtin::NeverType => {
                self.infer_fixed_builtin(tc_context, location, builtin, args, [], Expr::type_n(0))
            }

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
                [Expr::int_type()],
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
                [Expr::int_type(), Expr::int_type()],
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
                [Expr::int_type(), Expr::int_type()],
                Expr::bool_type(),
            ),

            Builtin::StringConcat => {
                let checked_args = args
                    .iter()
                    .map(|arg| self.check(tc_context, arg.arg, &Expr::string_type()))
                    .collect();

                TypeInferResult::new(
                    Expr::Builtin {
                        builtin,
                        arguments: checked_args,
                    },
                    Expr::string_type(),
                )
            }

            Builtin::StringEq | Builtin::StringNe => self.infer_fixed_builtin(
                tc_context,
                location,
                builtin,
                args,
                [Expr::string_type(), Expr::string_type()],
                Expr::bool_type(),
            ),

            Builtin::BoolNot => self.infer_fixed_builtin(
                tc_context,
                location,
                builtin,
                args,
                [Expr::bool_type()],
                Expr::bool_type(),
            ),

            Builtin::BoolEq | Builtin::BoolNe => self.infer_fixed_builtin(
                tc_context,
                location,
                builtin,
                args,
                [Expr::bool_type(), Expr::bool_type()],
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

    fn infer_fixed_builtin<const ARG_COUNT: usize>(
        &mut self,
        tc_context: &mut impl TypeCheckContext,
        location: &Location,
        builtin: Builtin,
        args: VecDeque<ArgumentInfo<'_>>,
        expected_arg_types: [Expr<TypeCheckExprContext>; ARG_COUNT],
        inferred_type: Expr<TypeCheckExprContext>,
    ) -> TypeInferResult {
        if args.len() != ARG_COUNT {
            self.report_builtin_arity_error(location, builtin, ARG_COUNT, args.len());
            return TypeInferResult::error();
        }

        let checked_args = args
            .iter()
            .zip(expected_arg_types)
            .map(|(arg, expected_type)| self.check(tc_context, arg.arg, &expected_type))
            .collect();

        TypeInferResult::new(
            Expr::Builtin {
                builtin,
                arguments: checked_args,
            },
            inferred_type,
        )
    }

    fn infer_parameterized_builtin<const REST_ARG_COUNT: usize>(
        &mut self,
        tc_context: &mut impl TypeCheckContext,
        location: &Location,
        builtin: Builtin,
        mut args: VecDeque<ArgumentInfo<'_>>,
        create_rest_arg_types: impl FnOnce(
            &Expr<TypeCheckExprContext>,
        ) -> [Expr<TypeCheckExprContext>; REST_ARG_COUNT],
        create_result_type: impl FnOnce(
            &Expr<TypeCheckExprContext>,
            Expr<TypeCheckExprContext>,
        ) -> Expr<TypeCheckExprContext>,
    ) -> TypeInferResult {
        let expected_arg_count = REST_ARG_COUNT + 1;
        let total_arg_count = args.len();

        let Some(element_type_arg) = args.pop_front() else {
            self.report_builtin_arity_error(location, builtin, expected_arg_count, 0);
            return TypeInferResult::error();
        };

        let TypeInferResult {
            checked_expr: element_type,
            inferred_type: element_type_type,
        } = self.infer_uncached(tc_context, element_type_arg.arg);

        let expected_rest_arg_types = create_rest_arg_types(&element_type);
        if args.len() != REST_ARG_COUNT {
            self.report_builtin_arity_error(location, builtin, expected_arg_count, total_arg_count);
            return TypeInferResult::error();
        }

        let mut checked_args = Vec::with_capacity(total_arg_count);
        checked_args.push(element_type.clone());
        checked_args.extend(
            args.iter()
                .zip(expected_rest_arg_types)
                .map(|(arg, expected_type)| self.check(tc_context, arg.arg, &expected_type)),
        );

        TypeInferResult::new(
            Expr::Builtin {
                builtin,
                arguments: checked_args,
            },
            create_result_type(&element_type, element_type_type),
        )
    }

    fn infer_variable(
        &mut self,
        tc_context: &mut impl TypeCheckContext,
        v: Variable<TypeCheckExprContext>,
        args: VecDeque<ArgumentInfo<'_>>,
    ) -> TypeInferResult {
        let t = v.var_type();
        let expr = Expr::Variable(v);
        self.infer_function_object_call(tc_context, expr, t, args)
    }

    fn infer_function_object_call(
        &mut self,
        tc_context: &mut impl TypeCheckContext,
        mut expr: Expr<TypeCheckExprContext>,
        mut t: Expr<TypeCheckExprContext>,
        args: VecDeque<ArgumentInfo<'_>>,
    ) -> TypeInferResult {
        for arg in args {
            if let Expr::FunctionType { a, r } = t {
                let arg = self.check(tc_context, arg.arg, &a);
                expr = Expr::FunctionObjectCall {
                    function: Box::new(expr),
                    argument: Box::new(arg),
                };
                t = *r;
            } else {
                self.context
                    .reporter()
                    .report_error(CompileError::function_type_required(
                        arg.call_location.clone(),
                        format!("{:?}", t),
                    ));

                return TypeInferResult::error();
            }
        }

        TypeInferResult::new(expr, t)
    }

    fn process_call<'a>(
        &mut self,
        tc_context: &mut impl TypeCheckContext,
        mut func_expr: &'a WithLocation<ast::Expr>,
    ) -> CallInfo<'a> {
        let mut arguments = VecDeque::new();
        let call_location = &func_expr.location;

        loop {
            match &func_expr.value {
                ast::Expr::FunctionCall {
                    func,
                    arg,
                    list_type,
                } => {
                    arguments.push_front(ArgumentInfo {
                        call_location: &func_expr.location,
                        arg,
                        list_type: *list_type,
                    });
                    func_expr = func;
                }

                ast::Expr::Builtin(builtin_name) => {
                    let Ok(builtin) = builtin_name.parse::<Builtin>() else {
                        self.report_invalid_builtin(&func_expr.location, builtin_name);
                        return CallInfo {
                            location: call_location,
                            callee: CalleeInfo::Error,
                            arguments,
                        };
                    };

                    return CallInfo {
                        location: call_location,
                        callee: CalleeInfo::Builtin(builtin),
                        arguments,
                    };
                }

                ast::Expr::Identifier(identifier) => {
                    let callee = self.process_lookup(tc_context, &func_expr.location, identifier);
                    return CallInfo {
                        location: call_location,
                        callee,
                        arguments,
                    };
                }

                ast::Expr::Dot { .. } => todo!(),

                _ => {
                    return CallInfo {
                        location: call_location,
                        callee: CalleeInfo::Expr(func_expr),
                        arguments,
                    };
                }
            }
        }
    }

    fn process_binary_operator<'a>(
        &mut self,
        tc_context: &mut impl TypeCheckContext,
        location: &'a Location,
        left: &'a WithLocation<ast::Expr>,
        op: &'a WithLocation<ast::BinaryOperator>,
        right: &'a WithLocation<ast::Expr>,
    ) -> CallInfo<'a> {
        CallInfo {
            location,
            callee: self.process_lookup(
                tc_context,
                &op.location,
                &IdentifierExpr::BinaryOp(op.value),
            ),
            arguments: VecDeque::from([
                ArgumentInfo {
                    call_location: location,
                    arg: left,
                    list_type: FunctionParameterListType::NormalList,
                },
                ArgumentInfo {
                    call_location: location,
                    arg: right,
                    list_type: FunctionParameterListType::NormalList,
                },
            ]),
        }
    }

    fn process_unary_operator<'a>(
        &mut self,
        tc_context: &mut impl TypeCheckContext,
        location: &'a Location,
        op: &'a WithLocation<ast::UnaryOperator>,
        arg: &'a WithLocation<ast::Expr>,
    ) -> CallInfo<'a> {
        CallInfo {
            location,
            callee: self.process_lookup(
                tc_context,
                &op.location,
                &IdentifierExpr::UnaryOp(op.value),
            ),
            arguments: VecDeque::from([ArgumentInfo {
                call_location: location,
                arg,
                list_type: FunctionParameterListType::NormalList,
            }]),
        }
    }

    fn process_lookup<'a>(
        &mut self,
        tc_context: &mut impl TypeCheckContext,
        location: &Location,
        identifier: &IdentifierExpr,
    ) -> CalleeInfo<'a> {
        match tc_context.scope_mut().lookup(identifier) {
            Lookup::Empty => {
                self.context
                    .reporter()
                    .report_error(CompileError::unknown_identifier(
                        location.clone(),
                        identifier.to_string(),
                    ));

                CalleeInfo::Error
            }

            Lookup::Variable(v) => CalleeInfo::Variable(v),

            Lookup::Overloadable(overloadable) => CalleeInfo::Overloadable(overloadable),
        }
    }

    fn check_inferred_type(
        &mut self,
        location: &Location,
        infer: TypeInferResult,
        expected_type: &Expr<TypeCheckExprContext>,
    ) -> Expr<TypeCheckExprContext> {
        let TypeInferResult {
            checked_expr,
            inferred_type,
        } = infer;

        if !unify(expected_type, &inferred_type) {
            self.context
                .reporter()
                .report_error(CompileError::type_mismatch(
                    location.clone(),
                    format!("{:?}", expected_type),
                    format!("{:?}", inferred_type),
                ));

            return Expr::Error;
        }

        checked_expr
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

struct ArgumentInfo<'a> {
    call_location: &'a Location,
    arg: &'a WithLocation<ast::Expr>,
    list_type: FunctionParameterListType,
}

enum CalleeInfo<'a> {
    Error,
    Expr(&'a WithLocation<ast::Expr>),
    Builtin(Builtin),
    Variable(Variable<TypeCheckExprContext>),
    Overloadable(OverloadLookup),
}

struct CallInfo<'a> {
    location: &'a Location,
    callee: CalleeInfo<'a>,
    arguments: VecDeque<ArgumentInfo<'a>>,
}

fn skip_cache(expr: &WithLocation<ast::Expr>) -> bool {
    matches!(expr.value, ast::Expr::Error | ast::Expr::Paren(_))
}

#[cfg(test)]
mod tests {
    use super::*;
    use argon_compiler::test_utils::{TestContext, TestReporter};
    use argon_util::ErrorCode;
    use num_bigint::BigInt;
    use parse18_runtime::FilePosition;
    use std::path::PathBuf;

    struct TestScope;

    impl Scope for TestScope {
        type ExprContext = TypeCheckExprContext;

        fn lookup(&mut self, _name: &IdentifierExpr) -> Lookup<TypeCheckExprContext> {
            panic!("lookup is not needed for builtin tests")
        }

        fn lookup_assign(&mut self, _name: &IdentifierExpr) -> Lookup<TypeCheckExprContext> {
            panic!("lookup_assign is not needed for builtin tests")
        }
    }

    impl LocalScope for TestScope {
        fn add_variable(&mut self, _variable: Variable<TypeCheckExprContext>) {}

        fn has_variable(&self, _variable: &Variable<TypeCheckExprContext>) -> bool {
            false
        }
    }

    struct TestTypeCheckContext {
        scope: TestScope,
    }

    impl TypeCheckContext for TestTypeCheckContext {
        fn scope(&self) -> &dyn LocalScope<ExprContext = TypeCheckExprContext> {
            &self.scope
        }

        fn scope_mut(&mut self) -> &mut dyn LocalScope<ExprContext = TypeCheckExprContext> {
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

    fn builtin_expr(name: &str) -> WithLocation<ast::Expr> {
        WithLocation::new(ast::Expr::Builtin(name.to_owned()), test_location())
    }

    fn test_checker() -> (
        RecordingTypeChecker<TestContext, ()>,
        TestTypeCheckContext,
        TestReporter,
    ) {
        let reporter = TestReporter::default();
        let context = TestContext::new(reporter.clone());
        (
            RecordingTypeChecker::new(context, ()),
            TestTypeCheckContext { scope: TestScope },
            reporter,
        )
    }

    #[test]
    fn invalid_builtin_name_is_reported() {
        let (mut checker, mut tc_context, reporter) = test_checker();
        let expr = builtin_expr("missing");

        let result = checker.infer(&mut tc_context, &expr);

        assert!(matches!(result.checked_expr, Expr::Error));
        assert!(matches!(result.inferred_type, Expr::Error));

        let errors = reporter.errors();
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].code, ErrorCode::InvalidBuiltin);
        assert!(errors[0].message.contains("missing"));
        assert_eq!(errors[0].location, Some(test_location()));
    }

    #[test]
    fn invalid_builtin_arity_is_reported() {
        let (mut checker, mut tc_context, reporter) = test_checker();
        let expr = builtin_expr("int_add");

        let result = checker.infer(&mut tc_context, &expr);

        assert!(matches!(result.checked_expr, Expr::Error));
        assert!(matches!(result.inferred_type, Expr::Error));

        let errors = reporter.errors();
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].code, ErrorCode::InvalidBuiltin);
        assert!(errors[0].message.contains("int_add"));
        assert!(errors[0].message.contains("expected 2 arguments, got 0"));
        assert_eq!(errors[0].location, Some(test_location()));
    }

    #[test]
    fn builtin_type_infers_type_zero_without_errors() {
        let (mut checker, mut tc_context, reporter) = test_checker();
        let expr = builtin_expr("int_type");

        let result = checker.infer(&mut tc_context, &expr);

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

        assert!(reporter.errors().is_empty());
    }
}
