use argon_compiler::scope::{
    LocalScope, LocalVariableScope, Lookup, OverloadLookup, Overloadable, Scope, ShiftedScope,
};
use argon_compiler::{Context, DefaultExprContext};
use argon_expr::{Builtin, ErasureMode, Expr, ExprContext, ExprContextShifter, FreshVariableShifter, LocalVariable, Variable, VariableTupleElement};
use argon_parser::ast;
use argon_parser::ast::{FunctionParameterListType, Identifier, StringFragment};
use argon_util::{CompileError, ErrorReporter, UniqueIdentifier};
use nonempty_collections::NEVec;
use num_bigint::BigInt;
use parse18_runtime::{Location, WithLocation};
use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt::format;
use std::hash::{Hash, Hasher};
use std::sync::Arc;
use argon_compiler::scanner::PurityScanner;

pub struct TypeChecker<'a> {
    pub context: Context,
    pub scope: &'a mut dyn Scope<ExprContext = DefaultExprContext>,
}

impl<'a> TypeChecker<'a> {
    pub fn type_check_type_expr(
        &mut self,
        e: &WithLocation<ast::Expr>,
    ) -> Expr<DefaultExprContext> {
        let mut checker = RecordingTypeChecker::new(self.context.clone());
        let mut shifted_scope = ShiftedScope::new(self.scope, DefaultToTypeCheckExprContextShifter);
        let mut local_scope = LocalVariableScope::new(&mut shifted_scope);
        let recording_scope = RecordingScope::new(&mut local_scope);
        let mut tc_context = RecordingTypeCheckContext::new(recording_scope);

        let expr = checker.check(&mut tc_context, e, &Expr::AnyType);

        TypeCheckToDefaultExprContextShifter.shift(expr)
    }

    pub fn type_check_expr(
        &mut self,
        e: &WithLocation<ast::Expr>,
        expected_type: &Expr<DefaultExprContext>,
    ) -> Expr<DefaultExprContext> {
        let mut checker = RecordingTypeChecker::new(self.context.clone());
        let mut shifted_scope = ShiftedScope::new(self.scope, DefaultToTypeCheckExprContextShifter);
        let mut local_scope = LocalVariableScope::new(&mut shifted_scope);
        let recording_scope = RecordingScope::new(&mut local_scope);
        let mut tc_context = RecordingTypeCheckContext::new(recording_scope);

        let expected_type = DefaultToTypeCheckExprContextShifter.shift(expected_type.clone());
        let expr = checker.check(&mut tc_context, e, &expected_type);

        TypeCheckToDefaultExprContextShifter.shift(expr)
    }
}

#[derive(Debug, Clone)]
pub struct TypeCheckExprContext {}

impl ExprContext for TypeCheckExprContext {
    type Hole = Hole;
    type Function = <DefaultExprContext as ExprContext>::Function;
    type Method = <DefaultExprContext as ExprContext>::Method;
    type Record = <DefaultExprContext as ExprContext>::Record;
    type Enum = <DefaultExprContext as ExprContext>::Enum;
    type EnumVariant = <DefaultExprContext as ExprContext>::EnumVariant;
    type Trait = <DefaultExprContext as ExprContext>::Trait;
    type Instance = <DefaultExprContext as ExprContext>::Instance;
}

pub struct TypeCheckToDefaultExprContextShifter;

impl ExprContextShifter for TypeCheckToDefaultExprContextShifter {
    type EC1 = TypeCheckExprContext;
    type EC2 = DefaultExprContext;

    fn shift_hole(&mut self, hole: Hole) -> Expr<DefaultExprContext> {
        TypeCheckToDefaultExprContextShifter.shift(*hole.1)
    }
}

pub struct DefaultToTypeCheckExprContextShifter;

impl ExprContextShifter for DefaultToTypeCheckExprContextShifter {
    type EC1 = DefaultExprContext;
    type EC2 = TypeCheckExprContext;

    fn shift_hole(&mut self, hole: <Self::EC1 as ExprContext>::Hole) -> Expr<Self::EC2> {
        match hole {}
    }
}

#[derive(Debug, Clone)]
pub struct Hole(UniqueIdentifier, Box<Expr<TypeCheckExprContext>>);

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
    pub fn new(scope: RecordingScope<'a>) -> Self {
        Self {
            scope,
            known_holes: ReferencedHoles::new(),
        }
    }

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

    fn lookup(&mut self, name: &Identifier) -> Lookup<TypeCheckExprContext> {
        let lookup = self.scope.lookup(name);

        if let Lookup::Variable(variable) = &lookup
            && self.declared_variables.contains(variable)
        {
            self.referenced_variables.insert(variable.clone());
        }

        lookup
    }

    fn lookup_assign(&mut self, name: &Identifier) -> Lookup<TypeCheckExprContext> {
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

struct RecordingTypeChecker<C> {
    context: C,
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

fn unify(a: &Expr<TypeCheckExprContext>, b: &Expr<TypeCheckExprContext>) -> bool {
    match (a, b) {
        (Expr::Error, _) | (_, Expr::Error) => true,
        (Expr::AnyType, _) | (_, Expr::AnyType) => true,
        (Expr::Hole(a), Expr::Hole(b)) => a == b,
        (Expr::BoolLiteral(a), Expr::BoolLiteral(b)) => a == b,
        (
            Expr::Builtin {
                builtin: a,
                arguments: a_args,
            },
            Expr::Builtin {
                builtin: b,
                arguments: b_args,
            },
        ) => a == b && unify_all(a_args, b_args),
        (Expr::EnumType(a, a_args), Expr::EnumType(b, b_args)) => {
            a == b && unify_all(a_args, b_args)
        }
        (
            Expr::FunctionType {
                a: a_arg,
                r: a_result,
            },
            Expr::FunctionType {
                a: b_arg,
                r: b_result,
            },
        ) => unify(a_arg, b_arg) && unify(a_result, b_result),
        (Expr::IntLiteral(a), Expr::IntLiteral(b)) => a == b,
        (Expr::RecordType(a, a_args), Expr::RecordType(b, b_args)) => {
            a == b && unify_all(a_args, b_args)
        }
        (Expr::StringLiteral(a), Expr::StringLiteral(b)) => a == b,
        (Expr::TraitType(a, a_args), Expr::TraitType(b, b_args)) => {
            a == b && unify_all(a_args, b_args)
        }
        (Expr::Tuple { items: a }, Expr::Tuple { items: b }) => unify_all(a, b),
        (Expr::Type(a), Expr::Type(b)) => unify(a, b),
        (Expr::BigType(a), Expr::BigType(b)) => a == b,
        (Expr::Variable(a), Expr::Variable(b)) => a == b,
        _ => false,
    }
}

fn unify_all(a: &[Expr<TypeCheckExprContext>], b: &[Expr<TypeCheckExprContext>]) -> bool {
    a.len() == b.len() && a.iter().zip(b).all(|(a, b)| unify(a, b))
}

impl RecordingTypeChecker<Context> {
    fn new(context: Context) -> Self {
        Self {
            context,
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

            // Call expressions.
            ast::Expr::Builtin(_)
            | ast::Expr::FunctionCall { .. }
            | ast::Expr::Identifier(_)
            | ast::Expr::Type => {
                let call = self.process_call(tc_context, expr);
                self.check_call(tc_context, call, expected_type)
            }

            ast::Expr::Paren(inner) => self.check(tc_context, inner, expected_type),

            ast::Expr::BinaryOperation { .. } => todo!(),
            ast::Expr::Block { body, finally_body } => {
                let checked_body = self.check_block(tc_context, body, expected_type);
                let checked_finally_body = finally_body.as_ref().map(|finally_body| {
                    Box::new(self.check_block(tc_context, finally_body, &Expr::unit_type()))
                });

                Expr::Ensures {
                    block_body: Box::new(checked_body),
                    ensures_body: checked_finally_body,
                }
            }
            ast::Expr::Dot { .. } => todo!(),
            ast::Expr::FunctionLiteral { .. } => todo!(),
            ast::Expr::FunctionResultValue => todo!(),
            ast::Expr::FunctionType { .. } => todo!(),
            ast::Expr::IfElse {
                condition,
                when_true,
                when_false,
            } => {
                let conv_condition = self.check(tc_context, condition, &Expr::bool_type());
                let conv_when_true = self.check_block(tc_context, when_true, expected_type);
                let conv_when_false = self.check_block(tc_context, when_false, expected_type);

                let (when_true_var, when_false_var) = self.create_if_cond_vars(&conv_condition);

                Expr::IfElse {
                    when_true_var,
                    when_false_var,
                    condition: Box::new(conv_condition),
                    when_true: Box::new(conv_when_true),
                    when_false: Box::new(conv_when_false),
                }
            }
            ast::Expr::Loop { .. } => todo!(),
            ast::Expr::Match { .. } => todo!(),
            ast::Expr::NewTraitObject { .. } => todo!(),
            ast::Expr::RecordLiteral { .. } => todo!(),
            ast::Expr::Summon { .. } => todo!(),
            ast::Expr::Tuple { items } => {
                let checked_items = match expected_type {
                    Expr::AnyType | Expr::Type(_) | Expr::BigType(_) => items
                        .iter()
                        .map(|item| self.check(tc_context, item, expected_type))
                        .collect::<Vec<_>>(),

                    Expr::Tuple { items: item_types } => {
                        if item_types.len() != items.len() {
                            self.context.reporter().report_error(
                                CompileError::tuple_size_mismatch(
                                    expr.location.clone(),
                                    item_types.len(),
                                    items.len(),
                                ),
                            );
                            return Expr::Error;
                        }

                        items
                            .iter()
                            .zip(item_types.iter())
                            .map(|(item, expected_item_type)| {
                                self.check(tc_context, item, expected_item_type)
                            })
                            .collect::<Vec<_>>()
                    }

                    _ => {
                        let infer = self.infer(tc_context, expr);
                        return self.check_inferred_type(&expr.location, infer, expected_type);
                    }
                };

                Expr::Tuple {
                    items: checked_items,
                }
            }
            ast::Expr::BigType(_) => todo!(),
            ast::Expr::UnaryOperation { .. } => todo!(),
            ast::Expr::While { .. } => todo!(),
            ast::Expr::BoxedType { .. } => todo!(),
            ast::Expr::Box { .. } => todo!(),
            ast::Expr::Unbox { .. } => todo!(),
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

            ast::Expr::Builtin(_)
            | ast::Expr::FunctionCall { .. }
            | ast::Expr::Identifier(_)
            | ast::Expr::Type => {
                let call = self.process_call(tc_context, expr);
                self.infer_call(tc_context, call)
            }

            ast::Expr::BinaryOperation { a, op, b } => {
                let op_id = match op.value {
                    ast::BinaryOperator::Assign => todo!(),
                    ast::BinaryOperator::LogicalOr => todo!(),
                    ast::BinaryOperator::LogicalAnd => todo!(),
                    ast::BinaryOperator::PropEqual => todo!(),
                    ast::BinaryOperator::PropDisjunction => todo!(),
                    ast::BinaryOperator::PropConjunction => todo!(),

                    ast::BinaryOperator::Plus => ast::BinaryOperatorIdentifier::Plus,
                    ast::BinaryOperator::Minus => ast::BinaryOperatorIdentifier::Minus,
                    ast::BinaryOperator::Mul => ast::BinaryOperatorIdentifier::Mul,
                    ast::BinaryOperator::Div => ast::BinaryOperatorIdentifier::Div,
                    ast::BinaryOperator::Equal => ast::BinaryOperatorIdentifier::Equal,
                    ast::BinaryOperator::NotEqual => ast::BinaryOperatorIdentifier::NotEqual,
                    ast::BinaryOperator::LessThan => ast::BinaryOperatorIdentifier::LessThan,
                    ast::BinaryOperator::LessThanEq => ast::BinaryOperatorIdentifier::LessThanEq,
                    ast::BinaryOperator::GreaterThan => ast::BinaryOperatorIdentifier::GreaterThan,
                    ast::BinaryOperator::GreaterThanEq => {
                        ast::BinaryOperatorIdentifier::GreaterThanEq
                    }
                    ast::BinaryOperator::BitOr => ast::BinaryOperatorIdentifier::BitOr,
                    ast::BinaryOperator::BitXOr => ast::BinaryOperatorIdentifier::BitXOr,
                    ast::BinaryOperator::BitAnd => ast::BinaryOperatorIdentifier::BitAnd,
                    ast::BinaryOperator::ShiftLeft => ast::BinaryOperatorIdentifier::ShiftLeft,
                    ast::BinaryOperator::ShiftRight => ast::BinaryOperatorIdentifier::ShiftRight,
                    ast::BinaryOperator::Concat => ast::BinaryOperatorIdentifier::Concat,
                };

                let call = self.process_binary_operator(
                    tc_context,
                    &expr.location,
                    a,
                    op_id,
                    &op.location,
                    b,
                );
                self.infer_call(tc_context, call)
            }

            ast::Expr::UnaryOperation { op, a } => {
                let op_id = match op.value {
                    ast::UnaryOperator::Plus => ast::UnaryOperatorIdentifier::Plus,
                    ast::UnaryOperator::Minus => ast::UnaryOperatorIdentifier::Minus,
                    ast::UnaryOperator::BitNot => ast::UnaryOperatorIdentifier::BitNot,
                    ast::UnaryOperator::LogicalNot => ast::UnaryOperatorIdentifier::LogicalNot,
                };

                let call =
                    self.process_unary_operator(tc_context, &expr.location, op_id, &op.location, a);
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
            ast::Expr::BigType(value) => {
                TypeInferResult::new(Expr::BigType(value.clone()), Expr::BigType(value + 1))
            }
            ast::Expr::Assert { .. } => todo!("infer assert expressions"),
            ast::Expr::Block { body, finally_body } => {
                let TypeInferResult {
                    checked_expr: checked_body,
                    inferred_type,
                } = self.infer_block(tc_context, body);
                let checked_finally_body = finally_body.as_ref().map(|finally_body| {
                    Box::new(self.check_block(tc_context, finally_body, &Expr::unit_type()))
                });

                TypeInferResult::new(
                    Expr::Ensures {
                        block_body: Box::new(checked_body),
                        ensures_body: checked_finally_body,
                    },
                    inferred_type,
                )
            }
            ast::Expr::Dot { .. } => todo!("infer member access"),
            ast::Expr::FunctionLiteral { .. } => todo!("infer function literals"),
            ast::Expr::FunctionType { .. } => todo!("infer function types"),
            ast::Expr::IfElse {
                condition,
                when_true,
                when_false,
            } => {
                let conv_condition = self.check(tc_context, condition, &Expr::bool_type());
                let TypeInferResult {
                    checked_expr: conv_when_true,
                    inferred_type,
                } = self.infer_block(tc_context, when_true);
                let conv_when_false = self.check_block(tc_context, when_false, &inferred_type);

                let (when_true_var, when_false_var) = self.create_if_cond_vars(&conv_condition);

                TypeInferResult::new(
                    Expr::IfElse {
                        when_true_var,
                        when_false_var,
                        condition: Box::new(conv_condition),
                        when_true: Box::new(conv_when_true),
                        when_false: Box::new(conv_when_false),
                    },
                    inferred_type,
                )
            }
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

    fn check_block(
        &mut self,
        tc_context: &mut impl TypeCheckContext,
        body: &WithLocation<Vec<WithLocation<ast::Stmt>>>,
        expected_type: &Expr<TypeCheckExprContext>,
    ) -> Expr<TypeCheckExprContext> {
        let Some((last_stmt, leading_stmts)) = body.value.split_last() else {
            return self.check_inferred_type(
                &body.location,
                TypeInferResult::new(Expr::unit_type(), Expr::unit_type()),
                expected_type,
            );
        };
        
        
        
        let checked_stmts = leading_stmts
            .iter()
            .map(|stmt| self.check_stmt(tc_context, stmt, &Expr::unit_type()))
            .collect::<Vec<_>>();
        
        let last_stmt = self.check_stmt(tc_context, last_stmt, expected_type);
        
        let checked_stmts = match NEVec::try_from_vec(checked_stmts) {
            Some(mut checked_stmts) => {
                checked_stmts.push(last_stmt);
                checked_stmts
            },
            None => NEVec::new(last_stmt),
        };
        
        Expr::Sequence(checked_stmts)
    }

    fn infer_block(
        &mut self,
        tc_context: &mut impl TypeCheckContext,
        body: &WithLocation<Vec<WithLocation<ast::Stmt>>>,
    ) -> TypeInferResult {
        let Some((last_stmt, leading_stmts)) = body.value.split_last() else {
            return TypeInferResult::new(Expr::unit_type(), Expr::unit_type());
        };

        let checked_stmts = leading_stmts
            .iter()
            .map(|stmt| self.check_stmt(tc_context, stmt, &Expr::unit_type()))
            .collect::<Vec<_>>();
        let TypeInferResult {
            checked_expr,
            inferred_type,
        } = self.infer_stmt(tc_context, last_stmt);
        
        let checked_stmts = match NEVec::try_from_vec(checked_stmts) {
            Some(mut checked_stmts) => {
                checked_stmts.push(checked_expr);
                checked_stmts
            },
            None => NEVec::new(checked_expr),
        };
        
        TypeInferResult::new(Expr::Sequence(checked_stmts), inferred_type)
    }

    fn check_stmt(
        &mut self,
        tc_context: &mut impl TypeCheckContext,
        stmt: &WithLocation<ast::Stmt>,
        expected_type: &Expr<TypeCheckExprContext>,
    ) -> Expr<TypeCheckExprContext> {
        match &stmt.value {
            ast::Stmt::Expr(expr) => self.check(tc_context, expr, expected_type),
            _ => todo!("type checking non-expression statements in blocks"),
        }
    }

    fn infer_stmt(
        &mut self,
        tc_context: &mut impl TypeCheckContext,
        stmt: &WithLocation<ast::Stmt>,
    ) -> TypeInferResult {
        match &stmt.value {
            ast::Stmt::Expr(expr) => self.infer(tc_context, expr),
            _ => todo!("inferring non-expression statements in blocks"),
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
            CalleeInfo::VariableTupleElement(vte) => {
                let infer = self.infer_variable_tuple_element(tc_context, vte, call.arguments);
                self.check_inferred_type(call.location, infer, expected_type)
            }
            CalleeInfo::Overloadable(_) => todo!(),
            CalleeInfo::Expr(f) => todo!("Unimplement function expression calls: {:?}", f),
            CalleeInfo::TypeN => {
                if !call.arguments.is_empty() {
                    todo!()
                }

                self.check_inferred_type(
                    call.location,
                    TypeInferResult::new(Expr::type_n(0), Expr::type_n(1)),
                    expected_type,
                )
            }
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
            CalleeInfo::VariableTupleElement(vte) => self.infer_variable_tuple_element(tc_context, vte, call.arguments),
            CalleeInfo::Overloadable(_) => todo!(),
            CalleeInfo::Expr(_) => todo!(),
            CalleeInfo::TypeN => {
                if !call.arguments.is_empty() {
                    todo!()
                }

                TypeInferResult::new(Expr::type_n(0), Expr::type_n(1))
            }
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

    fn infer_variable_tuple_element(
        &mut self,
        tc_context: &mut impl TypeCheckContext,
        vte: VariableTupleElement<TypeCheckExprContext>,
        args: VecDeque<ArgumentInfo<'_>>,
    ) -> TypeInferResult {
        let t = vte.binding_type;
        let expr = Expr::TupleElement(
            Box::new(Expr::Variable(vte.variable)),
            vte.index,
        );
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

                ast::Expr::Type => {
                    return CallInfo {
                        location: call_location,
                        callee: CalleeInfo::TypeN,
                        arguments,
                    };
                }

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
        op: ast::BinaryOperatorIdentifier,
        op_location: &'a Location,
        right: &'a WithLocation<ast::Expr>,
    ) -> CallInfo<'a> {
        CallInfo {
            location,
            callee: self.process_lookup(tc_context, op_location, &Identifier::BinaryOp(op)),
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
        op: ast::UnaryOperatorIdentifier,
        op_location: &'a Location,
        arg: &'a WithLocation<ast::Expr>,
    ) -> CallInfo<'a> {
        CallInfo {
            location,
            callee: self.process_lookup(tc_context, op_location, &Identifier::UnaryOp(op)),
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
        identifier: &Identifier,
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
            Lookup::VariableTupleElement(vte) => CalleeInfo::VariableTupleElement(vte),

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

        match &inferred_type {
            Expr::Type(n) => match expected_type {
                Expr::AnyType | Expr::BigType(_) => return checked_expr,
                Expr::Type(n2) => match (&**n, &**n2) {
                    (Expr::IntLiteral(n), Expr::IntLiteral(n2)) => {
                        if n >= &BigInt::ZERO && n2 >= &BigInt::ZERO && n <= n2 {
                            return checked_expr;
                        }
                    }
                    _ => {}
                },
                _ => {}
            },

            _ => {}
        }

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

    fn create_if_cond_vars(&mut self, cond_expr: &Expr<TypeCheckExprContext>) -> (Option<Variable<TypeCheckExprContext>>, Option<Variable<TypeCheckExprContext>>) {
        let is_pure_cond = PurityScanner::contains_impure_function_call(cond_expr);

        // TODO: Get conditional vars from pattern expressions
        let when_true_var =
            if is_pure_cond { Some(self.create_if_branch_var(cond_expr, true)) }
            else { None };
        let when_false_var =
            if is_pure_cond { Some(self.create_if_branch_var(cond_expr, false)) }
            else { None };

        (when_true_var, when_false_var)
    }

    fn create_if_branch_var(&mut self, cond_expr: &Expr<TypeCheckExprContext>, equal_to_value: bool) -> Variable<TypeCheckExprContext> {
        Variable::Local(Arc::new(LocalVariable {
            name: None,
            var_type: Expr::Builtin {
                builtin: Builtin::EqualToType,
                arguments: vec![cond_expr.clone(), Expr::BoolLiteral(equal_to_value)],
            },
            erasure_mode: ErasureMode::Erased,
            is_witness: false,
            is_mutable: false,
        }))
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
    VariableTupleElement(VariableTupleElement<TypeCheckExprContext>),
    Overloadable(OverloadLookup),
    TypeN,
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
    use std::sync::Arc;

    struct TestScope;

    impl Scope for TestScope {
        type ExprContext = TypeCheckExprContext;

        fn lookup(&mut self, _name: &Identifier) -> Lookup<TypeCheckExprContext> {
            panic!("lookup is not needed for builtin tests")
        }

        fn lookup_assign(&mut self, _name: &Identifier) -> Lookup<TypeCheckExprContext> {
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

    fn expr_stmt(expr: ast::Expr) -> WithLocation<ast::Stmt> {
        WithLocation::new(
            ast::Stmt::Expr(WithLocation::new(expr, test_location())),
            test_location(),
        )
    }

    fn block(stmts: Vec<WithLocation<ast::Stmt>>) -> WithLocation<Vec<WithLocation<ast::Stmt>>> {
        WithLocation::new(stmts, test_location())
    }

    fn unit_expr() -> ast::Expr {
        ast::Expr::Tuple { items: vec![] }
    }

    fn test_checker() -> (
        RecordingTypeChecker<Context>,
        TestTypeCheckContext,
        TestReporter,
    ) {
        let reporter = TestReporter::default();
        let context: Context = Arc::new(TestContext::new(reporter.clone()));
        (
            RecordingTypeChecker::new(context),
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

    #[test]
    fn empty_block_checks_as_unit() {
        let (mut checker, mut tc_context, reporter) = test_checker();
        let block = block(vec![]);

        let result = checker.check_block(&mut tc_context, &block, &Expr::unit_type());

        assert!(matches!(result, Expr::Tuple { items } if items.is_empty()));
        assert!(reporter.errors().is_empty());
    }

    #[test]
    fn block_infers_last_statement_type() {
        let (mut checker, mut tc_context, reporter) = test_checker();
        let block = block(vec![
            expr_stmt(unit_expr()),
            expr_stmt(ast::Expr::IntLiteral(BigInt::from(3))),
        ]);

        let result = checker.infer_block(&mut tc_context, &block);

        assert!(unify(&result.inferred_type, &Expr::int_type()));
        assert!(reporter.errors().is_empty());
    }

    #[test]
    fn block_requires_leading_statements_to_be_unit() {
        let (mut checker, mut tc_context, reporter) = test_checker();
        let block = block(vec![
            expr_stmt(ast::Expr::IntLiteral(BigInt::from(2))),
            expr_stmt(ast::Expr::IntLiteral(BigInt::from(3))),
        ]);

        let result = checker.infer_block(&mut tc_context, &block);

        assert!(unify(&result.inferred_type, &Expr::int_type()));
        let errors = reporter.errors();
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].code, ErrorCode::TypeMismatch);
    }
}
