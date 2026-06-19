use crate::modifiers::{ERASURE_MODE, IS_WITNESS, ModifierParser};
use alloc::borrow::{Cow, ToOwned};
use alloc::collections::VecDeque;
use alloc::{boxed::Box, format, string::ToString, sync::Arc, vec, vec::Vec};
use argon_compiler::access::AccessToken;
use argon_compiler::expr_type::ExprTypeContext;
use argon_compiler::scanner::PurityScanner;
use argon_compiler::scope::{self, LocalScope, LocalVariableScope, Lookup, Scope, ShiftedScope};
use argon_compiler::shifter::DefaultToExprTypeContextShifter;
use argon_compiler::signature::SignatureParameter;
use argon_compiler::{Context, Declaration, DefaultExprComparer, DefaultExprContext, Function, FunctionImplementation, FunctionSignature, MethodOwner, ModuleExportBinding, ModulePath, RecordField, RecordFieldOwner, SubstFunctionSignature, Trait, TubeName};
use argon_expr::{BlockLabel, BlockLabelDeclaration, BlockLabelKind, Builtin, ClosureParameterVariable, ErasureMode, Expr, ExprContext, ExprContextShifter, ExprScanner, ExprScannerMut, ExpressionOwner, LocalVariable, LoopLabels, MatchCase, Normalizer, NormalizerScanner, Pattern, RecordFieldLiteral, RecordType, SubstScanner, TraitType, Unify, Variable, VariableTupleElement};
use argon_parser::ast;
use argon_parser::ast::{FunctionLiteral, FunctionParameterListType, Identifier, StringFragment};
use argon_util::{CompileError, Fuel, UniqueIdentifier};
use core::fmt::{Debug, Formatter};
use core::hash::{Hash, Hasher};
use core::{mem, ptr};
use hashbrown::{HashMap, HashSet, hash_map};
use mitsein::vec1;
use mitsein::vec1::Vec1;
use num_bigint::BigInt;
use argon_compiler::erasure::ErasureScanner;
use parse18_runtime::{Location, WithLocation};

mod exhaustive;
mod overload;

use crate::type_checker::exhaustive::ExhaustiveChecker;
use overload::{OverloadResolver, Overloadable, substitute_arg_in_param_types};

pub fn type_check_type_expr(
    context: Context,
    options: TypeCheckOptions<'_>,
    e: &WithLocation<ast::Expr>,
) -> Expr<DefaultExprContext> {
    let TypeCheckOptions { access, scope, erasure_mode } = options;
    let shifted_scope = ShiftedScope::new(scope, default_to_type_check_shifter());
    let mut local_scope = LocalVariableScope::new(shifted_scope);
    let mut model = Model::new();
    let mut checker = TypeChecker {
        context: context.clone(),
        access,
        scope: &mut local_scope,
        model: &mut model,
        erasure_check_mode: erasure_mode,
    };

    let expr = checker.check_type(e);

    let result = TypeCheckToDefaultExprContextShifter { context: context.clone(), model }.shift(expr);

    check_erasure(context, options, e, &result);

    result
}

pub fn type_check_expr(
    context: Context,
    options: TypeCheckOptions<'_>,
    e: &WithLocation<ast::Expr>,
    expected_type: &Expr<DefaultExprContext>,
) -> Expr<DefaultExprContext> {
    let TypeCheckOptions { access, scope, erasure_mode } = options;
    let shifted_scope = ShiftedScope::new(scope, default_to_type_check_shifter());
    let mut local_scope = LocalVariableScope::new(shifted_scope);
    let mut model = Model::new();
    let mut checker = TypeChecker {
        context: context.clone(),
        access,
        scope: &mut local_scope,
        model: &mut model,
        erasure_check_mode: erasure_mode,
    };

    let expected_type = default_to_type_check_shifter().shift(expected_type.clone());
    let expr = checker.check(e, &expected_type);

    let result = TypeCheckToDefaultExprContextShifter { context: context.clone(), model }.shift(expr);

    check_erasure(context, options, e, &result);

    result
}

fn check_erasure(
    context: Context,
    options: TypeCheckOptions<'_>,
    e: &WithLocation<ast::Expr>,
    result: &Expr<DefaultExprContext>,
) {
    let mut erasure_scanner = ErasureScanner {
        context: context.clone(),
        comparer: &mut DefaultExprComparer { context },
        expected_erasure: options.erasure_mode,
        location: &e.location,
        suppress_errors: false,
    };
    erasure_scanner.scan(&result);
}

pub struct TypeCheckOptions<'a> {
    access: &'a AccessToken,
    scope: &'a dyn Scope<ExprContext = DefaultExprContext>,
    erasure_mode: ErasureMode,
}

impl<'a> TypeCheckOptions<'a> {
    pub fn new(
        access: &'a AccessToken,
        scope: &'a dyn Scope<ExprContext = DefaultExprContext>,
        erasure_mode: ErasureMode,
    ) -> Self {
        Self { access, scope, erasure_mode }
    }
}

#[derive(Debug, Clone)]
struct TypeCheckExprContext {}

impl ExprContext for TypeCheckExprContext {
    type Hole = Hole;
    type Function = <DefaultExprContext as ExprContext>::Function;
    type Method = <DefaultExprContext as ExprContext>::Method;
    type Record = <DefaultExprContext as ExprContext>::Record;
    type RecordField = <DefaultExprContext as ExprContext>::RecordField;
    type Enum = <DefaultExprContext as ExprContext>::Enum;
    type EnumVariant = <DefaultExprContext as ExprContext>::EnumVariant;
    type Trait = <DefaultExprContext as ExprContext>::Trait;
    type Instance = <DefaultExprContext as ExprContext>::Instance;
}

type ExpectedType<'a> = argon_expr::ExpectedType<'a, TypeCheckExprContext>;

struct TypeCheckToDefaultExprContextShifter {
    context: Context,
    model: Model,
}

impl ExprContextShifter for TypeCheckToDefaultExprContextShifter {
    type EC1 = TypeCheckExprContext;
    type EC2 = DefaultExprContext;

    fn shift_hole(&mut self, hole: Hole) -> Expr<DefaultExprContext> {
        match self.model.hole_values.get(&hole) {
            Some(value) => self.shift(value.clone()),
            None => {
                self.context
                    .reporter()
                    .report_error(CompileError::could_not_infer(
                        hole.hole_info.location.clone(),
                    ));

                Expr::Error
            }
        }
    }
}

type DefaultTypeCheckShifter = DefaultToExprTypeContextShifter<TypeCheckExprContext>;

fn default_to_type_check_shifter() -> DefaultTypeCheckShifter {
    DefaultToExprTypeContextShifter::default()
}

impl ExprTypeContext for TypeCheckExprContext {
    fn get_hole_type(hole: &Hole) -> Expr<Self> {
        hole.hole_info.hole_type.clone()
    }
}

struct ExprNormalizer<'a> {
    model: &'a mut Model,
}

impl Normalizer for ExprNormalizer<'_> {
    type EC = TypeCheckExprContext;

    fn resolve_hole(&mut self, hole: &<Self::EC as ExprContext>::Hole) -> Option<Expr<Self::EC>> {
        self.model.hole_values.get(hole).cloned()
    }

    fn get_function_body(
        &mut self,
        function: &Arc<dyn Function>,
        arguments: &mut Vec<Expr<TypeCheckExprContext>>,
    ) -> Option<Expr<TypeCheckExprContext>> {
        if !function.metadata().is_inline {
            return None;
        }

        let implementation = function.clone().implementation()?;
        let FunctionImplementation::Expr(body) = implementation.as_ref() else {
            return None;
        };

        let signature = function.clone().signature();
        let owner: ExpressionOwner<TypeCheckExprContext> =
            ExpressionOwner::Function(function.clone());
        let mut body = default_to_type_check_shifter().shift(body.clone());

        let arguments = mem::take(arguments);
        let mut subst = SubstScanner::new();
        let mut shifter = default_to_type_check_shifter();
        let signature = signature.as_ref().clone().shift(&mut shifter);
        subst.add_function_parameter_substitutions(owner, &signature, &arguments);
        subst.scan(&mut body);

        Some(body)
    }
}

struct HoleInfo {
    location: Location,
    hole_type: Expr<TypeCheckExprContext>, // TODO: Change to ExpectedType
}

#[derive(Clone)]
struct Hole {
    hole_info: Arc<HoleInfo>,
}

impl Hole {
    fn new(location: Location, hole_type: Expr<TypeCheckExprContext>) -> Self {
        Self {
            hole_info: Arc::new(HoleInfo {
                location,
                hole_type,
            }),
        }
    }
}

impl Debug for Hole {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        write!(
            f,
            "Hole({})",
            self.hole_info.as_ref() as *const HoleInfo as usize
        )
    }
}

impl PartialEq for Hole {
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self.hole_info.as_ref(), other.hole_info.as_ref())
    }
}

impl Eq for Hole {}

impl Hash for Hole {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (self.hole_info.as_ref() as *const HoleInfo as *const ()).hash(state);
    }
}

#[derive(Clone)]
struct Model {
    hole_values: HashMap<Hole, Expr<TypeCheckExprContext>>,
}

impl Model {
    fn new() -> Self {
        Self {
            hole_values: HashMap::new(),
        }
    }
}

#[derive(Debug, Clone)]
enum TypeInferResult<'a> {
    // Fully inferred type
    Complete(InferredType),

    // Types that needs additional type information
    Closure {
        location: &'a Location,
        function_literal: &'a FunctionLiteral,
    },

    Tuple {
        location: &'a Location,
        elements: Vec<TypeInferResult<'a>>,
    },

    // The remaining cases are compound expressions that may have subexpressions that require additional type information
    Finally {
        body_result: Box<TypeInferResult<'a>>,
        finally_body: Box<Expr<TypeCheckExprContext>>,
    },

    IfElse {
        condition: Box<Expr<TypeCheckExprContext>>,
        true_body: Box<TypeInferResult<'a>>,
        false_body_location: &'a Location,
        false_body: Box<TypeInferResult<'a>>,
    },

    Match {
        expression: Box<Expr<TypeCheckExprContext>>,
        arms: Vec<InferResultMatchArm<'a>>,
    },

    Sequence {
        init_exprs: Vec1<Expr<TypeCheckExprContext>>,
        last_result: Box<TypeInferResult<'a>>,
    },
}

impl<'a> TypeInferResult<'a> {
    fn error() -> Self {
        Self::Complete(InferredType::error())
    }

    fn partially_inferred_type<'b>(&'b self) -> PartiallyInferredType<'b> {
        match self {
            TypeInferResult::Complete(inferred_type) => {
                PartiallyInferredType::Full(Cow::Borrowed(&inferred_type.inferred_type))
            }
            TypeInferResult::Closure { .. } => PartiallyInferredType::Closure,
            TypeInferResult::Tuple { elements, .. } => PartiallyInferredType::Tuple(
                elements
                    .iter()
                    .map(|e| e.partially_inferred_type())
                    .collect(),
            ),
            TypeInferResult::Finally { body_result, .. } => body_result.partially_inferred_type(),
            TypeInferResult::IfElse { true_body, .. } => true_body.partially_inferred_type(),
            TypeInferResult::Match { arms, .. } => arms
                .first()
                .map(|arm| arm.body.partially_inferred_type())
                .unwrap_or_else(|| PartiallyInferredType::Full(Cow::Owned(Expr::never_type()))),
            TypeInferResult::Sequence { last_result, .. } => last_result.partially_inferred_type(),
        }
    }

    fn infer_fully(self) -> InferredType {
        match self {
            TypeInferResult::Complete(inferred_type) => inferred_type,
            _ => todo!("infer_fully: {:?}", self),
        }
    }
}

#[derive(Debug, Clone)]
struct InferResultMatchArm<'a> {
    location: &'a Location,
    pattern: Pattern<TypeCheckExprContext>,
    body: TypeInferResult<'a>,
}

#[derive(Debug, Clone)]
struct InferredType {
    inferred_type: Expr<TypeCheckExprContext>,
    checked_expr: Expr<TypeCheckExprContext>,
}

impl InferredType {
    fn error() -> Self {
        InferredType {
            inferred_type: Expr::Error,
            checked_expr: Expr::Error,
        }
    }
}

#[derive(Debug)]
enum PartiallyInferredType<'b> {
    Full(Cow<'b, Expr<TypeCheckExprContext>>),
    Closure,
    Tuple(Vec<PartiallyInferredType<'b>>),
}

struct TypeChecker<'access, 'scope, 'model> {
    context: Context,
    access: &'access AccessToken,
    scope: &'scope mut dyn LocalScope<ExprContext = TypeCheckExprContext>,
    model: &'model mut Model,
    erasure_check_mode: ErasureMode,
}

macro_rules! with_nested_scope {
    ($tc:ident) => {
        TypeChecker {
            context: $tc.context.clone(),
            access: $tc.access,
            scope: &mut LocalVariableScope::new(
                &mut *$tc.scope as &mut dyn Scope<ExprContext = TypeCheckExprContext>,
            ),
            model: $tc.model,
            erasure_check_mode: $tc.erasure_check_mode,
        }
    };
}

impl<'access, 'scope, 'model> TypeChecker<'access, 'scope, 'model> {
    fn check<'e>(
        &mut self,
        expr: &'e WithLocation<ast::Expr>,
        expected_type: &Expr<TypeCheckExprContext>,
    ) -> Expr<TypeCheckExprContext> {
        let infer = self.infer(expr);
        self.check_inferred_type(&expr.location, infer, ExpectedType::Exact(expected_type))
            .checked_expr
    }

    fn check_type<'e>(&mut self, expr: &'e WithLocation<ast::Expr>) -> Expr<TypeCheckExprContext> {
        self.check_type_with_meta_type(expr).checked_expr
    }

    fn check_type_with_meta_type<'e>(&mut self, expr: &'e WithLocation<ast::Expr>) -> InferredType {
        let infer = self.infer(expr);
        let mut inferred = self.check_inferred_type(&expr.location, infer, ExpectedType::AnyMetaType);

        if !self.treat_as_token(&inferred.checked_expr) {
            inferred.checked_expr = Expr::BoxedType(Box::new(inferred.checked_expr));
        }

        inferred
    }

    fn treat_as_token(&self, expr: &Expr<TypeCheckExprContext>) -> bool {
        match expr {
            Expr::EnumType(_)
            | Expr::FunctionType { .. }
            | Expr::InstanceType(_)
            | Expr::RecordType(_)
            | Expr::TraitType(_)
            | Expr::Tuple { .. }
            | Expr::Type(_)
            | Expr::BigType(_)
            | Expr::BoxedType(_) => true,

            Expr::FunctionCall { function, .. } =>
                function.metadata().erasure_mode == ErasureMode::Token,

            Expr::Variable(v) => v.erasure_mode() == ErasureMode::Token,

            _ => false,
        }
    }

    fn infer<'e>(&mut self, expr: &'e WithLocation<ast::Expr>) -> TypeInferResult<'e> {
        match &expr.value {
            ast::Expr::Error => TypeInferResult::error(),

            ast::Expr::As { value, value_type } => {
                let t = self.check_type(value_type);
                let e = self.check(value, &t);

                TypeInferResult::Complete(InferredType {
                    checked_expr: e,
                    inferred_type: t,
                })
            }

            ast::Expr::BoolLiteral(value) => {
                TypeInferResult::Complete(InferredType {
                    checked_expr: Expr::BoolLiteral(*value),
                    inferred_type: Expr::bool_type(),
                })
            },
            ast::Expr::IntLiteral(value) => {
                TypeInferResult::Complete(InferredType {
                    checked_expr: Expr::IntLiteral(value.clone()),
                    inferred_type: Expr::int_type(),
                })
            },

            ast::Expr::Break { label, value } => {
                let block_label = match label {
                    Some(label) => {
                        self.scope
                            .lookup_block_label(&label.value)
                            .map(|decl| match decl {
                                BlockLabelDeclaration::Block(label) => label,
                                BlockLabelDeclaration::Loop(labels) => labels.break_label(),
                            })
                    }
                    None => self.scope.latest_loop_labels().map(LoopLabels::break_label),
                };

                let Some(block_label) = block_label else {
                    self.context
                        .reporter()
                        .report_error(CompileError::loop_label_not_found(
                            label
                                .as_ref()
                                .map(|label| &label.location)
                                .unwrap_or(&expr.location)
                                .clone(),
                        ));

                    return TypeInferResult::error();
                };

                let t = block_label.block_result_type.clone();
                let value = match value {
                    Some(value) => self.check(value, &t),
                    None => {
                        let infer = TypeInferResult::Complete(InferredType {
                            checked_expr: Expr::unit(),
                            inferred_type: Expr::unit(),
                        });
                        self.check_inferred_type(&expr.location, infer, ExpectedType::Exact(&t))
                            .checked_expr
                    }
                };

                TypeInferResult::Complete(InferredType {
                    checked_expr: Expr::Break {
                        label: Box::new(block_label),
                        value: Box::new(value),
                    },
                    inferred_type: Expr::never_type(),
                })
            }

            ast::Expr::Next { label } => {
                let block_label = match label {
                    Some(label) => self
                        .scope
                        .lookup_block_label(&label.value)
                        .and_then(|decl| match decl {
                            BlockLabelDeclaration::Block(_) => None,
                            BlockLabelDeclaration::Loop(labels) => Some(labels.next_label()),
                        }),
                    None => self.scope.latest_loop_labels().map(LoopLabels::next_label),
                };

                let Some((block_label, is_break)) = block_label else {
                    self.context
                        .reporter()
                        .report_error(CompileError::loop_label_not_found(
                            label
                                .as_ref()
                                .map(|label| &label.location)
                                .unwrap_or(&expr.location)
                                .clone(),
                        ));

                    return TypeInferResult::error();
                };

                if is_break {
                    TypeInferResult::Complete(InferredType {
                        checked_expr: Expr::Break {
                            label: Box::new(block_label),
                            value: Box::new(Expr::unit()),
                        },
                        inferred_type: Expr::never_type(),
                    })
                } else {
                    TypeInferResult::Complete(InferredType {
                        checked_expr: Expr::Retry {
                            label: Box::new(block_label),
                        },
                        inferred_type: Expr::never_type(),
                    })
                }
            }
            ast::Expr::Redo { label } => {
                let block_label = match label {
                    Some(label) => self
                        .scope
                        .lookup_block_label(&label.value)
                        .and_then(|decl| match decl {
                            BlockLabelDeclaration::Block(_) => None,
                            BlockLabelDeclaration::Loop(labels) => Some(labels.redo_label()),
                        }),
                    None => self.scope.latest_loop_labels().map(LoopLabels::redo_label),
                };

                let Some(block_label) = block_label else {
                    self.context
                        .reporter()
                        .report_error(CompileError::loop_label_not_found(
                            label
                                .as_ref()
                                .map(|label| &label.location)
                                .unwrap_or(&expr.location)
                                .clone(),
                        ));

                    return TypeInferResult::error();
                };

                TypeInferResult::Complete(InferredType {
                    checked_expr: Expr::Retry {
                        label: Box::new(block_label),
                    },
                    inferred_type: Expr::never_type(),
                })
            }
            ast::Expr::Retry { label } => {
                let block_label = match label {
                    Some(label) => self
                        .scope
                        .lookup_block_label(&label.value)
                        .and_then(|decl| match decl {
                            BlockLabelDeclaration::Block(label) => Some(label),
                            BlockLabelDeclaration::Loop(_) => None,
                        }),
                    None => self.scope.latest_block_label(),
                };

                let Some(block_label) = block_label else {
                    self.context
                        .reporter()
                        .report_error(CompileError::loop_label_not_found(
                            label
                                .as_ref()
                                .map(|label| &label.location)
                                .unwrap_or(&expr.location)
                                .clone(),
                        ));

                    return TypeInferResult::error();
                };

                TypeInferResult::Complete(InferredType {
                    checked_expr: Expr::Retry {
                        label: Box::new(block_label),
                    },
                    inferred_type: Expr::never_type(),
                })
            }

            ast::Expr::Builtin(_)
            | ast::Expr::Dot { .. }
            | ast::Expr::FunctionCall { .. }
            | ast::Expr::Identifier(_)
            | ast::Expr::Index { .. }
            | ast::Expr::Type => {
                let call = self.process_call(expr, None);
                self.infer_call(call)
            }

            ast::Expr::BinaryOperation { a, op, b } => {
                let op_id = match op.value {
                    ast::BinaryOperator::Assign => {
                        let assigned_value = AssignedValue {
                            assign_location: &op.location,
                            value: b,
                        };

                        let call = self.process_call(a, Some(assigned_value));
                        return self.infer_call(call);
                    }

                    ast::BinaryOperator::LogicalOr => {
                        let a = self.check_condition_expr(a);
                        let b = self.check_condition_expr(b);
                        return TypeInferResult::Complete(InferredType {
                            checked_expr: Expr::Or(Box::new(a), Box::new(b)),
                            inferred_type: Expr::bool_type(),
                        });
                    }
                    ast::BinaryOperator::LogicalAnd => {
                        let a = self.check_condition_expr(a);
                        let b = self.check_condition_expr(b);
                        return TypeInferResult::Complete(InferredType {
                            checked_expr: Expr::And(Box::new(a), Box::new(b)),
                            inferred_type: Expr::bool_type(),
                        });
                    }

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

                let call = self.process_binary_operator(&expr.location, a, op_id, &op.location, b);
                self.infer_call(call)
            }

            ast::Expr::UnaryOperation { op, a } => {
                let op_id = match op.value {
                    ast::UnaryOperator::LogicalNot => {
                        let value = self.check_condition_expr(a);
                        return TypeInferResult::Complete(InferredType {
                            checked_expr: Expr::Not(Box::new(value)),
                            inferred_type: Expr::bool_type(),
                        });
                    }

                    ast::UnaryOperator::Plus => ast::UnaryOperatorIdentifier::Plus,
                    ast::UnaryOperator::Minus => ast::UnaryOperatorIdentifier::Minus,
                    ast::UnaryOperator::BitNot => ast::UnaryOperatorIdentifier::BitNot,
                };

                let call = self.process_unary_operator(&expr.location, op_id, &op.location, a);
                self.infer_call(call)
            }

            ast::Expr::FunctionResultValue => {
                todo!()
            }

            ast::Expr::Paren(inner) => self.infer(inner),

            ast::Expr::RecordLiteral {
                record_expr,
                fields,
            } => {
                'not_record: {
                    let record_call = self.process_call(record_expr, None);

                    let overloads = match record_call.callee {
                        CalleeInfo::Error => return TypeInferResult::error(),
                        CalleeInfo::Overloadable(overloads) => overloads,
                        _ => break 'not_record,
                    };

                    let resolved = OverloadResolver::new(self, record_call.location)
                        .resolve_overload_lookup(overloads, record_call.arguments);

                    let Some(mut selected_overload) = resolved.overload else {
                        return TypeInferResult::error();
                    };

                    let owner: RecordFieldOwner;
                    let expr_owner: ExpressionOwner<DefaultExprContext>;
                    let record_fields: Arc<Vec<Arc<dyn RecordField>>>;
                    let record_type_signature: Arc<FunctionSignature<DefaultExprContext>>;

                    match selected_overload.overload {
                        Overloadable::Base(scope::Overloadable::Record(r)) => {
                            owner = RecordFieldOwner::Record(r.clone());
                            expr_owner = ExpressionOwner::Record(r.clone());
                            record_fields = r.clone().fields();
                            record_type_signature = r.clone().signature();
                        }
                        Overloadable::Base(scope::Overloadable::EnumVariant(v)) => {
                            owner = RecordFieldOwner::EnumVariant(v.clone());
                            expr_owner = ExpressionOwner::EnumVariant(v.clone());
                            record_fields = v.clone().fields();
                            record_type_signature = v.clone().signature();
                        }

                        _ => break 'not_record,
                    };

                    if !resolved.extra_argument_info.is_empty() {
                        self.context.reporter().report_error(
                            CompileError::record_literal_extra_arguments(
                                record_expr.location.clone(),
                            ),
                        );
                    }

                    if !selected_overload.unspecified_parameters.is_empty() {
                        self.context.reporter().report_error(
                            CompileError::record_literal_missing_arguments(
                                record_expr.location.clone(),
                            ),
                        );

                        selected_overload.args.extend(
                            selected_overload
                                .unspecified_parameters
                                .drain(..)
                                .map(|_| Expr::Error),
                        );
                    }

                    let mut subst = SubstScanner::new();
                    for (i, (param, arg)) in record_type_signature
                        .parameters
                        .iter()
                        .zip(selected_overload.args.iter())
                        .enumerate()
                    {
                        let v = param.clone().to_parameter_var(expr_owner.clone(), i);
                        let v = default_to_type_check_shifter()
                            .shift_variable(Variable::Parameter(Box::new(v)));
                        subst.add_substitution(v, Cow::Borrowed(arg));
                    }

                    let mut seen_field_names = HashSet::new();
                    let mut converted_fields = Vec::new();

                    let mut remaining_fields = record_fields
                        .iter()
                        .cloned()
                        .map(|field| (field.clone().metadata().name.clone(), field))
                        .collect::<HashMap<_, _>>();

                    for field_literal in &fields.value {
                        if !seen_field_names.insert(field_literal.value.name.value.clone()) {
                            self.context.reporter().report_error(
                                CompileError::duplicate_record_literal_field(
                                    field_literal.value.name.location.clone(),
                                    field_literal.value.name.value.to_string(),
                                ),
                            );
                            continue;
                        }

                        let Some(field) = remaining_fields.remove(&field_literal.value.name.value)
                        else {
                            self.context.reporter().report_error(
                                CompileError::unknown_record_literal_field(
                                    field_literal.value.name.location.clone(),
                                    field_literal.value.name.value.to_string(),
                                ),
                            );
                            continue;
                        };

                        let mut field_type = default_to_type_check_shifter()
                            .shift((*field.clone().field_type()).clone());
                        subst.scan(&mut field_type);

                        let value = self.check(&field_literal.value.value, &field_type);

                        converted_fields.push(RecordFieldLiteral { field, value });
                    }
                    drop(subst);

                    if !remaining_fields.is_empty() {
                        self.context.reporter().report_error(
                            CompileError::missing_record_literal_field(
                                expr.location.clone(),
                                &remaining_fields
                                    .into_keys()
                                    .map(|name| name.to_string())
                                    .collect::<Vec<_>>(),
                            ),
                        )
                    }

                    let expr: Expr<TypeCheckExprContext>;
                    let expr_type: Expr<TypeCheckExprContext>;
                    match owner {
                        RecordFieldOwner::Record(r) => {
                            expr = Expr::RecordLiteral {
                                record_type: RecordType {
                                    record: r.clone(),
                                    arguments: selected_overload.args.clone(),
                                },
                                fields: converted_fields,
                            };
                            expr_type = Expr::RecordType(RecordType {
                                record: r.clone(),
                                arguments: selected_overload.args,
                            });
                        }
                        RecordFieldOwner::EnumVariant(v) => {
                            let enum_type = match selected_overload.return_type {
                                Expr::Error => return TypeInferResult::error(),
                                Expr::EnumType(t) => t,
                                _ => panic!("Expected enum type"),
                            };

                            expr_type = Expr::EnumType(enum_type.clone());
                            expr = Expr::EnumVariantLiteral {
                                enum_type,
                                variant: v.clone(),
                                arguments: selected_overload.args,
                                fields: converted_fields,
                            };
                        }
                    }

                    return TypeInferResult::Complete(InferredType {
                        checked_expr: expr,
                        inferred_type: expr_type,
                    });
                }

                self.context
                    .reporter()
                    .report_error(CompileError::record_type_required(
                        record_expr.location.clone(),
                    ));
                TypeInferResult::error()
            }

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
                                self.check(e, &Expr::string_type())
                            }
                        })
                        .collect::<Vec<_>>();
                    parts
                        .into_iter()
                        .reduce(|lhs, rhs| {
                            Expr::Builtin(Builtin::StringConcat {
                                lhs: Box::new(lhs),
                                rhs: Box::new(rhs),
                            })
                        })
                        .unwrap_or_else(|| Expr::StringLiteral(Box::from("")))
                };

                TypeInferResult::Complete(InferredType {
                    checked_expr: str_expr,
                    inferred_type: Expr::string_type(),
                })
            }
            ast::Expr::BigType(value) => TypeInferResult::Complete(InferredType {
                checked_expr: Expr::BigType(value.clone()),
                inferred_type: Expr::BigType(value + 1),
            }),
            ast::Expr::Assert { .. } => todo!("infer assert expressions"),
            ast::Expr::Block { body, finally_body } => {
                let mut result = self.infer_block(body);

                if let Some(finally_body) = finally_body {
                    let checked_finally_body = self.check_block(finally_body, &Expr::unit());

                    result = match result {
                        TypeInferResult::Complete(InferredType {
                            checked_expr,
                            inferred_type,
                        }) => TypeInferResult::Complete(InferredType {
                            checked_expr: Expr::Finally {
                                block_body: Box::new(checked_expr),
                                finally_body: Box::new(checked_finally_body),
                            },
                            inferred_type,
                        }),

                        _ => TypeInferResult::Finally {
                            body_result: Box::new(result),
                            finally_body: Box::new(checked_finally_body),
                        },
                    };
                }

                result
            }
            ast::Expr::FunctionLiteral(func) => TypeInferResult::Closure {
                location: &expr.location,
                function_literal: func,
            },

            ast::Expr::FunctionType { a, r } => {
                let arg_type = self.check_type(a);
                let ret_type = self.check_type_with_meta_type(r);

                // TODO: Figure out the proper kind

                let arg = ClosureParameterVariable {
                    id: UniqueIdentifier::new(),
                    var_type: arg_type,
                    name: None,
                    is_mutable: false,
                    erasure_mode: ErasureMode::Concrete,
                    is_witness: false,
                };

                TypeInferResult::Complete(InferredType {
                    checked_expr: Expr::FunctionType {
                        a: Box::new(arg),
                        r: Box::new(ret_type.checked_expr),
                    },
                    inferred_type: ret_type.inferred_type,
                })
            }

            ast::Expr::IfElse {
                condition,
                when_true,
                when_false,
            } => {
                let conv_condition = self.check_condition_expr(condition);

                let mut when_true_vars = Vec::new();
                let mut when_false_vars = Vec::new();
                get_condition_vars(&conv_condition, &mut when_true_vars, &mut when_false_vars);

                let true_body_result = {
                    let mut checker = with_nested_scope!(self);
                    for v in when_true_vars {
                        checker.scope.add_variable(Variable::Local(Box::new(v)));
                    }
                    checker.infer_block(when_true)
                };

                let false_body_result = {
                    let mut checker = with_nested_scope!(self);
                    for v in when_false_vars {
                        checker.scope.add_variable(Variable::Local(Box::new(v)));
                    }
                    checker.infer_block(when_false)
                };

                match (&true_body_result, &false_body_result) {
                    (
                        TypeInferResult::Complete(false_body_inferred),
                        TypeInferResult::Complete(true_body_inferred),
                    ) => {
                        let mut branch_typer = BranchTyper::new();
                        branch_typer.add_branch(self, &true_body_inferred.inferred_type);
                        branch_typer.add_branch(self, &false_body_inferred.inferred_type);

                        let branch_type = branch_typer.into_branch_type();

                        let checked_true_body = self
                            .check_inferred_type(
                                &when_true.location,
                                true_body_result,
                                ExpectedType::Exact(&branch_type),
                            )
                            .checked_expr;

                        let checked_false_body = self
                            .check_inferred_type(
                                &when_false.location,
                                false_body_result,
                                ExpectedType::Exact(&branch_type),
                            )
                            .checked_expr;

                        TypeInferResult::Complete(InferredType {
                            checked_expr: Expr::IfElse {
                                condition: Box::new(conv_condition),
                                when_true: Box::new(checked_true_body),
                                when_false: Box::new(checked_false_body),
                            },
                            inferred_type: branch_type,
                        })
                    }

                    _ => TypeInferResult::IfElse {
                        condition: Box::new(conv_condition),
                        true_body: Box::new(true_body_result),
                        false_body_location: &when_false.location,
                        false_body: Box::new(false_body_result),
                    },
                }
            }
            ast::Expr::Is { value, pattern } => {
                let conv_value = self.infer(value).infer_fully();
                let conv_pattern = self.check_pattern(pattern, conv_value.inferred_type);

                TypeInferResult::Complete(InferredType {
                    checked_expr: Expr::Is {
                        value: Box::new(conv_value.checked_expr),
                        pattern: Box::new(conv_pattern),
                    },
                    inferred_type: Expr::bool_type(),
                })
            }

            ast::Expr::Loop { body, label } => {
                let mut checker = with_nested_scope!(self);

                let label_location = label
                    .as_ref()
                    .map(|label| &label.location)
                    .unwrap_or(&expr.location)
                    .clone();

                let block_label_hole = Hole::new(label_location, Expr::type_n(0));
                let mut block_label = BlockLabel {
                    id: UniqueIdentifier::new(),
                    name: label.as_ref().map(|l| l.value.clone()),
                    kind: BlockLabelKind::Loop,
                    block_result_type: Expr::Hole(block_label_hole.clone()),
                };

                checker
                    .scope
                    .add_block_label(BlockLabelDeclaration::Loop(LoopLabels::Loop(
                        block_label.clone(),
                    )));

                let mut body = checker.check_block(body, &Expr::unit());
                match body {
                    Expr::Sequence(ref mut items) => {
                        items.push(Expr::Retry {
                            label: Box::new(block_label.clone()),
                        });
                    }

                    _ => {
                        body = Expr::Sequence(vec1![
                            body,
                            Expr::Retry {
                                label: Box::new(block_label.clone()),
                            },
                        ]);
                    }
                }

                let loop_type = match checker.model.hole_values.get(&block_label_hole) {
                    Some(t) => t.clone(),
                    None => {
                        block_label.block_result_type = Expr::never_type();
                        Expr::never_type()
                    }
                };

                TypeInferResult::Complete(InferredType {
                    checked_expr: Expr::Block {
                        label: Box::new(block_label),
                        body: Box::new(body),
                    },
                    inferred_type: loop_type,
                })
            }

            ast::Expr::Match { value, cases } => {
                enum CaseResults<'a> {
                    FullyInferred(Vec<(&'a Location, Pattern<TypeCheckExprContext>, InferredType)>),
                    InferResults(Vec<InferResultMatchArm<'a>>),
                }

                impl<'a> CaseResults<'a> {
                    fn as_infer_results(&mut self) -> &mut Vec<InferResultMatchArm<'a>> {
                        match self {
                            CaseResults::FullyInferred(cases) => {
                                let infer_res = mem::take(cases)
                                    .into_iter()
                                    .map(|(location, pattern, inferred)| InferResultMatchArm {
                                        location,
                                        pattern,
                                        body: TypeInferResult::Complete(inferred),
                                    })
                                    .collect();

                                *self = CaseResults::InferResults(infer_res);

                                match self {
                                    CaseResults::InferResults(infer_res) => infer_res,
                                    _ => unreachable!(),
                                }
                            }

                            CaseResults::InferResults(infer_res) => infer_res,
                        }
                    }
                }

                let value = self.infer(value).infer_fully();

                let mut case_res = CaseResults::FullyInferred(Vec::new());
                let mut branch_typer = BranchTyper::new();

                for case in cases {
                    let pattern =
                        self.check_pattern(&case.value.pattern, value.inferred_type.clone());

                    let body = {
                        let mut nested = with_nested_scope!(self);
                        let mut pattern_vars = Vec::new();
                        get_pattern_vars(&pattern, &mut pattern_vars);
                        for v in pattern_vars {
                            nested.scope.add_variable(Variable::Local(Box::new(v)));
                        }
                        nested.infer(&case.value.body)
                    };

                    match (&mut case_res, body) {
                        (
                            CaseResults::FullyInferred(cases),
                            TypeInferResult::Complete(inferred),
                        ) => {
                            branch_typer.add_branch(self, &inferred.inferred_type);
                            cases.push((&case.location, pattern, inferred));
                        }

                        (case_res, body) => {
                            case_res.as_infer_results().push(InferResultMatchArm {
                                location: &case.location,
                                pattern,
                                body,
                            });
                        }
                    }
                }

                let mut exhaustive_check = ExhaustiveChecker {
                    location: &expr.location,
                    tc: self,
                };

                let is_exhaustive = match &case_res {
                    CaseResults::FullyInferred(arms) => {
                        let patterns = arms.iter().map(|(_, pattern, _)| pattern);
                        exhaustive_check.check(&value.inferred_type, patterns)
                    }

                    CaseResults::InferResults(arms) => {
                        let patterns = arms.iter().map(|arm| &arm.pattern);
                        exhaustive_check.check(&value.inferred_type, patterns)
                    }
                };

                if !is_exhaustive {
                    todo!("report exhaustive check failure");
                }

                match case_res {
                    CaseResults::FullyInferred(inferred_res) => {
                        let branch_type = branch_typer.into_branch_type();

                        let cases = inferred_res
                            .into_iter()
                            .map(|(_, pattern, inferred)| MatchCase {
                                pattern,
                                body: inferred.checked_expr,
                            })
                            .collect();

                        TypeInferResult::Complete(InferredType {
                            checked_expr: Expr::Match {
                                value: Box::new(value.checked_expr),
                                cases,
                            },
                            inferred_type: branch_type,
                        })
                    }

                    CaseResults::InferResults(infer_res) => TypeInferResult::Match {
                        expression: Box::new(value.checked_expr),
                        arms: infer_res,
                    },
                }
            }
            ast::Expr::NewTraitObject { .. } => todo!("infer trait object construction"),

            ast::Expr::Raise { ex } => {
                let ex_trait = self.get_exception_trait();
                let ex_type = Expr::TraitType(TraitType {
                    trait_: ex_trait,
                    arguments: vec![],
                });

                let ex = self.check(ex, &ex_type);

                TypeInferResult::Complete(InferredType {
                    checked_expr: Expr::Raise { ex: Box::new(ex) },
                    inferred_type: Expr::never_type(),
                })
            }

            ast::Expr::Summon { .. } => todo!("infer summon expressions"),

            ast::Expr::Tuple { items } => {
                let element_results = items
                    .iter()
                    .map(|item| self.infer(item))
                    .collect::<Vec<_>>();

                if element_results
                    .iter()
                    .all(|elem_res| matches!(elem_res, TypeInferResult::Complete(_)))
                {
                    let mut values = Vec::new();
                    let mut types = Vec::new();

                    for elem_res in element_results {
                        let inferred_type = match elem_res {
                            TypeInferResult::Complete(inferred_type) => inferred_type,
                            _ => unreachable!(),
                        };

                        values.push(inferred_type.checked_expr);
                        types.push(inferred_type.inferred_type);
                    }

                    return TypeInferResult::Complete(InferredType {
                        checked_expr: Expr::Tuple { items: values },
                        inferred_type: Expr::Tuple { items: types },
                    });
                }

                TypeInferResult::Tuple {
                    location: &expr.location,
                    elements: element_results,
                }
            }

            ast::Expr::While {
                label,
                condition,
                body,
            } => {
                let mut checker = with_nested_scope!(self);

                let block_label = BlockLabel {
                    id: UniqueIdentifier::new(),
                    name: label.as_ref().map(|l| l.value.clone()),
                    kind: BlockLabelKind::WhileOuter,
                    block_result_type: Expr::unit(),
                };

                // Treat the label as a loop label when checking the condition
                checker
                    .scope
                    .add_block_label(BlockLabelDeclaration::Loop(LoopLabels::Loop(
                        block_label.clone(),
                    )));

                let cond = checker.check_block(condition, &Expr::bool_type());

                let inner_label = BlockLabel {
                    id: UniqueIdentifier::new(),
                    name: block_label.name.clone(),
                    kind: BlockLabelKind::WhileInner,
                    block_result_type: Expr::unit(),
                };

                checker
                    .scope
                    .add_block_label(BlockLabelDeclaration::Loop(LoopLabels::While {
                        outer: block_label.clone(),
                        inner: inner_label.clone(),
                    }));

                let mut body = checker.check_block(body, &Expr::unit());
                match body {
                    Expr::Sequence(ref mut items) => {
                        items.push(Expr::Retry {
                            label: Box::new(block_label.clone()),
                        });
                    }

                    _ => {
                        body = Expr::Sequence(vec1![
                            body,
                            Expr::Retry {
                                label: Box::new(block_label.clone()),
                            },
                        ]);
                    }
                }

                let while_loop = Expr::Block {
                    label: Box::new(block_label.clone()),
                    body: Box::new(Expr::Sequence(vec1![
                        Expr::IfElse {
                            condition: Box::new(cond),
                            when_true: Box::new(Expr::unit()),
                            when_false: Box::new(Expr::Break {
                                label: Box::new(block_label.clone()),
                                value: Box::new(Expr::unit()),
                            }),
                        },
                        Expr::Block {
                            label: Box::new(inner_label.clone()),
                            body: Box::new(body),
                        },
                        Expr::Retry {
                            label: Box::new(block_label.clone()),
                        },
                    ])),
                };

                TypeInferResult::Complete(InferredType {
                    checked_expr: while_loop,
                    inferred_type: Expr::unit(),
                })
            }

            ast::Expr::BoxedType { t } => {
                let inner = self.check_type_with_meta_type(t);

                TypeInferResult::Complete(InferredType {
                    checked_expr: Expr::BoxedType(Box::new(inner.checked_expr)),
                    inferred_type: inner.inferred_type,
                })
            }
            ast::Expr::Box { value } => {
                let value = self.infer(value).infer_fully();
                TypeInferResult::Complete(InferredType {
                    checked_expr: Expr::Box {
                        t: Box::new(value.inferred_type.clone()),
                        value: Box::new(value.checked_expr),
                    },
                    inferred_type: Expr::BoxedType(Box::new(value.inferred_type)),
                })
            }
            ast::Expr::Unbox { value } => {
                let mut value = self.infer(value).infer_fully();

                {
                    let mut norm = NormalizerScanner::new(
                        self.context.normalize_fuel(),
                        ExprNormalizer {
                            model: &mut self.model,
                        },
                    );
                    norm.normalize(&mut value.inferred_type);
                }

                let unboxed_type = match value.inferred_type {
                    Expr::BoxedType(t) => t,
                    _ => todo!("Expected boxed type"),
                };

                let inferred_type = (*unboxed_type).clone();

                TypeInferResult::Complete(InferredType {
                    checked_expr: Expr::Unbox {
                        t: unboxed_type,
                        value: Box::new(value.checked_expr),
                    },
                    inferred_type,
                })
            }
        }
    }

    fn infer_block<'e>(
        &mut self,
        body: &'e WithLocation<Vec<WithLocation<ast::Stmt>>>,
    ) -> TypeInferResult<'e> {
        let mut nested = with_nested_scope!(self);

        let Some((last_stmt, leading_stmts)) = body.value.split_last() else {
            return TypeInferResult::Complete(InferredType {
                checked_expr: Expr::unit(),
                inferred_type: Expr::unit(),
            });
        };

        let checked_stmts = leading_stmts
            .iter()
            .map(|stmt| nested.check_stmt(stmt, &Expr::unit()))
            .collect::<Vec<_>>();

        let Ok(mut checked_stmts) = Vec1::try_from(checked_stmts) else {
            return nested.infer_stmt(last_stmt);
        };

        match nested.infer_stmt(last_stmt) {
            TypeInferResult::Complete(InferredType {
                checked_expr,
                inferred_type,
            }) => {
                checked_stmts.push(checked_expr);
                TypeInferResult::Complete(InferredType {
                    checked_expr: Expr::Sequence(checked_stmts),
                    inferred_type,
                })
            }

            last_result => TypeInferResult::Sequence {
                init_exprs: checked_stmts,
                last_result: Box::new(last_result),
            },
        }
    }

    fn check_block<'e>(
        &mut self,
        body: &'e WithLocation<Vec<WithLocation<ast::Stmt>>>,
        expected_type: &Expr<TypeCheckExprContext>,
    ) -> Expr<TypeCheckExprContext> {
        let infer = self.infer_block(body);
        self.check_inferred_type(&body.location, infer, ExpectedType::Exact(expected_type))
            .checked_expr
    }

    fn check_stmt<'e>(
        &mut self,
        stmt: &'e WithLocation<ast::Stmt>,
        expected_type: &Expr<TypeCheckExprContext>,
    ) -> Expr<TypeCheckExprContext> {
        let infer = self.infer_stmt(stmt);
        self.check_inferred_type(&stmt.location, infer, ExpectedType::Exact(expected_type))
            .checked_expr
    }

    fn infer_stmt<'e>(&mut self, stmt: &'e WithLocation<ast::Stmt>) -> TypeInferResult<'e> {
        match &stmt.value {
            ast::Stmt::Expr(expr) => self.infer(expr),
            ast::Stmt::VariableDeclaration(v) => {
                let variable_value: Expr<TypeCheckExprContext>;
                let var_type: Expr<TypeCheckExprContext>;
                if let Some(t) = &v.var_type {
                    var_type = self.check_type(t);
                    variable_value = self.check(&v.value, &var_type);
                } else {
                    let result = self.infer(&v.value).infer_fully();
                    var_type = result.inferred_type;
                    variable_value = result.checked_expr;
                }

                let mut mp =
                    ModifierParser::new(self.context.clone(), &v.modifiers, &stmt.location);
                let erasure_mode = mp.parse(ERASURE_MODE);
                let is_witness = mp.parse(IS_WITNESS);
                mp.done();

                if erasure_mode == ErasureMode::Erased && v.is_mutable {
                    self.context.reporter().report_error(
                        CompileError::mutable_erased_local_variable(stmt.location.clone()),
                    );
                }

                let v = Box::new(LocalVariable {
                    id: UniqueIdentifier::new(),
                    name: v.name.clone(),
                    var_type,
                    erasure_mode,
                    is_witness,
                    is_mutable: v.is_mutable,
                });

                self.scope.add_variable(Variable::Local(v.clone()));

                TypeInferResult::Complete(InferredType {
                    checked_expr: Expr::VariableBinding(v, Box::new(variable_value)),
                    inferred_type: Expr::unit(),
                })
            }
            _ => todo!("inferring non-expression statements in blocks: {:#?}", stmt),
        }
    }

    fn infer_call<'e>(&mut self, call: CallInfo<'e>) -> TypeInferResult<'e> {
        match call.callee {
            CalleeInfo::Error => TypeInferResult::error(),
            CalleeInfo::Builtin(builtin) => {
                self.infer_builtin(call.location, builtin, call.arguments)
            }
            CalleeInfo::Variable(v) => self.infer_variable(call.location, v, call.arguments),
            CalleeInfo::VariableStore(v, value) => {
                self.infer_variable_store(call.location, v, value)
            }
            CalleeInfo::VariableTupleElement(vte) => {
                self.infer_variable_tuple_element(call.location, vte, call.arguments)
            }
            CalleeInfo::Overloadable(overloads) => {
                let resolved = OverloadResolver::new(self, call.location)
                    .resolve_overload_lookup(overloads, call.arguments);

                let Some(mut selected_overload) = resolved.overload else {
                    return TypeInferResult::error();
                };

                let mut curried_closure_params =
                    Vec::with_capacity(selected_overload.unspecified_parameters.len());
                while let Some(param) = selected_overload.unspecified_parameters.pop_front() {
                    let closure_param = ClosureParameterVariable {
                        id: UniqueIdentifier::new(),
                        name: None,
                        var_type: param.param_type.clone(),
                        is_mutable: false,
                        erasure_mode: param.erasure_mode,
                        is_witness: false,
                    };

                    let closure_param_var =
                        Variable::ClosureParameter(Box::new(closure_param.clone()));

                    if let Some(owner) = selected_overload.overload.as_expression_owner() {
                        let param_var = param.to_parameter_var(owner, selected_overload.args.len());

                        substitute_arg_in_param_types(
                            &mut selected_overload.unspecified_parameters,
                            &mut selected_overload.return_type,
                            Variable::Parameter(Box::new(param_var)),
                            &Expr::Variable(closure_param_var.clone()),
                        )
                    }

                    curried_closure_params.push(closure_param);
                    selected_overload
                        .args
                        .push(Expr::Variable(closure_param_var))
                }

                let mut inferred_type = selected_overload.into_inferred_type(self);

                for curried_closure_param in curried_closure_params.into_iter().rev() {
                    let return_type = inferred_type.inferred_type.clone();
                    inferred_type.inferred_type = Expr::FunctionType {
                        a: Box::new(curried_closure_param.clone()),
                        r: Box::new(inferred_type.inferred_type),
                    };

                    inferred_type.checked_expr = Expr::Closure {
                        v: Box::new(curried_closure_param),
                        return_type: Box::new(return_type),
                        body: Box::new(inferred_type.checked_expr),
                    };
                }

                self.infer_function_object_call_inferred(
                    TypeInferResult::Complete(inferred_type),
                    resolved
                        .extra_inferred_args
                        .into_iter()
                        .zip(resolved.extra_argument_info.iter()),
                )
            }
            CalleeInfo::Expr(e) => {
                let res = self.infer(e);
                self.infer_function_object_call(res, call.arguments)
            }
            CalleeInfo::TypeN => {
                if !call.arguments.is_empty() {
                    todo!()
                }

                TypeInferResult::Complete(InferredType {
                    checked_expr: Expr::type_n(0),
                    inferred_type: Expr::type_n(1),
                })
            }
            CalleeInfo::Index(obj, index) => {
                let index_call = self.process_method_call(
                    obj,
                    &Identifier::Index,
                    VecDeque::from(vec![ArgumentInfo {
                        call_location: &call.location,
                        arg: index,
                        list_type: FunctionParameterListType::NormalList,
                    }]),
                    None,
                );
                let result = self.infer_call(index_call);

                self.infer_function_object_call(result, call.arguments)
            }
            CalleeInfo::IndexUpdate(obj, index, value) => {
                let index_call = self.process_method_call(
                    obj,
                    &Identifier::Update(Box::new(Identifier::Index)),
                    VecDeque::from(vec![
                        ArgumentInfo {
                            call_location: &call.location,
                            arg: index,
                            list_type: FunctionParameterListType::NormalList,
                        },
                        ArgumentInfo {
                            call_location: value.assign_location,
                            arg: value.value,
                            list_type: FunctionParameterListType::NormalList,
                        },
                    ]),
                    None,
                );
                self.infer_call(index_call)
            }
        }
    }

    fn infer_builtin<'e>(
        &mut self,
        location: &'e Location,
        builtin_name: &str,
        args: VecDeque<ArgumentInfo<'e>>,
    ) -> TypeInferResult<'e> {
        macro_rules! nullary_builtin {
            ($variant:ident) => {
                self.infer_fixed_builtin(
                    location,
                    builtin_name,
                    args,
                    || [],
                    || Expr::type_n(0),
                    |[]| Builtin::$variant,
                )
            };
        }

        macro_rules! fixed_builtin {
            (
                $variant:ident,
                [$($param_type:expr),* $(,)?] => $result_type:expr,
                { $($field:ident),+ $(,)? }
            ) => {
                self.infer_fixed_builtin(
                    location,
                    builtin_name,
                    args,
                    || [$($param_type),*],
                    || $result_type,
                    |[$($field),+]| Builtin::$variant {
                        $(
                            $field: Box::new($field),
                        )+
                    },
                )
            };
        }

        macro_rules! int_to_int_binary_builtin {
            ($variant:ident) => {
                fixed_builtin!(
                    $variant,
                    [Expr::int_type(), Expr::int_type()] => Expr::int_type(),
                    { lhs, rhs }
                )
            };
        }

        macro_rules! int_to_bool_binary_builtin {
            ($variant:ident) => {
                fixed_builtin!(
                    $variant,
                    [Expr::int_type(), Expr::int_type()] => Expr::bool_type(),
                    { lhs, rhs }
                )
            };
        }

        macro_rules! same_type_bool_binary_builtin {
            ($variant:ident, $type_expr:expr) => {
                fixed_builtin!(
                    $variant,
                    [$type_expr, $type_expr] => Expr::bool_type(),
                    { lhs, rhs }
                )
            };
        }

        macro_rules! parameterized_builtin {
            (
                $variant:ident,
                $type_arg:ident,
                [$($param_type:expr),* $(,)?] => $result_type:expr,
                { $($field:ident),* $(,)? }
            ) => {
                self.infer_parameterized_builtin(
                    location,
                    builtin_name,
                    args,
                    |$type_arg| {
                        let _ = &$type_arg;
                        [$($param_type),*]
                    },
                    |$type_arg, _type_arg_type| {
                        let _ = &$type_arg;
                        $result_type
                    },
                    |$type_arg, [$($field),*]| Builtin::$variant {
                        $type_arg: Box::new($type_arg),
                        $(
                            $field: Box::new($field),
                        )*
                    },
                )
            };
        }

        match builtin_name {
            "int_type" => nullary_builtin!(IntType),
            "bool_type" => nullary_builtin!(BoolType),
            "string_type" => nullary_builtin!(StringType),
            "never_type" => nullary_builtin!(NeverType),
            "array_type" => self.infer_parameterized_builtin(
                location,
                builtin_name,
                args,
                |_| [],
                |_, element_type_type| element_type_type,
                |element_type, []| Builtin::ArrayType {
                    element_type: Box::new(element_type),
                },
            ),

            "int_negate" => fixed_builtin!(
                IntNegate,
                [Expr::int_type()] => Expr::int_type(),
                { value }
            ),
            "int_bitnot" => fixed_builtin!(
                IntBitNot,
                [Expr::int_type()] => Expr::int_type(),
                { value }
            ),

            "int_add" => int_to_int_binary_builtin!(IntAdd),
            "int_sub" => int_to_int_binary_builtin!(IntSub),
            "int_mul" => int_to_int_binary_builtin!(IntMul),
            "int_bitand" => int_to_int_binary_builtin!(IntBitAnd),
            "int_bitor" => int_to_int_binary_builtin!(IntBitOr),
            "int_bitxor" => int_to_int_binary_builtin!(IntBitXor),
            "int_bitshiftleft" => int_to_int_binary_builtin!(IntBitShiftLeft),
            "int_bitshiftright" => int_to_int_binary_builtin!(IntBitShiftRight),

            "int_eq" => int_to_bool_binary_builtin!(IntEq),
            "int_ne" => int_to_bool_binary_builtin!(IntNe),
            "int_lt" => int_to_bool_binary_builtin!(IntLt),
            "int_le" => int_to_bool_binary_builtin!(IntLe),
            "int_gt" => int_to_bool_binary_builtin!(IntGt),
            "int_ge" => int_to_bool_binary_builtin!(IntGe),

            "string_concat" => fixed_builtin!(
                StringConcat,
                [Expr::string_type(), Expr::string_type()] => Expr::string_type(),
                { lhs, rhs }
            ),
            "string_eq" => same_type_bool_binary_builtin!(StringEq, Expr::string_type()),
            "string_ne" => same_type_bool_binary_builtin!(StringNe, Expr::string_type()),
            "bool_eq" => same_type_bool_binary_builtin!(BoolEq, Expr::bool_type()),
            "bool_ne" => same_type_bool_binary_builtin!(BoolNe, Expr::bool_type()),

            "array_create_unsafe_uninitialized" => parameterized_builtin!(
                ArrayCreateUnsafeUninitialized,
                element_type,
                [Expr::int_type()] => Expr::array_type(element_type.clone()),
                { length }
            ),
            "array_length" => parameterized_builtin!(
                ArrayLength,
                element_type,
                [Expr::array_type(element_type.clone())] => Expr::int_type(),
                { array }
            ),
            "array_get" => parameterized_builtin!(
                ArrayGet,
                element_type,
                [Expr::array_type(element_type.clone()), Expr::int_type()] => element_type.clone(),
                { array, index }
            ),
            "array_set" => parameterized_builtin!(
                ArraySet,
                element_type,
                [
                    Expr::array_type(element_type.clone()),
                    Expr::int_type(),
                    element_type.clone(),
                ] => Expr::unit(),
                { array, index, value }
            ),

            _ => {
                self.report_invalid_builtin(location, builtin_name);
                TypeInferResult::error()
            }
        }
    }

    fn infer_fixed_builtin<'e, const ARG_COUNT: usize>(
        &mut self,
        location: &Location,
        builtin_name: &str,
        args: VecDeque<ArgumentInfo<'e>>,
        create_expected_arg_types: impl FnOnce() -> [Expr<TypeCheckExprContext>; ARG_COUNT],
        create_inferred_type: impl FnOnce() -> Expr<TypeCheckExprContext>,
        create_builtin: impl FnOnce(
            [Expr<TypeCheckExprContext>; ARG_COUNT],
        ) -> Builtin<TypeCheckExprContext>,
    ) -> TypeInferResult<'e> {
        if args.len() != ARG_COUNT {
            self.report_builtin_arity_error(location, builtin_name, ARG_COUNT, args.len());
            return TypeInferResult::error();
        }

        let checked_args = args
            .iter()
            .zip(create_expected_arg_types())
            .map(|(arg, expected_type)| self.check(arg.arg, &expected_type))
            .collect::<Vec<_>>();
        let checked_args: [Expr<TypeCheckExprContext>; ARG_COUNT] =
            checked_args.try_into().unwrap_or_else(|_| unreachable!());

        TypeInferResult::Complete(InferredType {
            checked_expr: Expr::Builtin(create_builtin(checked_args)),
            inferred_type: create_inferred_type(),
        })
    }

    fn infer_parameterized_builtin<'e, const REST_ARG_COUNT: usize>(
        &mut self,
        location: &'e Location,
        builtin_name: &str,
        mut args: VecDeque<ArgumentInfo<'e>>,
        create_rest_arg_types: impl FnOnce(
            &Expr<TypeCheckExprContext>,
        ) -> [Expr<TypeCheckExprContext>; REST_ARG_COUNT],
        create_result_type: impl FnOnce(
            &Expr<TypeCheckExprContext>,
            Expr<TypeCheckExprContext>,
        ) -> Expr<TypeCheckExprContext>,
        create_builtin: impl FnOnce(
            Expr<TypeCheckExprContext>,
            [Expr<TypeCheckExprContext>; REST_ARG_COUNT],
        ) -> Builtin<TypeCheckExprContext>,
    ) -> TypeInferResult<'e> {
        let expected_arg_count = REST_ARG_COUNT + 1;
        let total_arg_count = args.len();

        let Some(element_type_arg) = args.pop_front() else {
            self.report_builtin_arity_error(location, builtin_name, expected_arg_count, 0);
            return TypeInferResult::error();
        };

        let InferredType {
            checked_expr: element_type,
            inferred_type: element_type_type,
        } = self.check_type_with_meta_type(element_type_arg.arg);

        let expected_rest_arg_types = create_rest_arg_types(&element_type);
        if args.len() != REST_ARG_COUNT {
            self.report_builtin_arity_error(
                location,
                builtin_name,
                expected_arg_count,
                total_arg_count,
            );
            return TypeInferResult::error();
        }

        let checked_args = args
            .iter()
            .zip(expected_rest_arg_types)
            .map(|(arg, expected_type)| self.check(arg.arg, &expected_type))
            .collect::<Vec<_>>();
        let checked_args: [Expr<TypeCheckExprContext>; REST_ARG_COUNT] =
            checked_args.try_into().unwrap_or_else(|_| unreachable!());

        TypeInferResult::Complete(InferredType {
            checked_expr: Expr::Builtin(create_builtin(element_type.clone(), checked_args)),
            inferred_type: create_result_type(&element_type, element_type_type),
        })
    }

    fn infer_variable<'e>(
        &mut self,
        _var_location: &'e Location,
        v: Variable<TypeCheckExprContext>,
        args: VecDeque<ArgumentInfo<'e>>,
    ) -> TypeInferResult<'e> {
        let t = v.var_type().clone();
        let expr = Expr::Variable(v);

        let inferred = InferredType {
            checked_expr: expr,
            inferred_type: t,
        };

        let res = TypeInferResult::Complete(inferred);

        self.infer_function_object_call(res, args)
    }

    fn infer_variable_store<'e>(
        &mut self,
        location: &'e Location,
        v: Variable<TypeCheckExprContext>,
        value: AssignedValue<'e>,
    ) -> TypeInferResult<'e> {
        let t = v.var_type().clone();

        let value = self.check(&value.value, &t);

        if !v.is_mutable() {
            self.context
                .reporter()
                .report_error(CompileError::can_not_mutate(location.clone()))
        }

        let expr = Expr::VariableStore(v, Box::new(value));
        TypeInferResult::Complete(InferredType {
            checked_expr: expr,
            inferred_type: Expr::unit(),
        })
    }

    fn infer_variable_tuple_element<'e>(
        &mut self,
        _location: &Location,
        vte: VariableTupleElement<TypeCheckExprContext>,
        args: VecDeque<ArgumentInfo<'e>>,
    ) -> TypeInferResult<'e> {
        let t = vte.binding_type;
        let expr = Expr::TupleElement(Box::new(Expr::Variable(vte.variable)), vte.index);
        let res = TypeInferResult::Complete(InferredType {
            checked_expr: expr,
            inferred_type: t,
        });

        self.infer_function_object_call(res, args)
    }

    fn infer_function_object_call<'e>(
        &mut self,
        expr_result: TypeInferResult<'e>,
        args: VecDeque<ArgumentInfo<'e>>,
    ) -> TypeInferResult<'e> {
        let args = args
            .iter()
            .map(|arg| (self.infer(arg.arg), arg))
            .collect::<Vec<_>>();

        self.infer_function_object_call_inferred(expr_result, args.into_iter())
    }

    fn infer_function_object_call_inferred<'b, 'e>(
        &mut self,
        mut expr_result: TypeInferResult<'e>,
        args: impl Iterator<Item = (TypeInferResult<'e>, &'b ArgumentInfo<'e>)>,
    ) -> TypeInferResult<'e>
    where
        'e: 'b,
    {
        'args: for (arg_result, arg) in args {
            let t = 'not_func_type: {
                match expr_result.partially_inferred_type() {
                    PartiallyInferredType::Full(t) => match t.as_ref() {
                        func_type @ Expr::FunctionType { a, r } => {
                            let func_type = func_type.clone();
                            let r = (**r).clone();

                            let checked_arg = self.check_inferred_type(
                                &arg.arg.location,
                                arg_result,
                                ExpectedType::Exact(&a.var_type),
                            );
                            let f = self.check_inferred_type(
                                &arg.arg.location,
                                expr_result,
                                ExpectedType::Exact(&func_type),
                            );

                            expr_result = TypeInferResult::Complete(InferredType {
                                checked_expr: Expr::FunctionObjectCall {
                                    function: Box::new(f.checked_expr),
                                    argument: Box::new(checked_arg.checked_expr),
                                },
                                inferred_type: r,
                            });
                            continue 'args;
                        }

                        _ => break 'not_func_type PartiallyInferredType::Full(t),
                    },

                    PartiallyInferredType::Closure => todo!(),

                    t => break 'not_func_type t,
                }
            };

            self.context
                .reporter()
                .report_error(CompileError::function_type_required(
                    arg.call_location.clone(),
                    format!("{:?}", t),
                ));

            return TypeInferResult::error();
        }

        expr_result
    }

    fn process_call<'e>(
        &mut self,
        mut func_expr: &'e WithLocation<ast::Expr>,
        assigned_value: Option<AssignedValue<'e>>,
    ) -> CallInfo<'e> {
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
                        list_type: list_type.clone(),
                    });
                    func_expr = func;
                }

                ast::Expr::Builtin(builtin_name) => {
                    if assigned_value.is_some() {
                        self.report_invalid_assignment_target(&func_expr.location);
                        return CallInfo {
                            location: call_location,
                            callee: CalleeInfo::Error,
                            arguments,
                        };
                    }

                    return CallInfo {
                        location: call_location,
                        callee: CalleeInfo::Builtin(builtin_name),
                        arguments,
                    };
                }

                ast::Expr::Identifier(identifier) => {
                    let callee =
                        self.process_lookup(&func_expr.location, identifier, assigned_value);

                    if !arguments.is_empty() && matches!(callee, CalleeInfo::VariableStore(..)) {
                        self.report_invalid_assignment_target(call_location);
                        return CallInfo {
                            location: call_location,
                            callee: CalleeInfo::Error,
                            arguments,
                        };
                    }

                    return CallInfo {
                        location: call_location,
                        callee,
                        arguments,
                    };
                }

                ast::Expr::Dot { o, member } => {
                    return self.process_method_call(o, &member.value, arguments, assigned_value);
                }

                ast::Expr::Index { obj, index } => {
                    if let Some(assigned_value) = assigned_value {
                        if !arguments.is_empty() {
                            self.report_invalid_assignment_target(call_location);
                            return CallInfo {
                                location: call_location,
                                callee: CalleeInfo::Error,
                                arguments,
                            };
                        }

                        return CallInfo {
                            location: call_location,
                            callee: CalleeInfo::IndexUpdate(obj, index, assigned_value),
                            arguments,
                        };
                    } else {
                        return CallInfo {
                            location: call_location,
                            callee: CalleeInfo::Index(obj, index),
                            arguments,
                        };
                    }
                }

                ast::Expr::Type => {
                    if assigned_value.is_some() {
                        self.report_invalid_assignment_target(&func_expr.location);
                        return CallInfo {
                            location: call_location,
                            callee: CalleeInfo::Error,
                            arguments,
                        };
                    }

                    return CallInfo {
                        location: call_location,
                        callee: CalleeInfo::TypeN,
                        arguments,
                    };
                }

                _ => {
                    if assigned_value.is_some() {
                        self.report_invalid_assignment_target(call_location);
                        return CallInfo {
                            location: call_location,
                            callee: CalleeInfo::Error,
                            arguments,
                        };
                    }

                    return CallInfo {
                        location: call_location,
                        callee: CalleeInfo::Expr(func_expr),
                        arguments,
                    };
                }
            }
        }
    }

    fn process_method_call<'e>(
        &mut self,
        o: &'e WithLocation<ast::Expr>,
        member: &Identifier,
        mut arguments: VecDeque<ArgumentInfo<'e>>,
        assigned_value: Option<AssignedValue<'e>>,
    ) -> CallInfo<'e> {
        let mut overload_groups: Vec<Vec<Overloadable>> = Vec::new();

        let mut instance = self.process_call(o, None);
        let call_location = instance.location;

        let adjusted_member_name = if assigned_value.is_some() {
            Identifier::Update(Box::new(member.clone()))
        } else {
            member.clone()
        };

        // Find enum variants
        if let CalleeInfo::Overloadable(instance_overloads) = &mut instance.callee {
            if let Some(e) = instance_overloads
                .iter()
                .flatten()
                .find_map(|overload| match overload {
                    Overloadable::Base(scope::Overloadable::Enum(e)) => Some(e),
                    _ => None,
                })
            {
                overload_groups.push(
                    e.clone()
                        .variants()
                        .iter()
                        .cloned()
                        .filter(|v| v.metadata().name == *member)
                        .map(|v| Overloadable::Base(scope::Overloadable::EnumVariant(v.clone())))
                        .collect(),
                );
            }
        }

        let mut instance = self.infer_call(instance).infer_fully();

        {
            let mut norm = NormalizerScanner::new(
                self.context.normalize_fuel(),
                ExprNormalizer { model: self.model },
            );
            norm.normalize(&mut instance.inferred_type);
        }

        let instance_type_as_method_owner = match &instance.inferred_type {
            Expr::TraitType(tt) => Some(MethodOwner::Trait(tt.trait_.clone())),
            Expr::InstanceType(it) => Some(MethodOwner::Instance(it.instance.clone())),
            _ => None,
        };

        match &instance.inferred_type {
            Expr::RecordType(record_type) => {
                let r = &record_type.record;
                let args = &record_type.arguments;
                overload_groups.extend(
                    r.clone()
                        .fields()
                        .iter()
                        .find(|field| field.metadata().name == *member)
                        .map(|field| {
                            let mut field_type = default_to_type_check_shifter()
                                .shift((*field.clone().field_type()).clone());

                            let mut subst = SubstScanner::new();
                            let record_sig = (*r.clone().signature())
                                .clone()
                                .shift(&mut default_to_type_check_shifter());

                            subst.add_function_parameter_substitutions(
                                ExpressionOwner::Record(r.clone()),
                                &record_sig,
                                args,
                            );
                            subst.scan(&mut field_type);

                            let overload = if assigned_value.is_some() {
                                Overloadable::RecordFieldStore {
                                    record_type: instance.inferred_type.clone(),
                                    field: field.clone(),
                                    field_type,
                                    record_value: instance.checked_expr.clone(),
                                }
                            } else {
                                Overloadable::RecordField {
                                    record_type: instance.inferred_type.clone(),
                                    field: field.clone(),
                                    field_type,
                                    record_value: instance.checked_expr.clone(),
                                }
                            };

                            vec![overload]
                        }),
                );
            }
            Expr::TraitType(trait_type) => {
                let methods = trait_type.trait_.clone().methods();

                let method_overloads = methods
                    .iter()
                    .filter(|entry| {
                        entry.method.metadata().name == adjusted_member_name
                            && self.access.allows_access(
                                &Declaration::Method(entry.method.clone()),
                                instance_type_as_method_owner.as_ref(),
                                entry.access,
                            )
                    })
                    .map(|entry| Overloadable::InstanceMethod {
                        method: entry.method.clone(),
                        trait_type: trait_type.clone(),
                        obj: instance.checked_expr.clone(),
                    })
                    .collect::<Vec<_>>();

                if !method_overloads.is_empty() {
                    overload_groups.push(method_overloads);
                }
            }
            _ => {}
        }

        // Extension methods
        if let Lookup::Overloadable(extension_overloads) = self.scope.lookup(
            &Identifier::Extension(Box::new(adjusted_member_name.clone())),
            self.access,
        ) {
            overload_groups.extend(
                extension_overloads
                    .item_groups
                    .into_iter()
                    .map(|overload_group| {
                        overload_group
                            .into_iter()
                            .filter_map(|overload| match overload {
                                scope::Overloadable::Function(f) => {
                                    Some(Overloadable::ExtensionMethod(
                                        f,
                                        ArgumentInfo {
                                            call_location,
                                            arg: o,
                                            list_type: FunctionParameterListType::NormalList,
                                        },
                                        instance.clone(),
                                    ))
                                }
                                _ => None,
                            })
                            .collect::<Vec<_>>()
                    })
                    .filter(|overload_group| !overload_group.is_empty()),
            );
        }

        if let Some(assigned_value) = assigned_value {
            arguments.push_back(ArgumentInfo {
                call_location: assigned_value.assign_location,
                arg: assigned_value.value,
                list_type: FunctionParameterListType::NormalList,
            });
        }

        return CallInfo {
            location: call_location,
            callee: CalleeInfo::Overloadable(overload_groups),
            arguments,
        };
    }

    fn process_binary_operator<'b>(
        &mut self,
        location: &'b Location,
        left: &'b WithLocation<ast::Expr>,
        op: ast::BinaryOperatorIdentifier,
        op_location: &'b Location,
        right: &'b WithLocation<ast::Expr>,
    ) -> CallInfo<'b> {
        CallInfo {
            location,
            callee: self.process_lookup(op_location, &Identifier::BinaryOp(op), None),
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

    fn process_unary_operator<'b>(
        &mut self,
        location: &'b Location,
        op: ast::UnaryOperatorIdentifier,
        op_location: &'b Location,
        arg: &'b WithLocation<ast::Expr>,
    ) -> CallInfo<'b> {
        CallInfo {
            location,
            callee: self.process_lookup(op_location, &Identifier::UnaryOp(op), None),
            arguments: VecDeque::from([ArgumentInfo {
                call_location: location,
                arg,
                list_type: FunctionParameterListType::NormalList,
            }]),
        }
    }

    fn process_lookup<'b>(
        &mut self,
        location: &Location,
        identifier: &Identifier,
        assigned_value: Option<AssignedValue<'b>>,
    ) -> CalleeInfo<'b> {
        let lookup = if assigned_value.is_some() {
            self.scope.lookup_assign(identifier, self.access)
        } else {
            self.scope.lookup(identifier, self.access)
        };

        match lookup {
            Lookup::Empty => {
                self.context
                    .reporter()
                    .report_error(CompileError::unknown_identifier(
                        location.clone(),
                        identifier.to_string(),
                    ));

                CalleeInfo::Error
            }

            Lookup::Variable(v) => {
                if let Some(assigned_value) = assigned_value {
                    CalleeInfo::VariableStore(v, assigned_value)
                } else {
                    CalleeInfo::Variable(v)
                }
            }
            Lookup::VariableTupleElement(vte) => {
                if assigned_value.is_some() {
                    self.report_invalid_assignment_target(location);
                    return CalleeInfo::Error;
                }

                CalleeInfo::VariableTupleElement(vte)
            }

            Lookup::Overloadable(overloadable) => CalleeInfo::Overloadable(
                overloadable
                    .item_groups
                    .into_iter()
                    .map(|group| {
                        group
                            .into_iter()
                            .map(Overloadable::from)
                            .collect::<Vec<_>>()
                    })
                    .collect::<Vec<_>>(),
            ),
        }
    }

    fn resolve_inferred_type<'f>(
        &mut self,
        infer: TypeInferResult,
        expected_type: ExpectedType<'f>,
    ) -> InferredType {
        match infer {
            TypeInferResult::Complete(inferred_type) => inferred_type,
            TypeInferResult::Closure {
                location,
                function_literal,
            } => {
                let ExpectedType::Exact(expected_type) = expected_type else {
                    self.context
                        .reporter()
                        .report_error(CompileError::tuple_type_required(
                            location.clone(),
                            format!("{:?}", expected_type),
                        ));
                    return InferredType::error();
                };
                let mut expected_type = expected_type.clone();

                let mut norm = NormalizerScanner::new(
                    self.context.normalize_fuel(),
                    ExprNormalizer {
                        model: &mut self.model,
                    },
                );
                norm.normalize(&mut expected_type);

                let Expr::FunctionType {
                    a: func_type_arg,
                    r: mut func_type_result,
                } = expected_type
                else {
                    self.context
                        .reporter()
                        .report_error(CompileError::tuple_type_required(
                            location.clone(),
                            format!("{:?}", expected_type),
                        ));
                    return InferredType::error();
                };

                let param = ClosureParameterVariable {
                    id: UniqueIdentifier::new(),
                    name: function_literal.parameter_name.clone(),
                    var_type: func_type_arg.var_type.clone(),
                    is_mutable: false,
                    erasure_mode: ErasureMode::Concrete,
                    is_witness: false,
                };

                let param_var = Variable::ClosureParameter(Box::new(param.clone()));

                let mut subst = SubstScanner::new();
                subst.add_substitution(
                    Variable::ClosureParameter(func_type_arg),
                    Cow::Owned(Expr::Variable(param_var.clone())),
                );
                subst.scan(&mut func_type_result);

                let mut checker = with_nested_scope!(self);

                checker.scope.add_variable(param_var);

                let body = checker.check(&function_literal.body, &func_type_result);

                let function_type = Expr::FunctionType {
                    a: Box::new(param.clone()),
                    r: func_type_result.clone(),
                };

                InferredType {
                    checked_expr: Expr::Closure {
                        v: Box::new(param),
                        return_type: func_type_result,
                        body: Box::new(body),
                    },
                    inferred_type: function_type,
                }
            }

            TypeInferResult::Tuple { location, elements } => {
                let element_expected_types: Vec<ExpectedType<'_>> = match expected_type {
                    ExpectedType::AnyMetaType
                    | ExpectedType::Exact(Expr::Type(_) | Expr::BigType(_)) => {
                        elements.iter().map(|_| expected_type).collect()
                    }
                    ExpectedType::Exact(Expr::Tuple { items }) => {
                        items.iter().map(ExpectedType::Exact).collect()
                    }

                    _ => {
                        self.context
                            .reporter()
                            .report_error(CompileError::tuple_type_required(
                                location.clone(),
                                format!("{:?}", expected_type),
                            ));

                        return InferredType {
                            checked_expr: Expr::Error,
                            inferred_type: Expr::Error,
                        };
                    }
                };

                if element_expected_types.len() != elements.len() {
                    self.context
                        .reporter()
                        .report_error(CompileError::tuple_size_mismatch(
                            location.clone(),
                            element_expected_types.len(),
                            elements.len(),
                        ));
                }

                let mut element_exprs = Vec::with_capacity(elements.len());
                let mut element_types = Vec::with_capacity(elements.len());

                for (expr, expected_type) in elements.into_iter().zip(element_expected_types) {
                    let inferred_type = self.resolve_inferred_type(expr, expected_type);
                    element_exprs.push(inferred_type.checked_expr);
                    element_types.push(inferred_type.inferred_type);
                }

                InferredType {
                    checked_expr: Expr::Tuple {
                        items: element_exprs,
                    },
                    inferred_type: Expr::Tuple {
                        items: element_types,
                    },
                }
            }

            TypeInferResult::Finally {
                body_result,
                finally_body,
            } => {
                let body_inferred = self.resolve_inferred_type(*body_result, expected_type);
                InferredType {
                    checked_expr: Expr::Finally {
                        block_body: Box::new(body_inferred.checked_expr),
                        finally_body,
                    },
                    inferred_type: body_inferred.inferred_type,
                }
            }
            TypeInferResult::IfElse {
                condition,
                true_body,
                false_body_location,
                false_body,
            } => {
                let true_body_inferred = self.resolve_inferred_type(*true_body, expected_type);
                let checked_false_body = self
                    .check_inferred_type(
                        false_body_location,
                        *false_body,
                        ExpectedType::Exact(&true_body_inferred.inferred_type),
                    )
                    .checked_expr;

                InferredType {
                    checked_expr: Expr::IfElse {
                        condition,
                        when_true: Box::new(true_body_inferred.checked_expr),
                        when_false: Box::new(checked_false_body),
                    },
                    inferred_type: true_body_inferred.inferred_type,
                }
            }
            TypeInferResult::Match { expression, arms } => {
                let mut cases = Vec::with_capacity(arms.len());
                let mut arms_iter = arms.into_iter();
                let selected_type;

                if let Some(arm) = arms_iter.next() {
                    let first_inferred = self.resolve_inferred_type(arm.body, expected_type);
                    selected_type = first_inferred.inferred_type;
                    cases.push(MatchCase {
                        pattern: arm.pattern,
                        body: first_inferred.checked_expr,
                    });

                    for arm in arms_iter {
                        let inferred = self.check_inferred_type(
                            arm.location,
                            arm.body,
                            ExpectedType::Exact(&selected_type),
                        );
                        cases.push(MatchCase {
                            pattern: arm.pattern,
                            body: inferred.checked_expr,
                        });
                    }
                } else {
                    selected_type = Expr::never_type();
                }

                InferredType {
                    checked_expr: Expr::Match {
                        value: expression,
                        cases,
                    },
                    inferred_type: selected_type,
                }
            }
            TypeInferResult::Sequence {
                init_exprs: mut exprs,
                last_result,
            } => {
                let last_inferred = self.resolve_inferred_type(*last_result, expected_type);
                exprs.push(last_inferred.checked_expr);
                InferredType {
                    checked_expr: Expr::Sequence(exprs),
                    inferred_type: last_inferred.inferred_type,
                }
            }
        }
    }

    fn check_inferred_type(
        &mut self,
        location: &Location,
        infer: TypeInferResult,
        expected_type: ExpectedType<'_>,
    ) -> InferredType {
        let mut inferred = self.resolve_inferred_type(infer, expected_type);

        let mut transformations = Vec::new();
        if !self.type_matches_expected(&mut transformations, &inferred.inferred_type, expected_type)
        {
            self.context
                .reporter()
                .report_error(CompileError::type_mismatch(
                    location.clone(),
                    format!("{:?}", expected_type),
                    format!("{:?}", inferred.inferred_type),
                ));
        }

        for transformation in transformations {
            transformation.transform(&mut inferred)
        }

        inferred
    }

    fn check_condition_expr(
        &mut self,
        expr: &WithLocation<ast::Expr>,
    ) -> Expr<TypeCheckExprContext> {
        let condition = self.infer(expr);
        self.check_condition(condition)
    }

    fn check_condition(&mut self, condition: TypeInferResult) -> Expr<TypeCheckExprContext> {
        let mut cond_expr = self
            .resolve_inferred_type(condition, ExpectedType::Exact(&Expr::bool_type()))
            .checked_expr;

        if !matches!(
            cond_expr,
            Expr::BoolLiteral(_) | Expr::And(..) | Expr::Or(..) | Expr::Not(_) | Expr::Is { .. }
        ) {
            let (when_true_witness, when_false_witness) = self.create_if_cond_vars(&cond_expr);

            if when_true_witness.is_some() || when_false_witness.is_some() {
                cond_expr = Expr::Condition {
                    value: Box::new(cond_expr),
                    when_true_witness,
                    when_false_witness,
                };
            }
        }

        cond_expr
    }

    fn create_if_cond_vars(
        &mut self,
        cond_expr: &Expr<TypeCheckExprContext>,
    ) -> (
        Option<Box<LocalVariable<TypeCheckExprContext>>>,
        Option<Box<LocalVariable<TypeCheckExprContext>>>,
    ) {
        let is_pure_cond = !PurityScanner::contains_impure_function_call(cond_expr);

        let when_true_var = is_pure_cond.then(|| self.create_if_branch_var(cond_expr, true));
        let when_false_var = is_pure_cond.then(|| self.create_if_branch_var(cond_expr, false));

        (when_true_var, when_false_var)
    }

    fn create_if_branch_var(
        &mut self,
        cond_expr: &Expr<TypeCheckExprContext>,
        equal_to_value: bool,
    ) -> Box<LocalVariable<TypeCheckExprContext>> {
        Box::new(LocalVariable {
            id: UniqueIdentifier::new(),
            name: None,
            var_type: Expr::Builtin(Builtin::EqualToType {
                lhs: Box::new(cond_expr.clone()),
                rhs: Box::new(Expr::BoolLiteral(equal_to_value)),
            }),
            erasure_mode: ErasureMode::Erased,
            is_witness: false,
            is_mutable: false,
        })
    }

    fn check_pattern(
        &mut self,
        pattern: &WithLocation<ast::Pattern>,
        mut pattern_type: Expr<TypeCheckExprContext>,
    ) -> Pattern<TypeCheckExprContext> {
        match &pattern.value {
            ast::Pattern::Discard => Pattern::Discard {
                t: Box::new(pattern_type),
            },

            ast::Pattern::Tuple { elements } => {
                let mut normalizer = NormalizerScanner::new(
                    self.context.normalize_fuel(),
                    ExprNormalizer { model: self.model },
                );
                normalizer.normalize(&mut pattern_type);

                let Expr::Tuple {
                    items: element_types,
                } = pattern_type
                else {
                    todo!("Tuple pattern type");
                };

                if element_types.len() != elements.len() {
                    todo!("Tuple pattern size mismatch");
                }

                let element_patterns = elements
                    .iter()
                    .zip(element_types)
                    .map(|(element, element_type)| self.check_pattern(element, element_type))
                    .collect();

                Pattern::Tuple(element_patterns)
            }

            ast::Pattern::Binding {
                pattern,
                is_mutable,
                name,
            } => {
                let v = LocalVariable {
                    id: UniqueIdentifier::new(),
                    name: Some(name.value.clone()),
                    var_type: pattern_type.clone(),
                    erasure_mode: ErasureMode::Concrete,
                    is_witness: false,
                    is_mutable: *is_mutable,
                };

                let inner = self.check_pattern(pattern, pattern_type);
                Pattern::Binding(v, Box::new(inner))
            }
            ast::Pattern::Constructor { path, args } => {
                let path_type = self.resolve_pattern_path(path);
                match path_type {
                    Overloadable::Base(scope::Overloadable::EnumVariant(v)) => {
                        let mut normalizer = NormalizerScanner::new(
                            self.context.normalize_fuel(),
                            ExprNormalizer { model: self.model },
                        );
                        normalizer.normalize(&mut pattern_type);

                        let enum_type = match &pattern_type {
                            Expr::EnumType(enum_type) => enum_type,
                            Expr::Error => {
                                return Pattern::Error;
                            }
                            _ => {
                                self.context
                                    .reporter()
                                    .report_error(CompileError::type_mismatch(
                                        pattern.location.clone(),
                                        format!("{:?}", pattern_type),
                                        "an enum type".to_string(),
                                    ));
                                return Pattern::Error;
                            }
                        };

                        let enum_type = enum_type.clone();

                        let sig = v.clone().signature().as_ref().clone();
                        let mut sig = sig.shift(&mut default_to_type_check_shifter());
                        substitute_holes_for_args(
                            &pattern.location,
                            &ExpressionOwner::EnumVariant(v.clone()),
                            &mut sig,
                        );

                        let mut transformations = Vec::new();
                        if !self.type_matches_expected(
                            &mut transformations,
                            &pattern_type,
                            ExpectedType::Exact(&sig.return_type),
                        ) {
                            todo!()
                        }

                        if !transformations.is_empty() {
                            todo!()
                        }

                        let arg_patterns = self.check_pattern_args(args, &sig.parameters);

                        Pattern::EnumVariant {
                            enum_type,
                            variant: v,
                            args: arg_patterns,
                            fields: vec![],
                        }
                    }

                    _ => todo!(),
                }
            }
            ast::Pattern::Record { .. } => {
                todo!()
            }

            ast::Pattern::String(s) => {
                let mut transformations = Vec::new();
                if !self.type_matches_expected(
                    &mut transformations,
                    &pattern_type,
                    ExpectedType::Exact(&Expr::string_type()),
                ) {
                    todo!()
                }

                if !transformations.is_empty() {
                    todo!()
                }

                match &s.parts[..] {
                    [] => Pattern::String("".to_owned()),
                    [StringFragment::Text(s)] => Pattern::String(s.to_owned()),
                    _ => todo!(),
                }
            }
            ast::Pattern::Int(i) => {
                let mut transformations = Vec::new();
                if !self.type_matches_expected(
                    &mut transformations,
                    &pattern_type,
                    ExpectedType::Exact(&Expr::int_type()),
                ) {
                    todo!()
                }

                if !transformations.is_empty() {
                    todo!()
                }

                Pattern::Int(i.clone())
            }
            ast::Pattern::Bool(b) => {
                let mut transformations = Vec::new();
                if !self.type_matches_expected(
                    &mut transformations,
                    &pattern_type,
                    ExpectedType::Exact(&Expr::bool_type()),
                ) {
                    todo!()
                }

                if !transformations.is_empty() {
                    todo!()
                }

                Pattern::Bool(*b)
            }
        }
    }

    fn resolve_pattern_path<'e>(
        &mut self,
        path: &'e WithLocation<ast::PatternPath>,
    ) -> Overloadable<'e> {
        fn process_lookup<'e>(res: Lookup<TypeCheckExprContext>) -> Overloadable<'e> {
            match res {
                Lookup::Overloadable(overloadable) => {
                    for mut group in overloadable.item_groups {
                        if group.is_empty() {
                            continue;
                        }

                        if group.len() == 1
                            && let Some(overload) = group.pop()
                        {
                            return Overloadable::Base(overload);
                        }

                        todo!()
                    }

                    todo!()
                }
                _ => todo!(),
            }
        }

        match &path.value {
            ast::PatternPath::Member { member, base } => match &base.value {
                ast::PatternPath::Base { name: base_name } => {
                    match process_lookup(self.scope.lookup(&base_name.value, self.access)) {
                        Overloadable::Base(scope::Overloadable::Enum(e)) => {
                            let Some(variant) = e
                                .variants()
                                .iter()
                                .find(|v| v.metadata().name == member.value)
                                .cloned()
                            else {
                                todo!()
                            };

                            Overloadable::Base(scope::Overloadable::EnumVariant(variant.clone()))
                        }
                        _ => todo!(),
                    }
                }

                _ => todo!(),
            },

            ast::PatternPath::Base { name } => {
                process_lookup(self.scope.lookup(&name.value, self.access))
            }
        }
    }

    fn check_pattern_args(
        &mut self,
        mut args: &[ast::PatternArgument],
        mut params: &[SignatureParameter<TypeCheckExprContext>],
    ) -> Vec<Pattern<TypeCheckExprContext>> {
        let mut patterns = Vec::with_capacity(params.len());

        loop {
            let Some((param, tail_params)) = params.split_first() else {
                if args.is_empty() {
                    break;
                } else {
                    todo!()
                }
            };

            if let Some((arg, tail_args)) = args.split_first()
                && param.list_type == arg.function_parameter_list_type
            {
                if param.list_type == arg.function_parameter_list_type {
                    let pattern = self.check_pattern(&arg.arg, param.param_type.clone());
                    patterns.push(pattern);
                    args = tail_args;
                }
            } else if param.list_type == FunctionParameterListType::NormalList {
                todo!()
            } else {
                patterns.push(Pattern::Discard {
                    t: Box::new(param.param_type.clone()),
                });
            }

            params = tail_params;
        }

        patterns
    }

    fn report_invalid_builtin(&self, location: &Location, name: impl AsRef<str>) {
        self.context
            .reporter()
            .report_error(CompileError::invalid_builtin(Some(location.clone()), name));
    }

    fn report_invalid_assignment_target(&self, location: &Location) {
        self.context
            .reporter()
            .report_error(CompileError::invalid_assignment_target(location.clone()));
    }

    fn report_builtin_arity_error(
        &self,
        location: &Location,
        builtin_name: &str,
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
                builtin_name
            ),
        );
    }

    fn is_type(&self, mut expr: Expr<TypeCheckExprContext>) -> bool {
        let mut model = self.model.clone();
        let mut norm = NormalizerScanner::new(
            self.context.normalize_fuel(),
            ExprNormalizer {
                model: &mut model,
            },
        );
        norm.normalize(&mut expr);

        match expr {
            Expr::Type(_) => true,
            Expr::BigType(_) => true,
            Expr::Tuple { items } => items.into_iter().all(|item| self.is_type(item)),
            _ => false,
        }
    }

    fn type_matches_expected(
        &mut self,
        transformations: &mut Vec<TypeCheckValueTransformation>,
        actual_type: &Expr<TypeCheckExprContext>,
        expected_type: ExpectedType<'_>,
    ) -> bool {
        match expected_type {
            ExpectedType::AnyMetaType => self.is_type(actual_type.clone()),

            ExpectedType::Exact(expected_type) => {
                let mut expected_type = expected_type.clone();
                let mut actual_type = actual_type.clone();

                loop {
                    {
                        let mut norm = NormalizerScanner::new(
                            self.context.normalize_fuel(),
                            ExprNormalizer {
                                model: &mut self.model,
                            },
                        );
                        norm.normalize(&mut expected_type);
                    }

                    {
                        let mut norm = NormalizerScanner::new(
                            self.context.normalize_fuel(),
                            ExprNormalizer {
                                model: &mut self.model,
                            },
                        );
                        norm.normalize(&mut actual_type);
                    }

                    match actual_type {
                        Expr::Type(ref n) => match &expected_type {
                            Expr::BigType(_) => return true,
                            Expr::Type(n2) => match (&**n, n2.as_ref()) {
                                (Expr::IntLiteral(n), Expr::IntLiteral(n2)) => {
                                    return n >= &BigInt::ZERO && n2 >= &BigInt::ZERO && n <= n2;
                                }
                                _ => {}
                            },
                            _ => {}
                        },

                        Expr::Builtin(Builtin::NeverType) => {
                            return true;
                        }

                        Expr::BoxedType(actual_unboxed) => {
                            if let Expr::BoxedType(expected_unboxed) = expected_type {
                                expected_type = *expected_unboxed;
                            } else {
                                transformations.push(TypeCheckValueTransformation::Unbox(
                                    (*actual_unboxed).clone(),
                                ));
                            }

                            actual_type = *actual_unboxed;
                            continue;
                        }

                        _ => {}
                    }

                    match expected_type {
                        Expr::BoxedType(expected_unboxed) => {
                            transformations.push(TypeCheckValueTransformation::Box(
                                (*expected_unboxed).clone(),
                            ));
                            expected_type = *expected_unboxed;
                            continue;
                        }

                        _ => {}
                    }

                    break;
                }

                self.unify(expected_type, actual_type)
            }
        }
    }

    fn partially_inferred_type_matches_expected(
        &mut self,
        allow_transform: bool,
        actual_type: &PartiallyInferredType<'_>,
        expected_type: ExpectedType<'_>,
    ) -> bool {
        match actual_type {
            PartiallyInferredType::Full(actual_type) => {
                let mut transformations = Vec::new();
                self.type_matches_expected(&mut transformations, actual_type, expected_type)
                    && (allow_transform || transformations.is_empty())
            }
            PartiallyInferredType::Closure => {
                let ExpectedType::Exact(expected_type) = expected_type else {
                    return false;
                };
                let mut expected_type = expected_type.clone();

                let mut norm = NormalizerScanner::new(
                    self.context.normalize_fuel(),
                    ExprNormalizer {
                        model: &mut self.model,
                    },
                );
                norm.normalize(&mut expected_type);

                matches!(expected_type, Expr::FunctionType { .. })
            }
            PartiallyInferredType::Tuple(elements) => match expected_type {
                ExpectedType::AnyMetaType
                | ExpectedType::Exact(Expr::Type(_) | Expr::BigType(_)) => {
                    elements.iter().all(|e| {
                        self.partially_inferred_type_matches_expected(false, e, expected_type)
                    })
                }

                ExpectedType::Exact(Expr::Tuple { items }) if items.len() == elements.len() => {
                    elements.iter().zip(items.iter()).all(|(actual, expected)| {
                        self.partially_inferred_type_matches_expected(
                            false,
                            actual,
                            ExpectedType::Exact(expected),
                        )
                    })
                }

                _ => false,
            },
        }
    }

    fn get_exception_trait(&self) -> Arc<dyn Trait> {
        let Some(core_tube) = self
            .scope
            .lookup_tube(&TubeName(vec1!["Argon".to_owned(), "Core".to_owned()]))
        else {
            todo!()
        };

        let Some(exception_module) = core_tube.module(&ModulePath(vec!["Exception".to_owned()]))
        else {
            todo!()
        };

        let group = exception_module
            .export_groups()
            .get(&Identifier::Named("Exception".to_owned()))
            .iter()
            .flat_map(|group| group.iter())
            .filter_map(|entry| match &entry.binding {
                ModuleExportBinding::Trait(t) => Some(t.clone()),
                _ => None,
            })
            .collect::<Vec<_>>();

        let Some(t) = group
            .into_iter()
            .find(|t| t.clone().signature().parameters.is_empty())
        else {
            todo!()
        };

        t
    }
}

impl<'access, 'scope, 'model> Unify for TypeChecker<'access, 'scope, 'model> {
    type EC = TypeCheckExprContext;
    type Norm<'a>
        = ExprNormalizer<'a>
    where
        Self: 'a;

    fn normalize_fuel(&self) -> Fuel {
        self.context.normalize_fuel()
    }

    fn normalizer<'a>(&'a mut self) -> Self::Norm<'a> {
        ExprNormalizer {
            model: &mut self.model,
        }
    }

    fn unify_hole(&mut self, a: <Self::EC as ExprContext>::Hole, b: Expr<Self::EC>) -> bool {
        if let Expr::Hole(b) = &b
            && a == *b
        {
            return false;
        }

        match self.model.hole_values.entry_ref(&a) {
            hash_map::EntryRef::Occupied(oea) => {
                let a_value = oea.get().clone();
                self.unify(a_value, b)
            }
            hash_map::EntryRef::Vacant(vea) => {
                // TODO: Type check the hole
                match b {
                    Expr::Hole(b) => match self.model.hole_values.entry_ref(&b) {
                        hash_map::EntryRef::Occupied(oeb) => {
                            let b_value = oeb.get().clone();
                            self.model.hole_values.insert(a, b_value);
                            true
                        }
                        hash_map::EntryRef::Vacant(veb) => {
                            veb.insert(Expr::Hole(a));
                            true
                        }
                    },
                    _ => {
                        vea.insert(b.clone());
                        true
                    }
                }
            }
        }
    }
}

enum TypeCheckValueTransformation {
    Box(Expr<TypeCheckExprContext>),
    Unbox(Expr<TypeCheckExprContext>),
}

impl TypeCheckValueTransformation {
    fn transform(self, e: &mut InferredType) {
        match self {
            TypeCheckValueTransformation::Box(t) => {
                let checked_expr = mem::replace(&mut e.checked_expr, Expr::Error);
                let inferred_type = mem::replace(&mut e.inferred_type, Expr::Error);

                e.checked_expr = Expr::Box {
                    t: Box::new(t),
                    value: Box::new(checked_expr),
                };
                e.inferred_type = Expr::BoxedType(Box::new(inferred_type));
            }
            TypeCheckValueTransformation::Unbox(t) => {
                let checked_expr = mem::replace(&mut e.checked_expr, Expr::Error);

                e.checked_expr = Expr::Unbox {
                    t: Box::new(t.clone()),
                    value: Box::new(checked_expr),
                };
                e.inferred_type = t;
            }
        }
    }
}

fn build_subst_holes_for_args(
    location: &Location,
    subst: &mut SubstScanner<TypeCheckExprContext>,
    owner: &ExpressionOwner<TypeCheckExprContext>,
    sig: &FunctionSignature<TypeCheckExprContext>,
) -> Vec<Hole> {
    let holes = sig
        .parameters
        .iter()
        .enumerate()
        .map(|(i, param)| {
            let v = param.clone().to_parameter_var(owner.clone(), i);
            let mut t = v.var_type.clone();
            subst.scan(&mut t);
            let hole = Hole::new(location.clone(), t);
            subst.add_substitution(
                Variable::Parameter(Box::new(v)),
                Cow::Owned(Expr::Hole(hole.clone())),
            );
            hole
        })
        .collect::<Vec<_>>();

    holes
}

fn substitute_holes_for_args(
    location: &Location,
    owner: &ExpressionOwner<TypeCheckExprContext>,
    sig: &mut FunctionSignature<TypeCheckExprContext>,
) -> Vec<Hole> {
    let mut subst = SubstScanner::new();
    let holes = build_subst_holes_for_args(location, &mut subst, owner, sig);

    sig.scan_mut(&mut subst);

    holes
}

fn get_condition_vars(
    condition: &Expr<TypeCheckExprContext>,
    when_true_vars: &mut Vec<LocalVariable<TypeCheckExprContext>>,
    when_false_vars: &mut Vec<LocalVariable<TypeCheckExprContext>>,
) {
    match condition {
        Expr::Condition {
            value,
            when_true_witness,
            when_false_witness,
        } => {
            when_true_vars.extend(when_true_witness.iter().map(Box::as_ref).cloned());
            when_false_vars.extend(when_false_witness.iter().map(Box::as_ref).cloned());
            get_condition_vars(value, when_true_vars, when_false_vars);
        }

        Expr::Not(value) => {
            get_condition_vars(value, when_false_vars, when_true_vars);
        }

        Expr::And(a, b) => {
            let mut discarded_false_vars = Vec::new();
            get_condition_vars(a, when_true_vars, &mut discarded_false_vars);
            get_condition_vars(b, when_true_vars, &mut discarded_false_vars);
        }

        Expr::Or(a, b) => {
            let mut discarded_true_vars = Vec::new();
            get_condition_vars(a, &mut discarded_true_vars, when_false_vars);
            get_condition_vars(b, &mut discarded_true_vars, when_false_vars);
        }

        Expr::Is { value: _, pattern } => {
            get_pattern_vars(pattern, when_true_vars);
        }

        _ => {}
    }
}

fn get_pattern_vars(
    pattern: &Pattern<TypeCheckExprContext>,
    vars: &mut Vec<LocalVariable<TypeCheckExprContext>>,
) {
    match pattern {
        Pattern::Error => {}
        Pattern::Discard { .. } => {}
        Pattern::Tuple(items) => {
            for item in items {
                get_pattern_vars(item, vars);
            }
        }
        Pattern::Binding(v, inner) => {
            vars.push(v.clone());
            get_pattern_vars(inner, vars);
        }
        Pattern::EnumVariant { args, fields, .. } => {
            for arg in args {
                get_pattern_vars(arg, vars);
            }
            for field in fields {
                get_pattern_vars(&field.pattern, vars);
            }
        }
        Pattern::String(_) => {}
        Pattern::Int(_) => {}
        Pattern::Bool(_) => {}
    }
}

#[derive(Debug, Clone)]
struct ArgumentInfo<'a> {
    call_location: &'a Location,
    arg: &'a WithLocation<ast::Expr>,
    list_type: FunctionParameterListType,
}

#[derive(Debug, Clone)]
struct AssignedValue<'e> {
    assign_location: &'e Location,
    value: &'e WithLocation<ast::Expr>,
}

enum CalleeInfo<'a> {
    Error,
    Expr(&'a WithLocation<ast::Expr>),
    Builtin(&'a str),
    Variable(Variable<TypeCheckExprContext>),
    VariableStore(Variable<TypeCheckExprContext>, AssignedValue<'a>),
    VariableTupleElement(VariableTupleElement<TypeCheckExprContext>),
    Overloadable(Vec<Vec<Overloadable<'a>>>),
    TypeN,
    Index(&'a WithLocation<ast::Expr>, &'a WithLocation<ast::Expr>),
    IndexUpdate(
        &'a WithLocation<ast::Expr>,
        &'a WithLocation<ast::Expr>,
        AssignedValue<'a>,
    ),
}

struct CallInfo<'a> {
    location: &'a Location,
    callee: CalleeInfo<'a>,
    arguments: VecDeque<ArgumentInfo<'a>>,
}

struct BranchTyper {
    branch_type: Expr<TypeCheckExprContext>,
}

impl BranchTyper {
    fn new() -> Self {
        Self {
            branch_type: Expr::never_type(),
        }
    }

    fn add_branch(
        &mut self,
        _checker: &mut TypeChecker<'_, '_, '_>,
        t: &Expr<TypeCheckExprContext>,
    ) {
        match self.branch_type {
            Expr::Builtin(Builtin::NeverType) => {
                self.branch_type = t.clone();
            }

            _ => {}
        }
    }

    fn into_branch_type(self) -> Expr<TypeCheckExprContext> {
        self.branch_type
    }
}
