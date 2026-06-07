use crate::modifiers::{ERASURE_MODE, IS_WITNESS, ModifierParser};
use alloc::collections::{BTreeMap, VecDeque};
use alloc::{boxed::Box, format, string::ToString, sync::Arc, vec, vec::Vec};
use argon_compiler::access::AccessToken;
use argon_compiler::scanner::PurityScanner;
use argon_compiler::scope::{self, LocalScope, LocalVariableScope, Lookup, Scope, ShiftedScope};
use argon_compiler::signature::SignatureParameter;
use argon_compiler::{
    Context, DefaultExprContext, Function, FunctionImplementation, FunctionSignature, RecordField,
    RecordFieldOwner, SubstFunctionSignature,
};
use argon_expr::{
    BlockLabel, BlockLabelDeclaration, BlockLabelKind, Builtin, ErasureMode, Expr, ExprContext,
    ExprContextShifter, ExprScannerMut, ExpressionOwner, LocalVariable, LoopLabels, Normalizer,
    NormalizerScanner, RecordFieldLiteral, RecordType, SubstScanner, Variable,
    VariableTupleElement,
};
use argon_parser::ast;
use argon_parser::ast::{FunctionLiteral, FunctionParameterListType, Identifier, StringFragment};
use argon_util::{CompileError, MultiSlice, UniqueIdentifier};
use core::cmp::Ordering;
use core::fmt::{Debug, Formatter};
use core::hash::{Hash, Hasher};
use core::{mem, ptr};
use hashbrown::{DefaultHashBuilder, HashMap, HashSet, hash_map};
use mitsein::vec1;
use mitsein::vec1::Vec1;
use num_bigint::BigInt;
use parse18_runtime::{Location, WithLocation};

pub fn type_check_type_expr(
    context: Context,
    options: TypeCheckOptions<'_>,
    e: &WithLocation<ast::Expr>,
) -> Expr<DefaultExprContext> {
    let TypeCheckOptions { access, scope } = options;
    let mut shifted_scope = ShiftedScope::new(scope, DefaultToTypeCheckExprContextShifter);
    let mut local_scope = LocalVariableScope::new(&mut shifted_scope);
    let mut model = Model::new();
    let mut checker = TypeChecker {
        context: context.clone(),
        access,
        scope: &mut local_scope,
        model: &mut model,
    };

    let expr = checker.check_type(e);

    TypeCheckToDefaultExprContextShifter { context, model }.shift(expr)
}

pub fn type_check_expr(
    context: Context,
    options: TypeCheckOptions<'_>,
    e: &WithLocation<ast::Expr>,
    expected_type: &Expr<DefaultExprContext>,
) -> Expr<DefaultExprContext> {
    let TypeCheckOptions { access, scope } = options;
    let mut shifted_scope = ShiftedScope::new(scope, DefaultToTypeCheckExprContextShifter);
    let mut local_scope = LocalVariableScope::new(&mut shifted_scope);
    let mut model = Model::new();
    let mut checker = TypeChecker {
        context: context.clone(),
        access,
        scope: &mut local_scope,
        model: &mut model,
    };

    let expected_type = DefaultToTypeCheckExprContextShifter.shift(expected_type.clone());
    let expr = checker.check(e, &expected_type);

    TypeCheckToDefaultExprContextShifter { context, model }.shift(expr)
}

pub struct TypeCheckOptions<'a> {
    access: &'a AccessToken,
    scope: &'a mut dyn Scope<ExprContext = DefaultExprContext>,
}

impl<'a> TypeCheckOptions<'a> {
    pub fn new(
        access: &'a AccessToken,
        scope: &'a mut dyn Scope<ExprContext = DefaultExprContext>,
    ) -> Self {
        Self { access, scope }
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

#[derive(Clone, Copy)]
struct DefaultToTypeCheckExprContextShifter;

impl ExprContextShifter for DefaultToTypeCheckExprContextShifter {
    type EC1 = DefaultExprContext;
    type EC2 = TypeCheckExprContext;

    fn shift_hole(&mut self, hole: <Self::EC1 as ExprContext>::Hole) -> Expr<Self::EC2> {
        match hole {}
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
        let mut body = DefaultToTypeCheckExprContextShifter.shift(body.clone());

        let arguments = mem::take(arguments);
        let mut subst = SubstScanner::new();
        let mut shifter = DefaultToTypeCheckExprContextShifter;
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
pub struct Hole {
    hole_info: Arc<HoleInfo>,
}

impl Hole {
    pub fn new(location: Location, hole_type: Expr<TypeCheckExprContext>) -> Self {
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

#[derive(Clone)]
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
        when_true_var: Option<Variable<TypeCheckExprContext>>,
        when_false_var: Option<Variable<TypeCheckExprContext>>,
        true_body: Box<TypeInferResult<'a>>,
        false_body_location: &'a Location,
        false_body: Box<TypeInferResult<'a>>,
    },

    Sequence {
        init_exprs: Vec1<Expr<TypeCheckExprContext>>,
        last_result: Box<TypeInferResult<'a>>,
    },
}

impl<'a> TypeInferResult<'a> {
    fn error() -> Self {
        Self::Complete(InferredType {
            inferred_type: Expr::Error,
            checked_expr: Expr::Error,
        })
    }

    fn partially_inferred_type<'b>(&'b self) -> PartiallyInferredType<'b> {
        match self {
            TypeInferResult::Complete(inferred_type) => {
                PartiallyInferredType::Full(&inferred_type.inferred_type)
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
            TypeInferResult::Sequence { last_result, .. } => last_result.partially_inferred_type(),
        }
    }

    fn infer_fully(self) -> InferredType {
        match self {
            TypeInferResult::Complete(inferred_type) => inferred_type,
            _ => todo!(),
        }
    }
}

#[derive(Debug, Clone)]
struct InferredType {
    inferred_type: Expr<TypeCheckExprContext>,
    checked_expr: Expr<TypeCheckExprContext>,
}

#[derive(Debug)]
enum PartiallyInferredType<'b> {
    Full(&'b Expr<TypeCheckExprContext>),
    Closure,
    Tuple(Vec<PartiallyInferredType<'b>>),
}

#[derive(Debug, Clone, Copy)]
enum ExpectedType<'a> {
    AnyMetaType,
    Exact(&'a Expr<TypeCheckExprContext>),
}

#[derive(Debug)]
enum Overloadable<'a> {
    Base(scope::Overloadable),
    ExtensionMethod(Arc<dyn Function>, ArgumentInfo<'a>, InferredType),
    RecordField {
        record_type: Expr<TypeCheckExprContext>,
        field: Arc<dyn RecordField>,
        field_type: Expr<TypeCheckExprContext>,
        record_value: Expr<TypeCheckExprContext>,
    },
    RecordFieldStore {
        record_type: Expr<TypeCheckExprContext>,
        field: Arc<dyn RecordField>,
        field_type: Expr<TypeCheckExprContext>,
        record_value: Expr<TypeCheckExprContext>,
    },
    EnumVariant(),
}

impl<'a> Overloadable<'a> {
    fn initial_arguments_info(&self) -> Vec<ArgumentInfo<'a>> {
        match self {
            Overloadable::Base(_) => vec![],
            Overloadable::ExtensionMethod(_, arg_info, _) => vec![arg_info.clone()],
            Overloadable::RecordField { .. } => vec![],
            Overloadable::RecordFieldStore { .. } => vec![],
            Overloadable::EnumVariant() => vec![],
        }
    }

    fn initial_arguments(&self) -> Vec<TypeInferResult<'static>> {
        match self {
            Overloadable::Base(_) => vec![],
            Overloadable::ExtensionMethod(_, _, arg) => {
                vec![TypeInferResult::Complete(arg.clone())]
            }
            Overloadable::RecordField { .. } => vec![],
            Overloadable::RecordFieldStore { .. } => vec![],
            Overloadable::EnumVariant() => todo!(),
        }
    }

    fn as_expression_owner(&self) -> Option<ExpressionOwner<TypeCheckExprContext>> {
        match self {
            Overloadable::Base(base) => Some(base.as_expression_owner()),
            Overloadable::ExtensionMethod(f, _, _) => Some(ExpressionOwner::Function(f.clone())),
            Overloadable::RecordField { .. } => None,
            Overloadable::RecordFieldStore { .. } => None,
            Overloadable::EnumVariant() => todo!(),
        }
    }

    fn signature(&self) -> FunctionSignature<TypeCheckExprContext> {
        match self {
            Overloadable::Base(base) => {
                let mut shifter = DefaultToTypeCheckExprContextShifter;
                base.signature().as_ref().clone().shift(&mut shifter)
            }
            Overloadable::ExtensionMethod(f, _, _) => {
                let mut shifter = DefaultToTypeCheckExprContextShifter;
                f.clone().signature().as_ref().clone().shift(&mut shifter)
            }
            Overloadable::RecordField { field_type, .. } => FunctionSignature {
                parameters: vec![],
                return_type: field_type.clone(),
                ensures_clauses: vec![],
            },
            Overloadable::RecordFieldStore { field_type, .. } => FunctionSignature {
                parameters: vec![SignatureParameter {
                    list_type: FunctionParameterListType::NormalList,
                    name: None,
                    erasure_mode: ErasureMode::Concrete,
                    param_type: field_type.clone(),
                    bindings: vec![],
                }],
                return_type: Expr::unit(),
                ensures_clauses: vec![],
            },
            Overloadable::EnumVariant() => todo!(),
        }
    }
}

impl From<scope::Overloadable> for Overloadable<'_> {
    fn from(value: scope::Overloadable) -> Self {
        Overloadable::Base(value)
    }
}

struct TypeChecker<'access, 'scope, 'model> {
    context: Context,
    access: &'access AccessToken,
    scope: &'scope mut dyn LocalScope<ExprContext = TypeCheckExprContext>,
    model: &'model mut Model,
}

macro_rules! with_nested_scope {
    ($tc:ident) => {
        TypeChecker {
            context: $tc.context.clone(),
            access: $tc.access,
            scope: &mut LocalVariableScope::new($tc.scope),
            model: $tc.model,
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
        self.check_inferred_type(&expr.location, infer, ExpectedType::AnyMetaType)
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

            ast::Expr::BoolLiteral(value) => TypeInferResult::Complete(InferredType {
                checked_expr: Expr::BoolLiteral(*value),
                inferred_type: Expr::bool_type(),
            }),
            ast::Expr::IntLiteral(value) => TypeInferResult::Complete(InferredType {
                checked_expr: Expr::IntLiteral(value.clone()),
                inferred_type: Expr::int_type(),
            }),

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
                    None => self
                        .scope
                        .latest_loop_labels()
                        .map(LoopLabels::break_label),
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
                            BlockLabelDeclaration::Loop(labels) => {
                                Some(labels.next_label())
                            }
                        }),
                    None => self
                        .scope
                        .latest_loop_labels()
                        .map(LoopLabels::next_label),
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
                }
                else {
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
                            BlockLabelDeclaration::Loop(labels) => {
                                Some(labels.redo_label())
                            }
                        }),
                    None => self
                        .scope
                        .latest_loop_labels()
                        .map(LoopLabels::redo_label),
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
                        let a = self.check(a, &Expr::bool_type());
                        let b = self.check(b, &Expr::bool_type());
                        return TypeInferResult::Complete(InferredType {
                            checked_expr: Expr::Or(Box::new(a), Box::new(b)),
                            inferred_type: Expr::bool_type(),
                        });
                    }
                    ast::BinaryOperator::LogicalAnd => {
                        let a = self.check(a, &Expr::bool_type());
                        let b = self.check(b, &Expr::bool_type());
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
                    ast::UnaryOperator::Plus => ast::UnaryOperatorIdentifier::Plus,
                    ast::UnaryOperator::Minus => ast::UnaryOperatorIdentifier::Minus,
                    ast::UnaryOperator::BitNot => ast::UnaryOperatorIdentifier::BitNot,
                    ast::UnaryOperator::LogicalNot => ast::UnaryOperatorIdentifier::LogicalNot,
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

                    let Some(selected_overload) = resolved.overload else {
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
                        Overloadable::EnumVariant() => {
                            todo!()
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

                    let mut subst = SubstScanner::new();
                    for (i, (param, arg)) in record_type_signature
                        .parameters
                        .iter()
                        .zip(selected_overload.args.iter())
                        .enumerate()
                    {
                        let v = param.clone().to_parameter_var(expr_owner.clone(), i);
                        let v = DefaultToTypeCheckExprContextShifter
                            .shift_variable(Variable::Parameter(Box::new(v)));
                        subst.add_substitution(v, arg);
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

                        let Some((name, field)) =
                            remaining_fields.remove_entry(&field_literal.value.name.value)
                        else {
                            self.context.reporter().report_error(
                                CompileError::unknown_record_literal_field(
                                    field_literal.value.name.location.clone(),
                                    field_literal.value.name.value.to_string(),
                                ),
                            );
                            continue;
                        };

                        let mut field_type = DefaultToTypeCheckExprContextShifter
                            .shift((*field.field_type()).clone());
                        subst.scan(&mut field_type);

                        let value = self.check(&field_literal.value.value, &field_type);

                        converted_fields.push(RecordFieldLiteral { name, value });
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
                                arguments: selected_overload.args.clone(),
                            });
                        }
                        RecordFieldOwner::EnumVariant(_) => {
                            todo!()
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
                    Expr::Builtin {
                        builtin: Builtin::StringConcat,
                        arguments: parts,
                    }
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
            ast::Expr::FunctionLiteral(_) => todo!("infer function literals"),
            ast::Expr::FunctionType { .. } => todo!("infer function types"),
            ast::Expr::IfElse {
                condition,
                when_true,
                when_false,
            } => {
                let conv_condition = self.check(condition, &Expr::bool_type());
                let true_body_result = with_nested_scope!(self).infer_block(when_true);
                let false_body_result = with_nested_scope!(self).infer_block(when_false);

                let (when_true_var, when_false_var) = self.create_if_cond_vars(&conv_condition);

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
                                when_true_var,
                                when_false_var,
                                condition: Box::new(conv_condition),
                                when_true: Box::new(checked_true_body),
                                when_false: Box::new(checked_false_body),
                            },
                            inferred_type: branch_type,
                        })
                    }

                    _ => TypeInferResult::IfElse {
                        condition: Box::new(conv_condition),
                        when_true_var,
                        when_false_var,
                        true_body: Box::new(true_body_result),
                        false_body_location: &when_false.location,
                        false_body: Box::new(false_body_result),
                    },
                }
            }
            ast::Expr::Is { .. } => todo!("infer is expressions"),

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

            ast::Expr::Match { .. } => todo!("infer match expressions"),
            ast::Expr::NewTraitObject { .. } => todo!("infer trait object construction"),
            ast::Expr::Raise { .. } => todo!("infer raise expressions"),
            ast::Expr::Summon { .. } => todo!("infer summon expressions"),

            ast::Expr::Tuple { items } => TypeInferResult::Tuple {
                location: &expr.location,
                elements: items.iter().map(|item| self.infer(item)).collect(),
            },

            ast::Expr::While { .. } => todo!("infer while expressions"),
            ast::Expr::BoxedType { .. } => todo!("infer boxed types"),
            ast::Expr::Box { .. } => todo!("infer box expressions"),
            ast::Expr::Unbox { .. } => todo!("infer unbox expressions"),
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

                let v = Variable::Local(Box::new(LocalVariable {
                    id: UniqueIdentifier::new(),
                    name: v.name.clone(),
                    var_type,
                    erasure_mode,
                    is_witness,
                    is_mutable: v.is_mutable,
                }));

                self.scope.add_variable(v.clone());

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
            CalleeInfo::Variable(v) => self.infer_variable(v, call.arguments),
            CalleeInfo::VariableStore(v) => {
                self.infer_variable_store(call.location, v, call.arguments)
            }
            CalleeInfo::VariableTupleElement(vte) => {
                self.infer_variable_tuple_element(vte, call.arguments)
            }
            CalleeInfo::Overloadable(overloads) => {
                let resolved = OverloadResolver::new(self, call.location)
                    .resolve_overload_lookup(overloads, call.arguments);

                let Some(inferred_type) = resolved.overload else {
                    return TypeInferResult::error();
                };

                self.infer_function_object_call_inferred(
                    TypeInferResult::Complete(inferred_type.into_inferred_type(self)),
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
        }
    }

    fn infer_builtin<'e>(
        &mut self,
        location: &'e Location,
        builtin: Builtin,
        args: VecDeque<ArgumentInfo<'e>>,
    ) -> TypeInferResult<'e> {
        match builtin {
            Builtin::IntType | Builtin::BoolType | Builtin::StringType | Builtin::NeverType => {
                self.infer_fixed_builtin(location, builtin, args, [], Expr::type_n(0))
            }

            Builtin::ArrayType => self.infer_parameterized_builtin(
                location,
                builtin,
                args,
                |_| [],
                |_, element_type_type| element_type_type,
            ),

            Builtin::IntNegate | Builtin::IntBitNot => self.infer_fixed_builtin(
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
                location,
                builtin,
                args,
                [Expr::int_type(), Expr::int_type()],
                Expr::bool_type(),
            ),

            Builtin::StringConcat => {
                let checked_args = args
                    .iter()
                    .map(|arg| self.check(arg.arg, &Expr::string_type()))
                    .collect();

                TypeInferResult::Complete(InferredType {
                    checked_expr: Expr::Builtin {
                        builtin,
                        arguments: checked_args,
                    },
                    inferred_type: Expr::string_type(),
                })
            }

            Builtin::StringEq | Builtin::StringNe => self.infer_fixed_builtin(
                location,
                builtin,
                args,
                [Expr::string_type(), Expr::string_type()],
                Expr::bool_type(),
            ),

            Builtin::BoolNot => self.infer_fixed_builtin(
                location,
                builtin,
                args,
                [Expr::bool_type()],
                Expr::bool_type(),
            ),

            Builtin::BoolEq | Builtin::BoolNe => self.infer_fixed_builtin(
                location,
                builtin,
                args,
                [Expr::bool_type(), Expr::bool_type()],
                Expr::bool_type(),
            ),

            Builtin::ArrayCreateUnsafeUninitialized => self.infer_parameterized_builtin(
                location,
                builtin,
                args,
                |_| [Expr::int_type()],
                |element_type, _| Expr::array_type(element_type.clone()),
            ),

            Builtin::ArrayLength => self.infer_parameterized_builtin(
                location,
                builtin,
                args,
                |element_type| [Expr::array_type(element_type.clone())],
                |_, _| Expr::int_type(),
            ),

            Builtin::ArrayGet => self.infer_parameterized_builtin(
                location,
                builtin,
                args,
                |element_type| [Expr::array_type(element_type.clone()), Expr::int_type()],
                |element_type, _| element_type.clone(),
            ),

            Builtin::ArraySet => self.infer_parameterized_builtin(
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
                |_, _| Expr::unit(),
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

    fn infer_fixed_builtin<'e, const ARG_COUNT: usize>(
        &mut self,
        location: &Location,
        builtin: Builtin,
        args: VecDeque<ArgumentInfo<'e>>,
        expected_arg_types: [Expr<TypeCheckExprContext>; ARG_COUNT],
        inferred_type: Expr<TypeCheckExprContext>,
    ) -> TypeInferResult<'e> {
        if args.len() != ARG_COUNT {
            self.report_builtin_arity_error(location, builtin, ARG_COUNT, args.len());
            return TypeInferResult::error();
        }

        let checked_args = args
            .iter()
            .zip(expected_arg_types)
            .map(|(arg, expected_type)| self.check(arg.arg, &expected_type))
            .collect();

        TypeInferResult::Complete(InferredType {
            checked_expr: Expr::Builtin {
                builtin,
                arguments: checked_args,
            },
            inferred_type,
        })
    }

    fn infer_parameterized_builtin<'e, const REST_ARG_COUNT: usize>(
        &mut self,
        location: &'e Location,
        builtin: Builtin,
        mut args: VecDeque<ArgumentInfo<'e>>,
        create_rest_arg_types: impl FnOnce(
            &Expr<TypeCheckExprContext>,
        ) -> [Expr<TypeCheckExprContext>; REST_ARG_COUNT],
        create_result_type: impl FnOnce(
            &Expr<TypeCheckExprContext>,
            Expr<TypeCheckExprContext>,
        ) -> Expr<TypeCheckExprContext>,
    ) -> TypeInferResult<'e> {
        let expected_arg_count = REST_ARG_COUNT + 1;
        let total_arg_count = args.len();

        let Some(element_type_arg) = args.pop_front() else {
            self.report_builtin_arity_error(location, builtin, expected_arg_count, 0);
            return TypeInferResult::error();
        };

        let InferredType {
            checked_expr: element_type,
            inferred_type: element_type_type,
        } = self.check_type_with_meta_type(element_type_arg.arg);

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
                .map(|(arg, expected_type)| self.check(arg.arg, &expected_type)),
        );

        TypeInferResult::Complete(InferredType {
            checked_expr: Expr::Builtin {
                builtin,
                arguments: checked_args,
            },
            inferred_type: create_result_type(&element_type, element_type_type),
        })
    }

    fn infer_variable<'e>(
        &mut self,
        v: Variable<TypeCheckExprContext>,
        args: VecDeque<ArgumentInfo<'e>>,
    ) -> TypeInferResult<'e> {
        let t = v.var_type().clone();
        let expr = Expr::Variable(v);
        let res = TypeInferResult::Complete(InferredType {
            checked_expr: expr,
            inferred_type: t,
        });

        self.infer_function_object_call(res, args)
    }

    fn infer_variable_store<'e>(
        &mut self,
        location: &'e Location,
        v: Variable<TypeCheckExprContext>,
        mut args: VecDeque<ArgumentInfo<'e>>,
    ) -> TypeInferResult<'e> {
        let t = v.var_type().clone();

        let value = args
            .pop_front()
            .expect("Variable value should have been added as an argument");

        let value = self.check(value.arg, &t);

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
        for (arg_result, arg) in args {
            match expr_result.partially_inferred_type() {
                PartiallyInferredType::Full(func_type @ Expr::FunctionType { a, r }) => {
                    let func_type = func_type.clone();
                    let r = (**r).clone();

                    let checked_arg = self.check_inferred_type(
                        &arg.arg.location,
                        arg_result,
                        ExpectedType::Exact(a),
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
                }

                PartiallyInferredType::Closure => todo!(),

                t => {
                    self.context
                        .reporter()
                        .report_error(CompileError::function_type_required(
                            arg.call_location.clone(),
                            format!("{:?}", t),
                        ));

                    return TypeInferResult::error();
                }
            }
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
                        list_type: *list_type,
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
                    let callee = self.process_lookup(
                        &func_expr.location,
                        identifier,
                        assigned_value.is_some(),
                    );

                    if !arguments.is_empty() && matches!(callee, CalleeInfo::VariableStore(_)) {
                        self.report_invalid_assignment_target(call_location);
                        return CallInfo {
                            location: call_location,
                            callee: CalleeInfo::Error,
                            arguments,
                        };
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
                        callee,
                        arguments,
                    };
                }

                ast::Expr::Dot { o, member } => {
                    let mut overload_groups = Vec::new();

                    'done: {
                        let mut instance = self.process_call(o, None);
                        let call_location = instance.location;

                        let adjusted_member_name = if assigned_value.is_some() {
                            Identifier::Update(Box::new(member.value.clone()))
                        } else {
                            member.value.clone()
                        };

                        // Find enum variants
                        // if arguments.is_empty() && let CalleeInfo::Overloadable(instance_overloads) = &mut instance.callee {
                        //     if let Some(e) = instance_overloads.item_groups
                        //         .iter()
                        //         .flatten()
                        //         .find_map(|overload| match overload {
                        //             Overloadable::Enum(e) => Some(e),
                        //             _ => None,
                        //         })
                        //     {
                        //         todo!("Find enum variants");
                        //
                        //         let sig = r.signature();
                        //         if sig.parameters.is_empty() {
                        //             break 'done;
                        //         }
                        //     }
                        // }

                        let mut instance = self.infer_call(instance).infer_fully();

                        {
                            let mut norm = NormalizerScanner::new(
                                self.context.normalize_fuel(),
                                ExprNormalizer { model: self.model },
                            );
                            norm.normalize(&mut instance.inferred_type);
                        }

                        match &instance.inferred_type {
                            Expr::RecordType(record_type) => {
                                let r = &record_type.record;
                                let args = &record_type.arguments;
                                overload_groups.extend(
                                    r.clone()
                                        .fields()
                                        .iter()
                                        .find(|field| field.metadata().name == member.value)
                                        .map(|field| {
                                            let mut field_type =
                                                DefaultToTypeCheckExprContextShifter
                                                    .shift((*field.clone().field_type()).clone());

                                            let mut subst = SubstScanner::new();
                                            let record_sig = (*r.clone().signature())
                                                .clone()
                                                .shift(&mut DefaultToTypeCheckExprContextShifter);

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
                            Expr::TraitType(..) => todo!("Trait members"),
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
            callee: self.process_lookup(op_location, &Identifier::BinaryOp(op), false),
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
            callee: self.process_lookup(op_location, &Identifier::UnaryOp(op), false),
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
        is_assignment: bool,
    ) -> CalleeInfo<'b> {
        let lookup = if is_assignment {
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
                if is_assignment {
                    CalleeInfo::VariableStore(v)
                } else {
                    CalleeInfo::Variable(v)
                }
            }
            Lookup::VariableTupleElement(vte) => {
                if is_assignment {
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
            TypeInferResult::Closure { .. } => {
                todo!()
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
                when_true_var,
                when_false_var,
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
                        when_true_var,
                        when_false_var,
                        condition,
                        when_true: Box::new(true_body_inferred.checked_expr),
                        when_false: Box::new(checked_false_body),
                    },
                    inferred_type: true_body_inferred.inferred_type,
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
        let inferred = self.resolve_inferred_type(infer, expected_type);

        if !self.type_matches_expected(&inferred.inferred_type, expected_type) {
            self.context
                .reporter()
                .report_error(CompileError::type_mismatch(
                    location.clone(),
                    format!("{:?}", inferred.inferred_type),
                    format!("{:?}", expected_type),
                ));
        }

        inferred
    }

    fn create_if_cond_vars(
        &mut self,
        cond_expr: &Expr<TypeCheckExprContext>,
    ) -> (
        Option<Variable<TypeCheckExprContext>>,
        Option<Variable<TypeCheckExprContext>>,
    ) {
        let is_pure_cond = PurityScanner::contains_impure_function_call(cond_expr);

        // TODO: Get conditional vars from pattern expressions
        let when_true_var = is_pure_cond.then(|| self.create_if_branch_var(cond_expr, true));
        let when_false_var = is_pure_cond.then(|| self.create_if_branch_var(cond_expr, false));

        (when_true_var, when_false_var)
    }

    fn create_if_branch_var(
        &mut self,
        cond_expr: &Expr<TypeCheckExprContext>,
        equal_to_value: bool,
    ) -> Variable<TypeCheckExprContext> {
        Variable::Local(Box::new(LocalVariable {
            id: UniqueIdentifier::new(),
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

    fn report_invalid_assignment_target(&self, location: &Location) {
        self.context
            .reporter()
            .report_error(CompileError::invalid_assignment_target(location.clone()));
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

    fn unify(
        &mut self,
        mut a: Expr<TypeCheckExprContext>,
        mut b: Expr<TypeCheckExprContext>,
    ) -> bool {
        {
            let mut norm = NormalizerScanner::new(
                self.context.normalize_fuel(),
                ExprNormalizer {
                    model: &mut self.model,
                },
            );
            norm.normalize(&mut a);
        }
        {
            let mut norm = NormalizerScanner::new(
                self.context.normalize_fuel(),
                ExprNormalizer {
                    model: &mut self.model,
                },
            );
            norm.normalize(&mut b);
        }

        match (a, b) {
            (Expr::Error, _) | (_, Expr::Error) => true,
            (Expr::Hole(a), Expr::Hole(b)) if a == b => true,

            (Expr::Hole(a), b) => {
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

            (a, Expr::Hole(b)) => {
                match self.model.hole_values.entry_ref(&b) {
                    hash_map::EntryRef::Occupied(oeb) => {
                        let b_value = oeb.get().clone();
                        self.unify(b_value, a)
                    }
                    hash_map::EntryRef::Vacant(veb) => {
                        // TODO: Type check the hole
                        veb.insert(a.clone());
                        true
                    }
                }
            }

            (Expr::And(a1, b1), Expr::And(a2, b2)) | (Expr::Or(a1, b1), Expr::Or(a2, b2)) => {
                self.unify(*a1, *a2) && self.unify(*b1, *b2)
            }
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
            ) => a == b && self.unify_all(a_args, b_args),
            (Expr::EnumType(a, a_args), Expr::EnumType(b, b_args)) => {
                a == b && self.unify_all(a_args, b_args)
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
            ) => self.unify(*a_arg, *b_arg) && self.unify(*a_result, *b_result),
            (Expr::IntLiteral(a), Expr::IntLiteral(b)) => a == b,
            (Expr::RecordType(a), Expr::RecordType(b)) => {
                &a.record == &b.record && self.unify_all(a.arguments, b.arguments)
            }
            (Expr::StringLiteral(a), Expr::StringLiteral(b)) => a == b,
            (Expr::TraitType(a, a_args), Expr::TraitType(b, b_args)) => {
                a == b && self.unify_all(a_args, b_args)
            }
            (Expr::Tuple { items: a }, Expr::Tuple { items: b }) => self.unify_all(a, b),
            (Expr::Type(a), Expr::Type(b)) => self.unify(*a, *b),
            (Expr::BigType(a), Expr::BigType(b)) => a == b,
            (Expr::Variable(a), Expr::Variable(b)) => a == b,
            _ => false,
        }
    }

    fn unify_all(
        &mut self,
        a: Vec<Expr<TypeCheckExprContext>>,
        b: Vec<Expr<TypeCheckExprContext>>,
    ) -> bool {
        a.len() == b.len() && a.into_iter().zip(b).all(|(a, b)| self.unify(a, b))
    }

    fn is_type(&mut self, mut expr: Expr<TypeCheckExprContext>) -> bool {
        let mut norm = NormalizerScanner::new(
            self.context.normalize_fuel(),
            ExprNormalizer {
                model: &mut self.model,
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
        actual_type: &Expr<TypeCheckExprContext>,
        expected_type: ExpectedType<'_>,
    ) -> bool {
        match expected_type {
            ExpectedType::AnyMetaType => self.is_type(actual_type.clone()),

            ExpectedType::Exact(expected_type) => {
                let mut expected_type = expected_type.clone();
                {
                    let mut norm = NormalizerScanner::new(
                        self.context.normalize_fuel(),
                        ExprNormalizer {
                            model: &mut self.model,
                        },
                    );
                    norm.normalize(&mut expected_type);
                }

                let mut actual_type = actual_type.clone();
                {
                    let mut norm = NormalizerScanner::new(
                        self.context.normalize_fuel(),
                        ExprNormalizer {
                            model: &mut self.model,
                        },
                    );
                    norm.normalize(&mut actual_type);
                }

                match &actual_type {
                    Expr::Type(n) => match &expected_type {
                        Expr::BigType(_) => return true,
                        Expr::Type(n2) => match (&**n, &**n2) {
                            (Expr::IntLiteral(n), Expr::IntLiteral(n2)) => {
                                return n >= &BigInt::ZERO && n2 >= &BigInt::ZERO && n <= n2;
                            }
                            _ => {}
                        },
                        _ => {}
                    },

                    Expr::Builtin {
                        builtin: Builtin::NeverType,
                        ..
                    } => {
                        return true;
                    }

                    _ => {}
                }

                self.unify(expected_type, actual_type)
            }
        }
    }

    fn partially_inferred_type_matches_expected(
        &mut self,
        actual_type: &PartiallyInferredType<'_>,
        expected_type: ExpectedType<'_>,
    ) -> bool {
        match actual_type {
            PartiallyInferredType::Full(actual_type) => {
                self.type_matches_expected(actual_type, expected_type)
            }
            PartiallyInferredType::Closure => {
                todo!()
            }
            PartiallyInferredType::Tuple(elements) => match expected_type {
                ExpectedType::AnyMetaType
                | ExpectedType::Exact(Expr::Type(_) | Expr::BigType(_)) => elements
                    .iter()
                    .all(|e| self.partially_inferred_type_matches_expected(e, expected_type)),

                ExpectedType::Exact(Expr::Tuple { items }) if items.len() == elements.len() => {
                    elements.iter().zip(items.iter()).all(|(actual, expected)| {
                        self.partially_inferred_type_matches_expected(
                            actual,
                            ExpectedType::Exact(expected),
                        )
                    })
                }

                _ => false,
            },
        }
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
    Builtin(Builtin),
    Variable(Variable<TypeCheckExprContext>),
    VariableStore(Variable<TypeCheckExprContext>),
    VariableTupleElement(VariableTupleElement<TypeCheckExprContext>),
    Overloadable(Vec<Vec<Overloadable<'a>>>),
    TypeN,
}

struct CallInfo<'a> {
    location: &'a Location,
    callee: CalleeInfo<'a>,
    arguments: VecDeque<ArgumentInfo<'a>>,
}

struct OverloadResolver<'parent, 'access, 'scope, 'model, 'e> {
    type_checker: &'parent mut TypeChecker<'access, 'scope, 'model>,
    call_location: &'e Location,
    rejected_overloads: Vec<(Overloadable<'e>, OverloadRejectionReason)>,
}

impl<'parent, 'access, 'scope, 'model, 'e> OverloadResolver<'parent, 'access, 'scope, 'model, 'e> {
    fn new(
        type_checker: &'parent mut TypeChecker<'access, 'scope, 'model>,
        call_location: &'e Location,
    ) -> Self {
        Self {
            type_checker,
            call_location,
            rejected_overloads: Vec::new(),
        }
    }

    fn resolve_overload_lookup(
        mut self,
        overload_lookup: Vec<Vec<Overloadable<'e>>>,
        mut args: VecDeque<ArgumentInfo<'e>>,
    ) -> ResolvedOverload<'e> {
        let mut inferred_args = args
            .iter()
            .map(|arg| self.type_checker.infer(arg.arg))
            .collect::<VecDeque<_>>();

        let overload = 'find_overload: {
            for group in overload_lookup {
                for mut group in self.group_overloads_by_arity(group, MultiSlice::from(&args)) {
                    let mut i = 0;
                    while i < group.len() {
                        if let Some(reason) = self.is_overload_applicable(
                            &group[i],
                            MultiSlice::from(&args),
                            MultiSlice::from(&inferred_args),
                        ) {
                            let overload = group.swap_remove(i);
                            self.rejected_overloads.push((overload, reason));
                        } else {
                            i += 1;
                        }
                    }

                    let Some(selected_overload) = group.pop() else {
                        continue;
                    };

                    if !group.is_empty() {
                        self.type_checker.context.reporter().report_error(
                            CompileError::ambiguous_overload(self.call_location.clone()),
                        );
                    }

                    break 'find_overload selected_overload;
                }
            }

            self.type_checker
                .context
                .reporter()
                .report_error(CompileError::invalid_overload(self.call_location.clone()));

            return ResolvedOverload {
                overload: None,
                extra_argument_info: args,
                extra_inferred_args: inferred_args,
            };
        };

        let selected = self.select_overload(overload, &mut args, &mut inferred_args);

        ResolvedOverload {
            overload: Some(selected),
            extra_argument_info: args,
            extra_inferred_args: inferred_args,
        }
    }

    fn group_overloads_by_arity(
        &mut self,
        overloads: Vec<Overloadable<'e>>,
        args: MultiSlice<'_, ArgumentInfo<'e>>,
    ) -> impl Iterator<Item = Vec<Overloadable<'e>>> + 'e {
        let mut groups: BTreeMap<OverloadArityRank, Vec<Overloadable>> = BTreeMap::new();

        for overload in overloads {
            match self.rank_by_arity(&overload, args.clone()) {
                Some(rank) => groups.entry(rank).or_default().push(overload),
                None => self
                    .rejected_overloads
                    .push((overload, OverloadRejectionReason::ParameterListTypeMismatch)),
            }
        }

        groups.into_values()
    }

    fn rank_by_arity(
        &self,
        overload: &Overloadable<'e>,
        mut args: MultiSlice<'_, ArgumentInfo<'e>>,
    ) -> Option<OverloadArityRank> {
        let sig = overload.signature();

        let mut params = &sig.parameters[..];

        let mut num_inferred = 0;

        args.push_front_vec(overload.initial_arguments_info());

        loop {
            let Some((param, tail_params)) = params.split_first() else {
                if args.is_empty() {
                    return Some(OverloadArityRank::Exact(num_inferred));
                } else {
                    return Some(OverloadArityRank::More(args.len()));
                }
            };

            if let Some(arg) = args.first() {
                if param.list_type == arg.list_type {
                    args.pop_front();
                } else if param.list_type == FunctionParameterListType::NormalList {
                    return None;
                } else {
                    num_inferred += 1;
                }
            } else if param.list_type == FunctionParameterListType::NormalList {
                return Some(OverloadArityRank::Less(params.len()));
            } else {
                num_inferred += 1;
            }

            params = tail_params;
        }
    }

    fn is_overload_applicable(
        &self,
        overload: &Overloadable<'e>,
        mut args: MultiSlice<'_, ArgumentInfo<'e>>,
        mut inferred_args: MultiSlice<'_, TypeInferResult<'e>>,
    ) -> Option<OverloadRejectionReason> {
        let sig = overload.signature();

        let mut model = self.type_checker.model.clone();
        let mut scope = LocalVariableScope::new(self.type_checker.scope);
        let mut type_checker = TypeChecker {
            context: self.type_checker.context.clone(),
            model: &mut model,
            access: self.type_checker.access,
            scope: &mut scope,
        };

        let mut param_types = sig
            .parameters
            .iter()
            .map(|p| p.param_type.clone())
            .collect::<VecDeque<_>>();

        let mut return_type = sig.return_type.clone();

        let mut params = &sig.parameters[..];
        let mut parameter_index = 0;

        args.push_front_vec(overload.initial_arguments_info());
        inferred_args.push_front_vec(overload.initial_arguments());

        loop {
            let Some((param, tail_params)) = params.split_first() else {
                break;
            };

            let Some(param_type) = param_types.pop_front() else {
                break;
            };

            // Skip substitution if an expression owner is not available.
            // This is safe because these overloads either do not have parameters
            // or they have a single parameter and a fixed result type.

            let v: Option<Variable<TypeCheckExprContext>> =
                overload.as_expression_owner().map(|owner| {
                    Variable::Parameter(Box::new(
                        param.clone().to_parameter_var(owner, parameter_index),
                    ))
                });

            if let (Some(arg), Some(inferred_arg)) = (args.first(), inferred_args.first())
                && param.list_type == arg.list_type
            {
                if !type_checker.partially_inferred_type_matches_expected(
                    &inferred_arg.partially_inferred_type(),
                    ExpectedType::Exact(&param_type),
                ) {
                    return Some(OverloadRejectionReason::ParameterTypeMismatch {
                        parameter_index,
                    });
                }

                if let Some(v) = v {
                    self.substitute_inferred_arg_in_param_types(
                        &mut param_types,
                        &mut return_type,
                        v,
                        &inferred_arg,
                    );
                }

                args.pop_front();
                inferred_args.pop_front();
            } else {
                match param.list_type {
                    FunctionParameterListType::NormalList => {
                        return Some(OverloadRejectionReason::ParameterListTypeMismatch);
                    }
                    FunctionParameterListType::InferrableList
                    | FunctionParameterListType::QuoteList => {
                        let hole = Hole::new(self.call_location.clone(), param_type);
                        if let Some(v) = v {
                            self.substitute_arg_in_param_types(
                                &mut param_types,
                                &mut return_type,
                                v,
                                &Expr::Hole(hole),
                            );
                        }
                    }
                    FunctionParameterListType::RequiresList => {
                        todo!("implicit resolution")
                    }
                }
            }

            parameter_index += 1;
            params = tail_params;
        }

        // Process extra arguments by comparing argument types to return type
        while let (Some(arg), Some(inferred_arg)) = (args.pop_front(), inferred_args.pop_front()) {
            todo!()
        }

        None
    }

    fn substitute_inferred_arg_in_param_types(
        &self,
        param_types: &mut VecDeque<Expr<TypeCheckExprContext>>,
        return_type: &mut Expr<TypeCheckExprContext>,
        v: Variable<TypeCheckExprContext>,
        arg: &TypeInferResult<'e>,
    ) {
        if let TypeInferResult::Complete(inferred_type) = arg {
            self.substitute_arg_in_param_types(
                param_types,
                return_type,
                v,
                &inferred_type.checked_expr,
            );
        } else {
            self.substitute_arg_in_param_types(param_types, return_type, v, &Expr::Error);
        }
    }

    fn substitute_arg_in_param_types(
        &self,
        param_types: &mut VecDeque<Expr<TypeCheckExprContext>>,
        return_type: &mut Expr<TypeCheckExprContext>,
        v: Variable<TypeCheckExprContext>,
        arg: &Expr<TypeCheckExprContext>,
    ) {
        let mut scanner = SubstScanner::new();
        scanner.add_substitution(v, arg);

        for param_type in param_types.iter_mut() {
            scanner.scan(param_type);
        }

        scanner.scan(return_type);
    }

    fn select_overload(
        &mut self,
        overload: Overloadable<'e>,
        args: &mut VecDeque<ArgumentInfo<'e>>,
        inferred_args: &mut VecDeque<TypeInferResult<'e>>,
    ) -> SelectedOverload<'e> {
        let sig = overload.signature();

        let mut param_types = sig
            .parameters
            .iter()
            .map(|p| p.param_type.clone())
            .collect::<VecDeque<_>>();

        let mut return_type = sig.return_type.clone();

        let mut params = &sig.parameters[..];
        let mut parameter_index = 0;

        for initial_arg in overload.initial_arguments_info().into_iter().rev() {
            args.push_front(initial_arg);
        }

        for initial_arg in overload.initial_arguments().into_iter().rev() {
            inferred_args.push_front(initial_arg);
        }

        let mut selected_args = Vec::with_capacity(inferred_args.len());

        loop {
            let Some((param, tail_params)) = params.split_first() else {
                break;
            };

            let Some(param_type) = param_types.pop_front() else {
                break;
            };

            let v: Option<Variable<TypeCheckExprContext>> =
                overload.as_expression_owner().map(|owner| {
                    Variable::Parameter(Box::new(
                        param.clone().to_parameter_var(owner, parameter_index),
                    ))
                });

            if let Some(arg) = args.front()
                && param.list_type == arg.list_type
            {
                let arg_result = inferred_args
                    .pop_front()
                    .expect("inferred_args should not be empty when args is not empty");

                let arg_expr = self.type_checker.check_inferred_type(
                    &arg.arg.location,
                    arg_result,
                    ExpectedType::Exact(&param_type),
                );

                if let Some(v) = v {
                    self.substitute_arg_in_param_types(
                        &mut param_types,
                        &mut return_type,
                        v,
                        &arg_expr.checked_expr,
                    );
                }

                selected_args.push(arg_expr.checked_expr);

                args.pop_front();
            } else {
                match param.list_type {
                    FunctionParameterListType::NormalList => {
                        unreachable!(
                            "Parameter list type mismatch should have been caught earlier"
                        );
                    }
                    FunctionParameterListType::InferrableList
                    | FunctionParameterListType::QuoteList => {
                        let hole = Hole::new(self.call_location.clone(), param_type);

                        if let Some(v) = v {
                            self.substitute_arg_in_param_types(
                                &mut param_types,
                                &mut return_type,
                                v,
                                &Expr::Hole(hole),
                            );
                        }
                    }
                    FunctionParameterListType::RequiresList => {
                        todo!("implicit resolution")
                    }
                }
            }

            parameter_index += 1;
            params = tail_params;
        }

        SelectedOverload {
            call_location: self.call_location,
            overload,
            args: selected_args,
            return_type,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum OverloadArityRank {
    Exact(usize),
    More(usize),
    Less(usize),
}

impl Ord for OverloadArityRank {
    fn cmp(&self, other: &Self) -> Ordering {
        use OverloadArityRank::*;

        match (self, other) {
            (Exact(left), Exact(right)) | (More(left), More(right)) | (Less(left), Less(right)) => {
                left.cmp(right)
            }

            (Exact(_), _) => Ordering::Less,
            (_, Exact(_)) => Ordering::Greater,
            (More(_), Less(_)) => Ordering::Less,
            (Less(_), More(_)) => Ordering::Greater,
        }
    }
}

impl PartialOrd for OverloadArityRank {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

enum OverloadRejectionReason {
    ParameterListTypeMismatch,
    ParameterTypeMismatch { parameter_index: usize },
}

struct ResolvedOverload<'e> {
    overload: Option<SelectedOverload<'e>>,
    extra_argument_info: VecDeque<ArgumentInfo<'e>>,
    extra_inferred_args: VecDeque<TypeInferResult<'e>>,
}

struct SelectedOverload<'e> {
    call_location: &'e Location,
    overload: Overloadable<'e>,
    args: Vec<Expr<TypeCheckExprContext>>,
    return_type: Expr<TypeCheckExprContext>,
}

impl<'a> SelectedOverload<'a> {
    fn into_inferred_type(mut self, checker: &TypeChecker<'_, '_, '_>) -> InferredType {
        let expr = match self.overload {
            Overloadable::Base(scope::Overloadable::Function(f)) => Expr::FunctionCall {
                function: f,
                arguments: self.args,
            },
            Overloadable::Base(scope::Overloadable::Record(r)) => Expr::RecordType(RecordType {
                record: r,
                arguments: self.args,
            }),
            Overloadable::ExtensionMethod(f, _, _) => Expr::FunctionCall {
                function: f,
                arguments: self.args,
            },
            Overloadable::RecordField {
                record_type,
                field,
                record_value,
                ..
            } => Expr::RecordFieldLoad {
                record_type: Box::new(record_type),
                field,
                record_value: Box::new(record_value),
            },
            Overloadable::RecordFieldStore {
                record_type,
                field,
                record_value,
                ..
            } => {
                let value = self
                    .args
                    .pop()
                    .expect("Record field store should have an argument");

                if !field.metadata().is_mutable {
                    checker
                        .context
                        .reporter()
                        .report_error(CompileError::can_not_mutate(self.call_location.clone()));
                }

                Expr::RecordFieldStore {
                    record_type: Box::new(record_type),
                    field,
                    record_value: Box::new(record_value),
                    new_value: Box::new(value),
                }
            }
            _ => todo!(
                "into_inferred_type: unimplemented overloadable {:?}",
                self.overload
            ),
        };

        InferredType {
            checked_expr: expr,
            inferred_type: self.return_type,
        }
    }
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
            Expr::Builtin {
                builtin: Builtin::NeverType,
                ..
            } => {
                self.branch_type = t.clone();
            }

            _ => {}
        }
    }

    fn into_branch_type(self) -> Expr<TypeCheckExprContext> {
        self.branch_type
    }
}
