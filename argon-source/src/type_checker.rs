use argon_compiler::scanner::PurityScanner;
use argon_compiler::scope::{
    LocalVariableScope, Lookup, OverloadLookup, Overloadable, Scope, ShiftedScope,
};
use argon_compiler::{Context, DefaultExprContext, FunctionSignature};
use argon_expr::{
    Builtin, ErasureMode, Expr, ExprContext, ExprContextShifter,
    LocalVariable, Variable, VariableTupleElement,
};
use argon_parser::ast;
use argon_parser::ast::{FunctionParameterListType, Identifier, StringFragment};
use argon_util::{CompileError, UniqueIdentifier, VecDequeSlice};
use nonempty_collections::{IntoNonEmptyIterator, NEVec, NonEmptyIterator};
use num_bigint::BigInt;
use parse18_runtime::{Location, WithLocation};
use std::collections::{btree_map, BTreeMap, HashMap, VecDeque};
use std::hash::{Hash, Hasher};
use std::sync::Arc;



pub fn type_check_type_expr(
    context: Context,
    scope: &mut dyn Scope<ExprContext = DefaultExprContext>,
    e: &WithLocation<ast::Expr>,
) -> Expr<DefaultExprContext> {
    let mut shifted_scope = ShiftedScope::new(scope, DefaultToTypeCheckExprContextShifter);
    let mut local_scope = LocalVariableScope::new(&mut shifted_scope);
    let mut checker = TypeChecker::new(context.clone(), &mut local_scope);

    let expr = checker.check_type(e);

    TypeCheckToDefaultExprContextShifter.shift(expr)
}

pub fn type_check_expr(
    context: Context,
    scope: &mut dyn Scope<ExprContext = DefaultExprContext>,
    e: &WithLocation<ast::Expr>,
    expected_type: &Expr<DefaultExprContext>,
) -> Expr<DefaultExprContext> {
    let mut shifted_scope = ShiftedScope::new(scope, DefaultToTypeCheckExprContextShifter);
    let mut local_scope = LocalVariableScope::new(&mut shifted_scope);
    let mut checker = TypeChecker::new(context.clone(), &mut local_scope);

    let expected_type = DefaultToTypeCheckExprContextShifter.shift(expected_type.clone());
    let expr = checker.check(e, &expected_type);

    TypeCheckToDefaultExprContextShifter.shift(expr)
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

#[derive(Clone, Copy)]
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

#[derive(Clone)]
struct Model {
    hole_values: HashMap<UniqueIdentifier, Expr<TypeCheckExprContext>>,
}

impl Model {
    fn new() -> Self {
        Self {
            hole_values: HashMap::new(),
        }
    }
}

enum TypeInferResult {
    Complete {
        inferred_type: Expr<TypeCheckExprContext>,
        checked_expr: Expr<TypeCheckExprContext>,
    },
}

impl TypeInferResult {
    fn complete(
        checked_expr: Expr<TypeCheckExprContext>,
        inferred_type: Expr<TypeCheckExprContext>,
    ) -> Self {
        Self::Complete {
            inferred_type,
            checked_expr,
        }
    }

    fn error() -> Self {
        Self::complete(Expr::Error, Expr::Error)
    }
}


struct TypeChecker<'a> {
    context: Context,
    scope: &'a mut dyn Scope<ExprContext = TypeCheckExprContext>,
    model: Model,
}

impl <'a> TypeChecker<'a> {
    fn new(context: Context, scope: &'a mut dyn Scope<ExprContext = TypeCheckExprContext>) -> Self {
        Self {
            context,
            scope,
            model: Model::new(),
        }
    }

    fn with_scope<'b>(&'a mut self, scope: &'b mut dyn Scope<ExprContext = TypeCheckExprContext>) -> TypeChecker<'b> {
        TypeChecker {
            context: self.context.clone(),
            scope,
            model: self.model.clone(),
        }
    }
}

impl <'a> TypeChecker<'a> {

    fn check(
        &mut self,
        expr: &WithLocation<ast::Expr>,
        expected_type: &Expr<TypeCheckExprContext>,
    ) -> Expr<TypeCheckExprContext> {
        let infer = self.infer(expr);
        self.check_inferred_type(&expr.location, infer, expected_type)
    }

    fn check_type(
        &mut self,
        expr: &WithLocation<ast::Expr>,
    ) -> Expr<TypeCheckExprContext> {
        self.check(expr, &Expr::AnyType)
    }

    fn infer(
        &mut self,
        expr: &WithLocation<ast::Expr>,
    ) -> TypeInferResult {
        match &expr.value {
            ast::Expr::Error => TypeInferResult::error(),

            ast::Expr::As { value, value_type } => {
                let t = self.check_type(value_type);
                let e = self.check(value, &t);

                TypeInferResult::complete(e, t)
            }

            ast::Expr::BoolLiteral(value) => {
                TypeInferResult::complete(Expr::BoolLiteral(*value), Expr::bool_type())
            }
            ast::Expr::IntLiteral(value) => {
                TypeInferResult::complete(Expr::IntLiteral(value.clone()), Expr::int_type())
            }

            ast::Expr::Break { label: Some(_) } => todo!(),
            ast::Expr::Break { label: None } => TypeInferResult::complete(
                Expr::Break { label: None },
                Expr::never_type(),
            ),
            ast::Expr::Next { label: Some(_) } => todo!(),
            ast::Expr::Next { label: None } => TypeInferResult::complete(
                Expr::Next { label: None },
                Expr::never_type(),
            ),
            ast::Expr::Redo { label: Some(_) } => todo!(),
            ast::Expr::Redo { label: None } => TypeInferResult::complete(
                Expr::Redo { label: None },
                Expr::never_type(),
            ),

            ast::Expr::Builtin(_)
            | ast::Expr::FunctionCall { .. }
            | ast::Expr::Identifier(_)
            | ast::Expr::Type => {
                let call = self.process_call(expr);
                self.infer_call(call)
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
                    &expr.location,
                    a,
                    op_id,
                    &op.location,
                    b,
                );
                self.infer_call(call)
            }

            ast::Expr::UnaryOperation { op, a } => {
                let op_id = match op.value {
                    ast::UnaryOperator::Plus => ast::UnaryOperatorIdentifier::Plus,
                    ast::UnaryOperator::Minus => ast::UnaryOperatorIdentifier::Minus,
                    ast::UnaryOperator::BitNot => ast::UnaryOperatorIdentifier::BitNot,
                    ast::UnaryOperator::LogicalNot => ast::UnaryOperatorIdentifier::LogicalNot,
                };

                let call =
                    self.process_unary_operator(&expr.location, op_id, &op.location, a);
                self.infer_call(call)
            }

            ast::Expr::FunctionResultValue => {
                todo!()
            }

            ast::Expr::Paren(inner) => self.infer(inner),

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

                TypeInferResult::complete(str_expr, Expr::string_type())
            }
            ast::Expr::BigType(value) => {
                TypeInferResult::complete(Expr::BigType(value.clone()), Expr::BigType(value + 1))
            }
            ast::Expr::Assert { .. } => todo!("infer assert expressions"),
            ast::Expr::Block { body, finally_body } => {
                let TypeInferResult::Complete {
                    checked_expr: checked_body,
                    inferred_type,
                } = self.infer_block(body);
                let checked_finally_body = finally_body.as_ref().map(|finally_body| {
                    Box::new(self.check_block(finally_body, &Expr::unit_type()))
                });

                TypeInferResult::complete(
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
                let conv_condition = self.check(condition, &Expr::bool_type());
                let TypeInferResult::Complete {
                    checked_expr: conv_when_true,
                    inferred_type,
                } = self.infer_block(when_true);
                let conv_when_false = self.check_block(when_false, &inferred_type);

                let (when_true_var, when_false_var) = self.create_if_cond_vars(&conv_condition);

                TypeInferResult::complete(
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

    fn infer_block(
        &mut self,
        body: &WithLocation<Vec<WithLocation<ast::Stmt>>>,
    ) -> TypeInferResult {
        let Some((last_stmt, leading_stmts)) = body.value.split_last() else {
            return TypeInferResult::complete(Expr::unit_type(), Expr::unit_type());
        };

        let checked_stmts = leading_stmts
            .iter()
            .map(|stmt| self.check_stmt(stmt, &Expr::unit_type()))
            .collect::<Vec<_>>();
        let TypeInferResult::Complete {
            checked_expr,
            inferred_type,
        } = self.infer_stmt(last_stmt);

        let checked_stmts = match NEVec::try_from_vec(checked_stmts) {
            Some(mut checked_stmts) => {
                checked_stmts.push(checked_expr);
                checked_stmts
            }
            None => NEVec::new(checked_expr),
        };

        TypeInferResult::complete(Expr::Sequence(checked_stmts), inferred_type)
    }

    fn check_block(
        &mut self,
        body: &WithLocation<Vec<WithLocation<ast::Stmt>>>,
        expected_type: &Expr<TypeCheckExprContext>,
    ) -> Expr<TypeCheckExprContext> {
        let infer = self.infer_block(body);
        self.check_inferred_type(&body.location, infer, expected_type)
    }

    fn check_stmt(
        &mut self,
        stmt: &WithLocation<ast::Stmt>,
        expected_type: &Expr<TypeCheckExprContext>,
    ) -> Expr<TypeCheckExprContext> {
        let infer = self.infer_stmt(stmt);
        self.check_inferred_type(&stmt.location, infer, expected_type)
    }

    fn infer_stmt(
        &mut self,
        stmt: &WithLocation<ast::Stmt>,
    ) -> TypeInferResult {
        match &stmt.value {
            ast::Stmt::Expr(expr) => self.infer(expr),
            _ => todo!("inferring non-expression statements in blocks"),
        }
    }

    fn infer_call(
        &mut self,
        call: CallInfo<'_>,
    ) -> TypeInferResult {
        match call.callee {
            CalleeInfo::Error => TypeInferResult::error(),
            CalleeInfo::Builtin(builtin) => {
                self.infer_builtin(call.location, builtin, call.arguments)
            }
            CalleeInfo::Variable(v) => self.infer_variable(v, call.arguments),
            CalleeInfo::VariableTupleElement(vte) => {
                self.infer_variable_tuple_element(vte, call.arguments)
            }
            CalleeInfo::Overloadable(_) => todo!(),
            CalleeInfo::Expr(_) => todo!(),
            CalleeInfo::TypeN => {
                if !call.arguments.is_empty() {
                    todo!()
                }

                TypeInferResult::complete(Expr::type_n(0), Expr::type_n(1))
            }
        }
    }

    fn infer_builtin(
        &mut self,
        location: &Location,
        builtin: Builtin,
        args: VecDeque<ArgumentInfo<'_>>,
    ) -> TypeInferResult {
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

                TypeInferResult::complete(
                    Expr::Builtin {
                        builtin,
                        arguments: checked_args,
                    },
                    Expr::string_type(),
                )
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
            .map(|(arg, expected_type)| self.check(arg.arg, &expected_type))
            .collect();

        TypeInferResult::complete(
            Expr::Builtin {
                builtin,
                arguments: checked_args,
            },
            inferred_type,
        )
    }

    fn infer_parameterized_builtin<const REST_ARG_COUNT: usize>(
        &mut self,
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

        let TypeInferResult::Complete {
            checked_expr: element_type,
            inferred_type: element_type_type,
        } = self.infer(element_type_arg.arg);

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

        TypeInferResult::complete(
            Expr::Builtin {
                builtin,
                arguments: checked_args,
            },
            create_result_type(&element_type, element_type_type),
        )
    }

    fn infer_variable(
        &mut self,
        v: Variable<TypeCheckExprContext>,
        args: VecDeque<ArgumentInfo<'_>>,
    ) -> TypeInferResult {
        let t = v.var_type();
        let expr = Expr::Variable(v);
        self.infer_function_object_call(expr, t, args)
    }

    fn infer_variable_tuple_element(
        &mut self,
        vte: VariableTupleElement<TypeCheckExprContext>,
        args: VecDeque<ArgumentInfo<'_>>,
    ) -> TypeInferResult {
        let t = vte.binding_type;
        let expr = Expr::TupleElement(Box::new(Expr::Variable(vte.variable)), vte.index);
        self.infer_function_object_call(expr, t, args)
    }

    fn infer_function_object_call(
        &mut self,
        mut expr: Expr<TypeCheckExprContext>,
        mut t: Expr<TypeCheckExprContext>,
        args: VecDeque<ArgumentInfo<'_>>,
    ) -> TypeInferResult {
        for arg in args {
            if let Expr::FunctionType { a, r } = t {
                let arg = self.check(arg.arg, &a);
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

        TypeInferResult::complete(expr, t)
    }

    fn process_call<'b>(
        &mut self,
        mut func_expr: &'b WithLocation<ast::Expr>,
    ) -> CallInfo<'b> {
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
                    let callee = self.process_lookup(&func_expr.location, identifier);
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
            callee: self.process_lookup(op_location, &Identifier::BinaryOp(op)),
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
            callee: self.process_lookup(op_location, &Identifier::UnaryOp(op)),
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
    ) -> CalleeInfo<'b> {
        match self.scope.lookup(identifier) {
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
        let TypeInferResult::Complete {
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


struct OverloadResolver<'a, 'b> {
    type_checker: &'b mut TypeChecker<'a>,
    rejected_overloads: Vec<Overloadable>,
}

impl <'a, 'b> OverloadResolver<'a, 'b> {

    fn resolve_overload_lookup(mut self, call_location: &Location, overload_lookup: OverloadLookup, args: VecDequeSlice<'_, ArgumentInfo<'_>>) -> TypeInferResult {
        for group in overload_lookup.into_item_groups() {
            if let Some(result) = self.resolve_overload(call_location, group, args) {
                return result;
            }
        }

        todo!()
    }

    fn resolve_overload(&mut self, call_location: &Location, mut overloads: Vec<Overloadable>, args: VecDequeSlice<'_, ArgumentInfo<'_>>) -> Option<TypeInferResult> {
        if overloads.len() == 1 && let Some(overload) = overloads.pop() {
            return Some(self.attempt_single_overload(call_location, overload, args));
        }

        for group in self.group_overloads_by_arity(overloads, args) {
            if group.len().get() == 1 {
                let (overload, _) = group.into_nonempty_iter().next();
                return Some(self.attempt_single_overload(call_location, overload, args));
            }

            todo!("Implement overload resolution")
        }

        None
    }

    fn attempt_single_overload(&mut self, call_location: &Location, overload: Overloadable, args: VecDequeSlice<'_, ArgumentInfo<'_>>) -> TypeInferResult {
        todo!()
    }

    fn group_overloads_by_arity(&mut self, overloads: Vec<Overloadable>, args: VecDequeSlice<'_, ArgumentInfo<'_>>) -> impl Iterator<Item=NEVec<Overloadable>> + 'static {
        let mut groups: BTreeMap<OverloadArityRank, NEVec<Overloadable>> =
            BTreeMap::new();

        for overload in overloads {
            let Some(rank) = self.rank_by_arity(&overload, args) else {
                self.rejected_overloads.push(overload);
                continue;
            };

            match groups.entry(rank) {
                btree_map::Entry::Occupied(mut entry) => {
                    entry.get_mut().push(overload);
                }
                btree_map::Entry::Vacant(entry) => {
                    entry.insert(NEVec::new(overload));
                }
            }
        }

        groups.into_values()
    }

    fn rank_by_arity(&mut self, overload: &Overloadable, mut args: VecDequeSlice<'_, ArgumentInfo<'_>>) -> Option<OverloadArityRank> {
        let sig = self.get_overload_params(overload);

        let mut params = &sig.parameters[..];

        let mut num_inferred = 0;

        loop {
            let Some((param, tail_params)) = params.split_first() else {
                if args.is_empty() {
                    return Some(OverloadArityRank::Exact(num_inferred));
                }
                else {
                    return Some(OverloadArityRank::More(args.len()));
                }
            };

            let Some((arg, tail_args)) = args.split_first() else {
                return Some(OverloadArityRank::Less(params.len()));
            };

            if param.list_type == arg.list_type {
                args = tail_args;
                params = tail_params;
            }
            else if param.list_type == FunctionParameterListType::NormalList {
                return None;
            }
            else {
                params = tail_params;
                num_inferred += 1;
            }
        }
    }

    fn get_overload_params(
        &self,
        overload: &Overloadable,
    ) -> Arc<FunctionSignature<TypeCheckExprContext>> {
        todo!()
    }
}


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum OverloadArityRank {
    Exact(usize),
    More(usize),
    Less(usize),
}

impl Ord for OverloadArityRank {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        use OverloadArityRank::*;

        match (self, other) {
            (Exact(left), Exact(right))
            | (More(left), More(right))
            | (Less(left), Less(right)) => left.cmp(right),

            (Exact(_), _) => std::cmp::Ordering::Less,
            (_, Exact(_)) => std::cmp::Ordering::Greater,
            (More(_), Less(_)) => std::cmp::Ordering::Less,
            (Less(_), More(_)) => std::cmp::Ordering::Greater,
        }
    }
}

impl PartialOrd for OverloadArityRank {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

enum OverloadRejectionReason {
    ParameterListTypeMismatch,

}

