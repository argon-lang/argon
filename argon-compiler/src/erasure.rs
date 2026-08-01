use crate::expr_type::ExprTypeContext;
use crate::scanner::PurityScanner;
use crate::shifter::DefaultToExprTypeContextShifter;
use crate::{Context, FunctionSignature};
use argon_expr::{
    Builtin, ErasureMode, Expr, ExprContext, ExprScanner, LocatedExpr, TypeComparer, default_scan,
};
use argon_util::CompileError;
use parse18_runtime::Location;

pub struct ErasureScanner<'a, Cmp> {
    pub context: Context,
    pub comparer: &'a mut Cmp,
    pub expected_erasure: ErasureMode,
    pub location: &'a Location,
    pub suppress_errors: bool,
}

impl<'a, Cmp> ExprScanner for ErasureScanner<'a, Cmp>
where
    Cmp: TypeComparer,
    Cmp::EC: ExprTypeContext,
{
    type EC = Cmp::EC;

    fn scan(&mut self, expr: &LocatedExpr<Self::EC>) -> bool {
        let old_suppress_errors = self.suppress_errors;

        match self.expected_erasure {
            ErasureMode::Erased => {
                let mut purity = PurityScanner::new();
                purity.scan(expr);
                if purity.found_impure() {
                    self.context
                        .reporter()
                        .report_error(CompileError::purity_error(self.location.clone()));
                }
            }
            ErasureMode::Token | ErasureMode::Concrete => match &expr.value {
                Expr::Error => {}

                Expr::Hole(_) | Expr::FunctionResultValue { .. } => self.erased_required(),

                Expr::Builtin(
                    Builtin::IntType { .. }
                    | Builtin::BoolType
                    | Builtin::StringType
                    | Builtin::NeverType,
                ) => {}

                Expr::Builtin(Builtin::ArrayType { element_type }) => {
                    self.scan_token(element_type);
                }

                Expr::Builtin(Builtin::EqualToRefl { r#type, value }) => {
                    self.erased_required();
                    self.scan_erased(r#type);
                    self.scan_erased(value);
                }

                Expr::Builtin(Builtin::UnsafeAssumeErased { r#type }) => {
                    self.erased_required();
                    self.scan_erased(r#type);
                }

                Expr::BindEnsures { value, variables } => {
                    self.scan(value);
                    for variable in variables {
                        self.scan_erased(&variable.var_type);
                    }
                }

                Expr::BindErasedAlias {
                    variable,
                    equality_witness,
                    value,
                } => {
                    self.scan_erased(&variable.var_type);
                    if let Some(equality_witness) = equality_witness {
                        self.scan_erased(&equality_witness.var_type);
                    }
                    self.scan(value);
                }

                Expr::EqualToType { .. }
                | Expr::ConjunctionType { .. }
                | Expr::DisjunctionType { .. } => {
                    todo!()
                }

                Expr::Closure {
                    v,
                    body,
                    return_type,
                } => {
                    self.prohibit_for_token();
                    self.scan_token(&v.var_type);
                    self.scan_token(return_type);
                    self.scan(body);
                }

                Expr::Condition {
                    value,
                    when_true_witness,
                    when_false_witness,
                } => {
                    self.scan(value);

                    if let Some(witness) = when_true_witness {
                        self.scan_erased(&witness.var_type)
                    }

                    if let Some(witness) = when_false_witness {
                        self.scan_erased(&witness.var_type)
                    }
                }

                Expr::EnumType(enum_type) => {
                    let sig = enum_type.enum_.clone().signature();
                    self.scan_arguments(&sig, &enum_type.arguments)
                }

                Expr::FunctionCall {
                    function,
                    arguments,
                } => match function.metadata().erasure_mode {
                    ErasureMode::Erased => self.erased_required(),
                    ErasureMode::Token => {
                        let sig = function.clone().signature();
                        self.scan_arguments(&sig, arguments)
                    }
                    ErasureMode::Concrete => {
                        self.prohibit_for_token();
                        let sig = function.clone().signature();
                        self.scan_arguments(&sig, arguments)
                    }
                },

                Expr::FunctionType { a, r } => {
                    self.scan_token(&a.var_type);
                    self.scan_token(r);
                }

                Expr::MethodCall {
                    method,
                    instance_type,
                    receiver,
                    arguments,
                } => {
                    if method.metadata().erasure_mode == ErasureMode::Erased {
                        self.erased_required();
                    }

                    self.prohibit_for_token();
                    self.scan(receiver);

                    let mut sig = method
                        .clone()
                        .signature()
                        .as_ref()
                        .clone()
                        .shift(&mut DefaultToExprTypeContextShifter::<Cmp::EC>::default());
                    sig.substitute_method_instance_type_parameters(instance_type);
                    self.scan_arguments(&sig, arguments);
                }

                Expr::NewInstance {
                    instance,
                    arguments,
                } => {
                    if instance.erasure_mode() != ErasureMode::Token {
                        self.prohibit_for_token();
                    }

                    let sig = instance.clone().signature();
                    self.scan_arguments(&sig, arguments);
                }

                Expr::RecordType(record_type) => {
                    let sig = record_type.record.clone().signature();
                    self.scan_arguments(&sig, &record_type.arguments);
                }

                Expr::TraitType(trait_type) => {
                    let sig = trait_type.trait_.clone().signature();
                    self.scan_arguments(&sig, &trait_type.arguments);
                }

                Expr::Tuple { .. } => {
                    default_scan(self, expr);
                }

                Expr::Type(level) => self.scan_erased(level),

                Expr::BigType(_) => {}

                Expr::VariableBinding(v, value) => match v.erasure_mode {
                    ErasureMode::Erased => {
                        self.scan_erased(&v.var_type);
                        self.scan_erased(value);
                    }
                    ErasureMode::Token => {
                        self.prohibit_for_token();
                    }
                    ErasureMode::Concrete => {
                        self.scan_token(&v.var_type);
                        self.scan(value);
                    }
                },

                Expr::Variable(v) => match v.erasure_mode() {
                    ErasureMode::Erased => self.erased_required(),
                    ErasureMode::Token => {}
                    ErasureMode::Concrete => self.prohibit_for_token(),
                },

                Expr::Use { inner: value, .. }
                | Expr::Shared { inner: value }
                | Expr::Share { value }
                | Expr::Borrow { value }
                | Expr::BorrowMut { value } => {
                    self.prohibit_for_token();
                    self.scan(value);
                }

                Expr::BoxedType(t) => self.scan_erased(t),

                Expr::Box { t, value } => {
                    self.prohibit_for_token();
                    self.scan_erased(t);
                    self.scan(value);
                }

                Expr::Unbox { t, value } => {
                    self.prohibit_for_token();
                    self.scan_erased(t);
                    self.scan(value);
                }

                _ => {
                    self.prohibit_for_token();
                    default_scan(self, expr);
                }
            },
        }

        self.suppress_errors = old_suppress_errors;
        true
    }
}

impl<'a, Cmp> ErasureScanner<'a, Cmp>
where
    Cmp: TypeComparer,
    Cmp::EC: ExprTypeContext,
{
    fn scan_token<T>(&mut self, expr: T)
    where
        T: AsLocatedExpr<Cmp::EC>,
    {
        let old_erasure = self.expected_erasure;
        self.expected_erasure = ErasureMode::Token;
        self.scan(expr.as_located_expr());
        self.expected_erasure = old_erasure;
    }

    fn scan_erased<T>(&mut self, expr: T)
    where
        T: AsLocatedExpr<Cmp::EC>,
    {
        let old_erasure = self.expected_erasure;
        self.expected_erasure = ErasureMode::Erased;
        self.scan(expr.as_located_expr());
        self.expected_erasure = old_erasure;
    }

    fn prohibit_for_token(&mut self) {
        if !self.suppress_errors && matches!(self.expected_erasure, ErasureMode::Token) {
            self.suppress_errors = true;
            self.context
                .reporter()
                .report_error(CompileError::token_expression_required(
                    self.location.clone(),
                ));
        }
    }

    fn erased_required(&mut self) {
        if !self.suppress_errors && !matches!(self.expected_erasure, ErasureMode::Erased) {
            self.suppress_errors = true;
            self.context
                .reporter()
                .report_error(CompileError::erased_expression_not_allowed(
                    self.location.clone(),
                ));
        }
    }

    fn scan_arguments<SigEC: ExprContext + ?Sized>(
        &mut self,
        sig: &FunctionSignature<SigEC>,
        arguments: &[LocatedExpr<Cmp::EC>],
    ) {
        let old_erasure = self.expected_erasure;

        for (arg, param) in arguments.iter().zip(sig.parameters.iter()) {
            self.expected_erasure = param.erasure_mode;
            self.scan(arg);
        }

        self.expected_erasure = old_erasure;
    }
}

trait AsLocatedExpr<EC: ExprContext + ?Sized> {
    fn as_located_expr(&self) -> &LocatedExpr<EC>;
}

impl<EC: ExprContext + ?Sized> AsLocatedExpr<EC> for LocatedExpr<EC> {
    fn as_located_expr(&self) -> &LocatedExpr<EC> {
        self
    }
}

impl<EC: ExprContext + ?Sized> AsLocatedExpr<EC> for Box<LocatedExpr<EC>> {
    fn as_located_expr(&self) -> &LocatedExpr<EC> {
        self
    }
}

impl<EC: ExprContext + ?Sized, T: AsLocatedExpr<EC> + ?Sized> AsLocatedExpr<EC> for &T {
    fn as_located_expr(&self) -> &LocatedExpr<EC> {
        (*self).as_located_expr()
    }
}
