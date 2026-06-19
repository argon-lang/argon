use super::{ArgumentInfo, default_to_type_check_shifter, ExpectedType, Hole, InferredType, TypeCheckExprContext, TypeChecker, TypeInferResult};
use alloc::borrow::Cow;
use alloc::boxed::Box;
use alloc::collections::{BTreeMap, VecDeque};
use alloc::vec;
use alloc::vec::Vec;
use argon_compiler::scope::{self, LocalVariableScope, Scope};
use argon_compiler::signature::SignatureParameter;
use argon_expr::{EnumType, ErasureMode, Expr, ExprScannerMut, ExpressionOwner, MethodInstanceType, RecordType, SubstScanner, TraitType, Variable};
use argon_parser::ast::FunctionParameterListType;
use argon_util::{CompileError, MultiSlice};
use core::cmp::Ordering;
use alloc::sync::Arc;
use argon_compiler::{Function, FunctionSignature, Method, RecordField, SubstFunctionSignature};
use parse18_runtime::Location;



#[derive(Debug)]
pub(super) enum Overloadable<'a> {
    Base(scope::Overloadable),
    InstanceMethod {
        method: Arc<dyn Method>,
        trait_type: TraitType<TypeCheckExprContext>,
        obj: Expr<TypeCheckExprContext>,
    },
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
}

impl<'a> Overloadable<'a> {
    fn initial_arguments_info(&self) -> Vec<ArgumentInfo<'a>> {
        match self {
            Overloadable::Base(_) => vec![],
            Overloadable::InstanceMethod { .. } => vec![],
            Overloadable::ExtensionMethod(_, arg_info, _) => vec![arg_info.clone()],
            Overloadable::RecordField { .. } => vec![],
            Overloadable::RecordFieldStore { .. } => vec![],
        }
    }

    fn initial_arguments(&self) -> Vec<TypeInferResult<'static>> {
        match self {
            Overloadable::Base(_) => vec![],
            Overloadable::InstanceMethod { .. } => vec![],
            Overloadable::ExtensionMethod(_, _, arg) => {
                vec![TypeInferResult::Complete(arg.clone())]
            }
            Overloadable::RecordField { .. } => vec![],
            Overloadable::RecordFieldStore { .. } => vec![],
        }
    }

    pub(super) fn as_expression_owner(&self) -> Option<ExpressionOwner<TypeCheckExprContext>> {
        match self {
            Overloadable::Base(base) => Some(base.as_expression_owner()),
            Overloadable::InstanceMethod { method, .. } => {
                Some(ExpressionOwner::Method(method.clone()))
            }
            Overloadable::ExtensionMethod(f, _, _) => Some(ExpressionOwner::Function(f.clone())),
            Overloadable::RecordField { .. } => None,
            Overloadable::RecordFieldStore { .. } => None,
        }
    }

    fn signature(&self) -> FunctionSignature<TypeCheckExprContext> {
        match self {
            Overloadable::Base(base) => {
                let mut shifter = default_to_type_check_shifter();
                base.signature().as_ref().clone().shift(&mut shifter)
            }
            Overloadable::InstanceMethod {
                method, trait_type, ..
            } => {
                let trait_sig = trait_type
                    .trait_
                    .clone()
                    .signature()
                    .as_ref()
                    .clone()
                    .shift(&mut default_to_type_check_shifter());

                let mut sig = method
                    .clone()
                    .signature()
                    .as_ref()
                    .clone()
                    .shift(&mut default_to_type_check_shifter());
                let mut subst = SubstScanner::new();
                subst.add_function_parameter_substitutions(
                    ExpressionOwner::Trait(trait_type.trait_.clone()),
                    &trait_sig,
                    &trait_type.arguments,
                );
                sig.scan_mut(&mut subst);

                sig
            }
            Overloadable::ExtensionMethod(f, _, _) => {
                let mut shifter = default_to_type_check_shifter();
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
        }
    }
}

impl From<scope::Overloadable> for Overloadable<'_> {
    fn from(value: scope::Overloadable) -> Self {
        Overloadable::Base(value)
    }
}

pub(super) struct OverloadResolver<'parent, 'access, 'scope, 'model, 'e> {
    type_checker: &'parent mut TypeChecker<'access, 'scope, 'model>,
    pub(super) call_location: &'e Location,
    rejected_overloads: Vec<(Overloadable<'e>, OverloadRejectionReason)>,
}

impl<'parent, 'access, 'scope, 'model, 'e> OverloadResolver<'parent, 'access, 'scope, 'model, 'e> {
    pub(super) fn new(
        type_checker: &'parent mut TypeChecker<'access, 'scope, 'model>,
        call_location: &'e Location,
    ) -> Self {
        Self {
            type_checker,
            call_location,
            rejected_overloads: Vec::new(),
        }
    }

    pub(super) fn resolve_overload_lookup(
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
        let mut scope = LocalVariableScope::new(
            &*self.type_checker.scope as &dyn Scope<ExprContext = TypeCheckExprContext>,
        );
        let mut type_checker = TypeChecker {
            context: self.type_checker.context.clone(),
            model: &mut model,
            access: self.type_checker.access,
            scope: &mut scope,
            erasure_check_mode: self.type_checker.erasure_check_mode,
        };

        let mut return_type = sig.return_type.clone();

        let mut params = VecDeque::from(sig.parameters);
        let mut parameter_index = 0;

        args.push_front_vec(overload.initial_arguments_info());
        inferred_args.push_front_vec(overload.initial_arguments());

        'processed_all_args: loop {
            let Some(param) = params.pop_front() else {
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

            'processed_arg: {
                'list_type_mismatch: {
                    if let (Some(arg), Some(inferred_arg)) = (args.first(), inferred_args.first()) {
                        if param.list_type != arg.list_type {
                            break 'list_type_mismatch;
                        }

                        if !type_checker.partially_inferred_type_matches_expected(
                            true,
                            &inferred_arg.partially_inferred_type(),
                            ExpectedType::Exact(&param.param_type),
                        ) {
                            return Some(OverloadRejectionReason::ParameterTypeMismatch {
                                parameter_index,
                            });
                        }

                        let mut inferred_arg = Cow::Borrowed(inferred_arg);

                        if param.erasure_mode == ErasureMode::Token && self.type_checker.is_type(param.param_type.clone()) {
                            match inferred_arg.as_ref() {
                                TypeInferResult::Complete(inferred) if !self.type_checker.treat_as_token(&inferred.checked_expr) => {
                                    let mut inferred = inferred.clone();

                                    inferred.checked_expr = Expr::BoxedType(Box::new(inferred.checked_expr));

                                    inferred_arg = Cow::Owned(TypeInferResult::Complete(inferred));
                                }

                                _ => {}
                            }
                        }

                        if let Some(v) = v {
                            self.substitute_inferred_arg_in_param_types(
                                &mut params,
                                &mut return_type,
                                v,
                                &inferred_arg,
                            );
                        }

                        args.pop_front();
                        inferred_args.pop_front();
                        break 'processed_arg;
                    } else if param.list_type == FunctionParameterListType::NormalList {
                        break 'processed_all_args;
                    }
                }

                match param.list_type {
                    FunctionParameterListType::NormalList => {
                        return Some(OverloadRejectionReason::ParameterListTypeMismatch);
                    }
                    FunctionParameterListType::InferrableList(_) => {
                        let hole = Hole::new(self.call_location.clone(), param.param_type.clone());
                        if let Some(v) = v {
                            substitute_arg_in_param_types(
                                &mut params,
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
        }

        None
    }

    fn substitute_inferred_arg_in_param_types(
        &self,
        params: &mut VecDeque<SignatureParameter<TypeCheckExprContext>>,
        return_type: &mut Expr<TypeCheckExprContext>,
        v: Variable<TypeCheckExprContext>,
        arg: &TypeInferResult<'e>,
    ) {
        if let TypeInferResult::Complete(inferred_type) = arg {
            substitute_arg_in_param_types(params, return_type, v, &inferred_type.checked_expr);
        } else {
            let hole = Hole::new(self.call_location.clone(), v.var_type().clone());
            substitute_arg_in_param_types(params, return_type, v, &Expr::Hole(hole));
        }
    }

    fn select_overload(
        &mut self,
        overload: Overloadable<'e>,
        args: &mut VecDeque<ArgumentInfo<'e>>,
        inferred_args: &mut VecDeque<TypeInferResult<'e>>,
    ) -> SelectedOverload<'e> {
        let sig = overload.signature();

        let mut return_type = sig.return_type;

        let mut params = VecDeque::from(sig.parameters);
        let mut parameter_index = 0;

        for initial_arg in overload.initial_arguments_info().into_iter().rev() {
            args.push_front(initial_arg);
        }

        for initial_arg in overload.initial_arguments().into_iter().rev() {
            inferred_args.push_front(initial_arg);
        }

        let mut selected_args = Vec::with_capacity(inferred_args.len());

        'processed_all_args: loop {
            let Some(param) = params.pop_front() else {
                break;
            };

            let v: Option<Variable<TypeCheckExprContext>> =
                overload.as_expression_owner().map(|owner| {
                    let param_var = param.clone().to_parameter_var(owner, parameter_index);
                    Variable::Parameter(Box::new(param_var))
                });

            'processed_arg: {
                'list_type_mismatch: {
                    if let Some(arg) = args.front() {
                        if param.list_type != arg.list_type {
                            break 'list_type_mismatch;
                        }

                        let arg_result = inferred_args
                            .pop_front()
                            .expect("inferred_args should not be empty when args is not empty");

                        let mut arg_expr = self.type_checker.check_inferred_type(
                            &arg.arg.location,
                            arg_result,
                            ExpectedType::Exact(&param.param_type),
                        );

                        if
                            param.erasure_mode == ErasureMode::Token &&
                            self.type_checker.is_type(param.param_type.clone()) &&
                            !self.type_checker.treat_as_token(&arg_expr.checked_expr)
                        {
                            arg_expr.checked_expr = Expr::BoxedType(Box::new(arg_expr.checked_expr));
                        }

                        if let Some(v) = v {
                            substitute_arg_in_param_types(
                                &mut params,
                                &mut return_type,
                                v,
                                &arg_expr.checked_expr,
                            );
                        }

                        selected_args.push(arg_expr.checked_expr);

                        args.pop_front();
                        break 'processed_arg;
                    } else if param.list_type == FunctionParameterListType::NormalList {
                        params.push_front(param);
                        break 'processed_all_args;
                    }
                }

                match param.list_type {
                    FunctionParameterListType::NormalList => {
                        unreachable!(
                            "Parameter list type mismatch should have been caught earlier"
                        );
                    }
                    FunctionParameterListType::InferrableList(_) => {
                        let hole = Hole::new(self.call_location.clone(), param.param_type);

                        if let Some(v) = v {
                            substitute_arg_in_param_types(
                                &mut params,
                                &mut return_type,
                                v,
                                &Expr::Hole(hole.clone()),
                            );
                        }

                        selected_args.push(Expr::Hole(hole));
                    }
                    FunctionParameterListType::RequiresList => {
                        todo!("implicit resolution")
                    }
                }
            }

            parameter_index += 1;
        }

        SelectedOverload {
            call_location: self.call_location,
            overload,

            args: selected_args,
            return_type,
            unspecified_parameters: params,
        }
    }
}

pub(super) fn substitute_arg_in_param_types(
    params: &mut VecDeque<SignatureParameter<TypeCheckExprContext>>,
    return_type: &mut Expr<TypeCheckExprContext>,
    v: Variable<TypeCheckExprContext>,
    arg: &Expr<TypeCheckExprContext>,
) {
    let mut scanner = SubstScanner::new();
    scanner.add_substitution(v, Cow::Borrowed(arg));

    for param in params.iter_mut() {
        scanner.scan(&mut param.param_type);
    }

    scanner.scan(return_type);
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

pub(super) struct ResolvedOverload<'e> {
    pub(super) overload: Option<SelectedOverload<'e>>,
    pub(super) extra_argument_info: VecDeque<ArgumentInfo<'e>>,
    pub(super) extra_inferred_args: VecDeque<TypeInferResult<'e>>,
}

pub(super) struct SelectedOverload<'e> {
    pub(super) call_location: &'e Location,
    pub(super) overload: Overloadable<'e>,
    pub(super) args: Vec<Expr<TypeCheckExprContext>>,
    pub(super) return_type: Expr<TypeCheckExprContext>,
    pub(super) unspecified_parameters: VecDeque<SignatureParameter<TypeCheckExprContext>>,
}

impl<'a> SelectedOverload<'a> {
    pub(super) fn into_inferred_type(mut self, checker: &TypeChecker<'_, '_, '_>) -> InferredType {
        let expr = match self.overload {
            Overloadable::Base(scope::Overloadable::Function(f)) => Expr::FunctionCall {
                function: f,
                arguments: self.args,
            },
            Overloadable::Base(scope::Overloadable::Record(r)) => Expr::RecordType(RecordType {
                record: r,
                arguments: self.args,
            }),
            Overloadable::Base(scope::Overloadable::Enum(e)) => Expr::EnumType(EnumType {
                enum_: e,
                arguments: self.args,
            }),
            Overloadable::Base(scope::Overloadable::Trait(t)) => Expr::TraitType(TraitType {
                trait_: t,
                arguments: self.args,
            }),
            Overloadable::Base(scope::Overloadable::EnumVariant(v)) => {
                if !v.clone().fields().is_empty() {
                    checker
                        .context
                        .reporter()
                        .report_error(CompileError::invalid_overload(self.call_location.clone()));
                }

                let enum_type = match &self.return_type {
                    Expr::Error => return InferredType::error(),
                    Expr::EnumType(e) => e.clone(),
                    _ => panic!("Expected enum type"),
                };

                Expr::EnumVariantLiteral {
                    enum_type,
                    variant: v,
                    arguments: self.args,
                    fields: vec![],
                }
            }
            Overloadable::Base(scope::Overloadable::Instance(i)) => Expr::NewInstance {
                instance: i.clone(),
                arguments: self.args,
            },
            Overloadable::InstanceMethod {
                method,
                trait_type,
                obj,
            } => Expr::MethodCall {
                method,
                instance_type: MethodInstanceType::Trait(trait_type),
                receiver: Box::new(obj),
                arguments: self.args,
            },
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
        };

        InferredType {
            checked_expr: expr,
            inferred_type: self.return_type,
        }
    }
}
