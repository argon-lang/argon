use crate::type_checker::{ExprNormalizer, TypeCheckExprContext, TypeChecker};
use argon_expr::ownership::{is_shared_type, is_use_mutable_type, is_use_type};
use argon_expr::{Builtin, Expr, FullNormalizer, LocatedExpr, LocatedPattern, MatchCase, Pattern, RecordFieldLiteral, RecordFieldPattern, Variable};
use argon_util::CompileError;
use hashbrown::{HashMap, HashSet};
use parse18_runtime::Location;



// Ownership Tracking algorithm
// - Any use of a variable not directly in Borrow, BorrowMut, or Share is considered a move, unless the variable has a shared type.
// - Any use of a variable within a nested closure is considered to be shared at that point.
//    - Any future assignments to such referenced variables are also considered shared.
// - Any borrows passed to a function are considered to be released once the function returns.
pub(super) fn check_ownership(tc: &mut TypeChecker, e: &LocatedExpr<TypeCheckExprContext>) {
    OwnershipChecker::new(tc).check(e);
}

#[derive(Clone, Copy, Default)]
struct VariableState {
    moved: bool,
    shared: bool,
    borrows: usize,
    mutable_borrows: usize,
}

#[derive(Clone, Copy)]
struct BorrowSnapshot {
    borrows: usize,
    mutable_borrows: usize,
}

struct OwnershipChecker<'a, 'access, 'scope, 'model> {
    tc: &'a mut TypeChecker<'access, 'scope, 'model>,
    states: HashMap<Variable<TypeCheckExprContext>, VariableState>,
    scopes: Vec<HashSet<Variable<TypeCheckExprContext>>>,
    closure_boundaries: Vec<usize>,
}

impl<'a, 'access, 'scope, 'model> OwnershipChecker<'a, 'access, 'scope, 'model> {
    fn new(tc: &'a mut TypeChecker<'access, 'scope, 'model>) -> Self {
        Self {
            tc,
            states: HashMap::new(),
            scopes: vec![HashSet::new()],
            closure_boundaries: vec![0],
        }
    }

    fn check(&mut self, expr: &LocatedExpr<TypeCheckExprContext>) {
        self.scan(expr);
    }

    fn report(&self, error: CompileError) {
        self.tc.context.reporter().report_error(error);
    }

    fn state_mut(&mut self, variable: &Variable<TypeCheckExprContext>) -> &mut VariableState {
        self.states.entry(variable.clone()).or_default()
    }

    fn is_shared(&self, variable: &Variable<TypeCheckExprContext>) -> bool {
        is_shared_type(variable.var_type())
            || self.states.get(variable).is_some_and(|state| state.shared)
    }

    fn is_current_closure_local(&self, variable: &Variable<TypeCheckExprContext>) -> bool {
        let boundary = *self.closure_boundaries.last().unwrap_or(&0);
        self.scopes[boundary..]
            .iter()
            .any(|scope| scope.contains(variable))
    }

    fn add_local(&mut self, variable: Variable<TypeCheckExprContext>) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(variable);
        }
    }

    fn use_variable(&mut self, variable: &Variable<TypeCheckExprContext>, location: &Location) {
        let mut norm_type = variable.var_type().clone();

        let mut norm = FullNormalizer::new(
            self.tc.context.normalize_fuel(),
            ExprNormalizer { model: &mut self.tc.model, }
        );
        norm.normalize(&mut norm_type);

        if is_shared_type(&norm_type) {
            self.share_variable(variable, location);
            return;
        }
        else if is_use_type(&norm_type) {
            self.borrow_variable(variable, location, false);
            return;
        }
        else if is_use_mutable_type(&norm_type) {
            self.borrow_variable(variable, location, true);
            return;
        }

        if self.closure_boundaries.len() > 1 && !self.is_current_closure_local(variable) {
            self.state_mut(variable).shared = true;
            return;
        }

        if self.is_shared(variable) {
            return;
        }

        let state = self.state_mut(variable);
        if state.moved {
            self.report(CompileError::use_after_move(location.clone()));
        } else if state.borrows > 0 || state.mutable_borrows > 0 {
            self.report(CompileError::move_while_borrowed(location.clone()));
        }

        self.state_mut(variable).moved = true;
    }

    fn borrow_variable(&mut self, variable: &Variable<TypeCheckExprContext>, location: &Location, is_mutable: bool) {
        let state = self.state_mut(variable);
        if state.moved {
            self.report(CompileError::borrow_after_move(location.clone()));
        } else if is_mutable && (state.borrows > 0 || state.mutable_borrows > 0)
            || !is_mutable && state.mutable_borrows > 0
        {
            self.report(CompileError::mutable_borrow_conflict(
                location.clone(),
            ));
        }

        let state = self.state_mut(variable);
        if is_mutable {
            state.mutable_borrows += 1;
        } else {
            state.borrows += 1;
        }
    }

    fn share_variable(&mut self, variable: &Variable<TypeCheckExprContext>, location: &Location) {
        let state = self.state_mut(variable);
        if state.moved {
            self.report(CompileError::use_after_move(location.clone()));
        }

        self.state_mut(variable).shared = true;
    }

    fn assign_variable(&mut self, variable: &Variable<TypeCheckExprContext>, location: &Location, value: &LocatedExpr<TypeCheckExprContext>) {
        if self.is_shared(variable) {
            let value = LocatedExpr {
                location: value.location.clone(),
                value: Expr::Share {
                    value: Box::new(value.clone()),
                },
            };

            self.scan(&value);
        }
        else {
            self.scan(value);
        }

        let state = self.state_mut(variable);
        if state.borrows > 0 || state.mutable_borrows > 0 {
            self.report(CompileError::mutable_borrow_conflict(
                location.clone(),
            ));
            return;
        }

        self.state_mut(variable).moved = false;
    }

    fn scan_call_arguments<'b>(
        &mut self,
        values: impl IntoIterator<Item = &'b LocatedExpr<TypeCheckExprContext>>,
    ) {
        let borrow_snapshot = self
            .states
            .iter()
            .map(|(variable, state)| {
                (
                    variable.clone(),
                    BorrowSnapshot {
                        borrows: state.borrows,
                        mutable_borrows: state.mutable_borrows,
                    },
                )
            })
            .collect::<HashMap<_, _>>();

        for value in values {
            self.scan(value);
        }

        for (variable, state) in &mut self.states {
            if let Some(snapshot) = borrow_snapshot.get(variable) {
                state.borrows = snapshot.borrows;
                state.mutable_borrows = snapshot.mutable_borrows;
            } else {
                state.borrows = 0;
                state.mutable_borrows = 0;
            }
        }
    }

    fn scan_branch(
        &mut self,
        expr: &LocatedExpr<TypeCheckExprContext>,
    ) -> HashMap<Variable<TypeCheckExprContext>, VariableState> {
        let original_states = self.states.clone();
        self.states = original_states.clone();
        self.scopes.push(HashSet::new());
        self.scan(expr);
        self.scopes.pop();
        let branch_states = self.states.clone();
        self.states = original_states;
        branch_states
    }

    fn merge_branches(
        &mut self,
        branches: impl IntoIterator<Item = HashMap<Variable<TypeCheckExprContext>, VariableState>>,
    ) {
        for branch in branches {
            for (variable, branch_state) in branch {
                let state = self.states.entry(variable).or_default();
                state.moved |= branch_state.moved;
                state.shared |= branch_state.shared;
                state.borrows = state.borrows.max(branch_state.borrows);
                state.mutable_borrows = state.mutable_borrows.max(branch_state.mutable_borrows);
            }
        }
    }

    fn scan(&mut self, expr: &LocatedExpr<TypeCheckExprContext>) {
        match &expr.value {
            Expr::Error
            | Expr::BoolLiteral(_)
            | Expr::IntLiteral(_)
            | Expr::I8Literal(_)
            | Expr::U8Literal(_)
            | Expr::I16Literal(_)
            | Expr::U16Literal(_)
            | Expr::I32Literal(_)
            | Expr::U32Literal(_)
            | Expr::I64Literal(_)
            | Expr::U64Literal(_)
            | Expr::StringLiteral(_)
            | Expr::BigType(_) => {}
            Expr::Hole(hole) => {
                let _ = hole;
            }
            Expr::And(a, b)
            | Expr::Or(a, b)
            | Expr::ConjunctionType { lhs: a, rhs: b }
            | Expr::DisjunctionType { lhs: a, rhs: b } => {
                self.scan(a);
                self.scan(b);
            }
            Expr::Finally {
                block_body,
                finally_body,
            } => {
                self.scan(block_body);
                self.scan(finally_body);
            }
            Expr::Block { label, body } => {
                self.scan(&label.block_result_type);
                self.scopes.push(HashSet::new());
                self.scan(body);
                self.scopes.pop();
            }
            Expr::Break { label, value } => {
                self.scan(&label.block_result_type);
                self.scan(value);
            }
            Expr::Builtin(builtin) => self.scan_builtin(builtin),
            Expr::BindEnsures { value, variables } => {
                self.scan(value);
                for variable in variables {
                    self.scan(&variable.var_type);
                    self.add_local(Variable::Local(variable.clone()));
                }
            }
            Expr::BindErasedAlias {
                variable,
                equality_witness,
                value,
            } => {
                self.scan(&variable.var_type);
                if let Some(witness) = equality_witness {
                    self.scan(&witness.var_type);
                }
                self.scan(value);
                self.add_local(Variable::Local(variable.clone()));
                if let Some(witness) = equality_witness {
                    self.add_local(Variable::Local(witness.clone()));
                }
            }
            Expr::EqualToType { r#type, lhs, rhs } => {
                self.scan(r#type);
                self.scan(lhs);
                self.scan(rhs);
            }
            Expr::Closure {
                v,
                return_type,
                body,
            } => {
                self.scan(&v.var_type);
                self.scan(return_type);
                self.scopes
                    .push(HashSet::from([Variable::ClosureParameter(v.clone())]));
                self.closure_boundaries.push(self.scopes.len() - 1);
                self.scan(body);
                self.closure_boundaries.pop();
                self.scopes.pop();
            }
            Expr::Condition {
                value,
                when_true_witness,
                when_false_witness,
            } => {
                self.scan(value);
                if let Some(variable) = when_true_witness {
                    self.scan(&variable.var_type);
                }
                if let Some(variable) = when_false_witness {
                    self.scan(&variable.var_type);
                }
            }
            Expr::EnumType(enum_type) => {
                for argument in &enum_type.arguments {
                    self.scan(argument);
                }
            }
            Expr::EnumVariantLiteral {
                enum_type,
                arguments,
                fields,
                ..
            } => {
                for argument in &enum_type.arguments {
                    self.scan(argument);
                }
                self.scan_call_arguments(
                    arguments
                        .iter()
                        .chain(fields.iter().map(|field| &field.value)),
                );
            }
            Expr::FunctionCall { arguments, .. } | Expr::NewInstance { arguments, .. } => {
                self.scan_call_arguments(arguments);
            }
            Expr::FunctionObjectCall { function, argument } => {
                self.scan(function);
                self.scan_call_arguments([argument.as_ref()]);
            }
            Expr::FunctionResultValue { result_type } => self.scan(result_type),
            Expr::FunctionType { a, r } => {
                self.scan(&a.var_type);
                self.scan(r);
            }
            Expr::IfElse {
                condition,
                when_true,
                when_false,
            } => {
                self.scan(condition);
                let true_states = self.scan_branch(when_true);
                let false_states = self.scan_branch(when_false);
                self.merge_branches([true_states, false_states]);
            }
            Expr::InstanceType(instance_type) => {
                for argument in &instance_type.arguments {
                    self.scan(argument);
                }
            }
            Expr::Is { value, pattern } => {
                self.scan(value);
                self.scan_pattern(pattern);
            }
            Expr::Match { value, cases } => {
                self.scan(value);
                let branches = cases
                    .iter()
                    .map(|case| self.scan_match_case(case))
                    .collect::<Vec<_>>();
                self.merge_branches(branches);
            }
            Expr::MethodCall {
                instance_type,
                receiver,
                arguments,
                ..
            } => {
                match instance_type {
                    argon_expr::MethodInstanceType::Trait(trait_type) => {
                        for argument in &trait_type.arguments {
                            self.scan(argument);
                        }
                    }
                }
                self.scan(receiver);
                self.scan_call_arguments(arguments);
            }
            Expr::Not(value) | Expr::Raise { ex: value } | Expr::TupleElement(value, _) => {
                self.scan(value);
            }
            Expr::RecordFieldLoad {
                record_type,
                record_value,
                ..
            } => {
                for argument in &record_type.arguments {
                    self.scan(argument);
                }
                self.scan(record_value);
            }
            Expr::RecordFieldStore {
                record_type,
                record_value,
                new_value,
                ..
            } => {
                for argument in &record_type.arguments {
                    self.scan(argument);
                }
                self.scan(record_value);
                self.scan(new_value);
            }
            Expr::RecordLiteral {
                record_type,
                fields,
            } => {
                for argument in &record_type.arguments {
                    self.scan(argument);
                }
                for RecordFieldLiteral { value, .. } in fields {
                    self.scan(value);
                }
            }
            Expr::Use { inner, .. } | Expr::Shared { inner } => self.scan(inner),
            Expr::Share { value } => {
                if let Expr::Variable(variable) = &value.value {
                    self.share_variable(variable, &value.location);
                } else {
                    self.scan(value);
                }
            }
            Expr::Borrow { value } => {
                if let Expr::Variable(variable) = &value.value {
                    self.borrow_variable(variable, &value.location, false);
                } else {
                    self.scan(value);
                }
            }
            Expr::BorrowMut { value } => {
                if let Expr::Variable(variable) = &value.value {
                    self.borrow_variable(variable, &value.location, true);
                } else {
                    self.scan(value);
                }
            }
            Expr::RecordType(record_type) => {
                for argument in &record_type.arguments {
                    self.scan(argument);
                }
            }
            Expr::Retry { label } => self.scan(&label.block_result_type),
            Expr::Sequence(exprs) => {
                for expr in exprs {
                    self.scan(expr);
                }
            }
            Expr::TraitType(trait_type) => {
                for argument in &trait_type.arguments {
                    self.scan(argument);
                }
            }
            Expr::Tuple { items } => {
                for item in items {
                    self.scan(item);
                }
            }
            Expr::Type(t) | Expr::BoxedType(t) => self.scan(t),
            Expr::Variable(variable) => self.use_variable(variable, &expr.location),
            Expr::VariableBinding(variable, value) => {
                self.scan(&variable.var_type);
                self.scan(value);
                self.add_local(Variable::Local(variable.clone()));
            }
            Expr::VariableStore(variable, value) => {
                self.assign_variable(variable, &expr.location, &**value);
            }
            Expr::Box { t, value } | Expr::Unbox { t, value } => {
                self.scan(t);
                self.scan(value);
            }
        }
    }

    fn scan_builtin(&mut self, builtin: &Builtin<TypeCheckExprContext>) {
        match builtin {
            Builtin::IntType { .. }
            | Builtin::BoolType
            | Builtin::StringType
            | Builtin::NeverType => {}
            Builtin::ArrayType { element_type }
            | Builtin::UnsafeAssumeErased {
                r#type: element_type,
            } => {
                self.scan(element_type);
            }
            Builtin::IntNegate { value, .. }
            | Builtin::IntBitNot { value, .. }
            | Builtin::IntConvert { value, .. } => self.scan(value),
            Builtin::IntAdd { lhs, rhs, .. }
            | Builtin::IntSub { lhs, rhs, .. }
            | Builtin::IntMul { lhs, rhs, .. }
            | Builtin::IntBitAnd { lhs, rhs, .. }
            | Builtin::IntBitOr { lhs, rhs, .. }
            | Builtin::IntBitXor { lhs, rhs, .. }
            | Builtin::IntBitShiftLeft { lhs, rhs, .. }
            | Builtin::IntBitShiftRight { lhs, rhs, .. }
            | Builtin::IntEq { lhs, rhs, .. }
            | Builtin::IntLt { lhs, rhs, .. }
            | Builtin::IntLe { lhs, rhs, .. }
            | Builtin::IntGt { lhs, rhs, .. }
            | Builtin::IntGe { lhs, rhs, .. }
            | Builtin::StringEq { lhs, rhs }
            | Builtin::BoolEq { lhs, rhs } => {
                self.scan(lhs);
                self.scan(rhs);
            }
            Builtin::StringConcat { values } => {
                for value in values {
                    self.scan(value);
                }
            }
            Builtin::ArrayCreateUnsafeUninitialized {
                element_type,
                length,
            } => {
                self.scan(element_type);
                self.scan(length);
            }
            Builtin::ArrayLength {
                element_type,
                array,
            } => {
                self.scan(element_type);
                self.scan(array);
            }
            Builtin::ArrayGet {
                element_type,
                array,
                index,
            } => {
                self.scan(element_type);
                self.scan(array);
                self.scan(index);
            }
            Builtin::ArraySet {
                element_type,
                array,
                index,
                value,
            } => {
                self.scan(element_type);
                self.scan(array);
                self.scan(index);
                self.scan(value);
            }
            Builtin::EqualToRefl { r#type, value } => {
                self.scan(r#type);
                self.scan(value);
            }
        }
    }

    fn scan_pattern(&mut self, pattern: &LocatedPattern<TypeCheckExprContext>) {
        match &pattern.value {
            Pattern::Error | Pattern::String(_) | Pattern::Int(_) | Pattern::Bool(_) => {}
            Pattern::Discard { t } => self.scan(t),
            Pattern::Tuple(patterns) => {
                for pattern in patterns {
                    self.scan_pattern(pattern);
                }
            }
            Pattern::Binding(variable, pattern) => {
                self.scan(&variable.var_type);
                self.scan_pattern(pattern);
                self.add_local(Variable::Local(Box::new(variable.clone())));
            }
            Pattern::EnumVariant {
                enum_type,
                args,
                fields,
                ..
            } => {
                for argument in &enum_type.arguments {
                    self.scan(argument);
                }
                for arg in args {
                    self.scan_pattern(arg);
                }
                for RecordFieldPattern { pattern, .. } in fields {
                    self.scan_pattern(pattern);
                }
            }
        }
    }

    fn scan_match_case(
        &mut self,
        case: &MatchCase<TypeCheckExprContext>,
    ) -> HashMap<Variable<TypeCheckExprContext>, VariableState> {
        let original_states = self.states.clone();
        self.states = original_states.clone();
        self.scopes.push(HashSet::new());
        self.scan_pattern(&case.pattern);
        self.scan(&case.body);
        self.scopes.pop();
        let branch_states = self.states.clone();
        self.states = original_states;
        branch_states
    }
}
