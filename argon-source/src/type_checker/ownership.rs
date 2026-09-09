use crate::type_checker::{ExprNormalizer, TypeCheckExprContext, TypeChecker};
use alloc::{boxed::Box, vec, vec::Vec};
use argon_compiler::expr_type::get_expr_type;
use argon_expr::ownership::{is_shared_type, is_use_mutable_type, is_use_type};
use argon_expr::{
    Builtin, Expr, FullNormalizer, LocatedExpr, LocatedPattern, MatchCase, Pattern,
    RecordFieldLiteral, RecordFieldPattern, Variable,
};
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

#[derive(Clone, Default)]
struct VariableState {
    moved: bool,
    shared: bool,
    borrows: usize,
    mutable_borrows: usize,
    moved_descendants: usize,
    shared_descendants: usize,
    borrows_descendants: usize,
    mutable_borrows_descendants: usize,
    projections: HashMap<usize, VariableState>,
}

#[derive(Clone, Default)]
struct BorrowSnapshots {
    borrows: usize,
    mutable_borrows: usize,
    projections: HashMap<usize, BorrowSnapshots>,
}

fn adjust_count(value: usize, delta: isize) -> usize {
    if delta >= 0 {
        value + delta as usize
    } else {
        value - (-delta) as usize
    }
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
        if !self.tc.enable_ownership {
            return;
        }

        self.scan(expr);
    }

    fn report(&self, error: CompileError) {
        self.tc.context.reporter().report_error(error);
    }

    fn state_mut_at(
        &mut self,
        variable: &Variable<TypeCheckExprContext>,
        projections: &[usize],
    ) -> &mut VariableState {
        let mut state = self.states.entry(variable.clone()).or_default();
        for projection in projections {
            state = state.projections.entry(*projection).or_default();
        }
        state
    }

    fn state_at(
        &self,
        variable: &Variable<TypeCheckExprContext>,
        projections: &[usize],
    ) -> Option<&VariableState> {
        let mut state = self.states.get(variable)?;
        for projection in projections {
            state = state.projections.get(projection)?;
        }
        Some(state)
    }

    fn adjust_descendant_summaries(
        state: &mut VariableState,
        projections: &[usize],
        moved: isize,
        shared: isize,
        borrows: isize,
        mutable_borrows: isize,
    ) {
        if projections.is_empty() {
            return;
        }

        state.moved_descendants = adjust_count(state.moved_descendants, moved);
        state.shared_descendants = adjust_count(state.shared_descendants, shared);
        state.borrows_descendants = adjust_count(state.borrows_descendants, borrows);
        state.mutable_borrows_descendants =
            adjust_count(state.mutable_borrows_descendants, mutable_borrows);

        let projection = projections[0];
        let child = state.projections.entry(projection).or_default();
        Self::adjust_descendant_summaries(
            child,
            &projections[1..],
            moved,
            shared,
            borrows,
            mutable_borrows,
        );
    }

    fn adjust_state_summary(
        &mut self,
        variable: &Variable<TypeCheckExprContext>,
        projections: &[usize],
        moved: isize,
        shared: isize,
        borrows: isize,
        mutable_borrows: isize,
    ) {
        let state = self.states.entry(variable.clone()).or_default();
        Self::adjust_descendant_summaries(
            state,
            projections,
            moved,
            shared,
            borrows,
            mutable_borrows,
        );
    }

    fn normalize_type(
        &mut self,
        mut r#type: LocatedExpr<TypeCheckExprContext>,
    ) -> LocatedExpr<TypeCheckExprContext> {
        let mut normalizer = FullNormalizer::new(
            self.tc.context.normalize_fuel(),
            ExprNormalizer {
                model: &mut self.tc.model,
            },
        );
        normalizer.normalize(&mut r#type);
        r#type
    }

    fn is_shared_place(
        &self,
        variable: &Variable<TypeCheckExprContext>,
        projections: &[usize],
    ) -> bool {
        let Some(mut state) = self.states.get(variable) else {
            return false;
        };

        if state.shared {
            return true;
        }

        for projection in projections {
            let Some(next_state) = state.projections.get(projection) else {
                return false;
            };
            state = next_state;
            if state.shared {
                return true;
            }
        }

        false
    }

    fn place_is_moved(
        &self,
        variable: &Variable<TypeCheckExprContext>,
        projections: &[usize],
    ) -> bool {
        let Some(mut state) = self.states.get(variable) else {
            return false;
        };

        if state.moved {
            return true;
        }

        for projection in projections {
            let Some(next_state) = state.projections.get(projection) else {
                return false;
            };
            state = next_state;
            if state.moved {
                return true;
            }
        }

        false
    }

    fn place_has_moved_descendants(
        &self,
        variable: &Variable<TypeCheckExprContext>,
        projections: &[usize],
    ) -> bool {
        self.state_at(variable, projections)
            .is_some_and(|state| state.moved_descendants > 0)
    }

    fn place_has_borrow(&self, state: &VariableState) -> bool {
        state.borrows > 0
            || state.mutable_borrows > 0
            || state.borrows_descendants > 0
            || state.mutable_borrows_descendants > 0
    }

    fn ancestor_has_borrow(
        &self,
        variable: &Variable<TypeCheckExprContext>,
        projections: &[usize],
    ) -> bool {
        let Some(mut state) = self.states.get(variable) else {
            return false;
        };

        if state.borrows > 0 || state.mutable_borrows > 0 {
            return true;
        }

        for projection in projections {
            let Some(next_state) = state.projections.get(projection) else {
                return false;
            };
            state = next_state;
            if state.borrows > 0 || state.mutable_borrows > 0 {
                return true;
            }
        }

        false
    }

    fn ancestor_has_mutable_borrow(
        &self,
        variable: &Variable<TypeCheckExprContext>,
        projections: &[usize],
    ) -> bool {
        let Some(mut state) = self.states.get(variable) else {
            return false;
        };

        if state.mutable_borrows > 0 {
            return true;
        }

        for projection in projections {
            let Some(next_state) = state.projections.get(projection) else {
                return false;
            };
            state = next_state;
            if state.mutable_borrows > 0 {
                return true;
            }
        }

        false
    }

    fn use_place(
        &mut self,
        variable: &Variable<TypeCheckExprContext>,
        projections: &[usize],
        r#type: &LocatedExpr<TypeCheckExprContext>,
        location: &Location,
    ) {
        let normalized_type = self.normalize_type(r#type.clone());
        if is_shared_type(&normalized_type) {
            self.share_place(variable, projections, location);
            return;
        }

        if is_use_type(&normalized_type) {
            self.borrow_place(variable, projections, location, false);
            return;
        }
        if is_use_mutable_type(&normalized_type) {
            self.borrow_place(variable, projections, location, true);
            return;
        }

        if self.closure_boundaries.len() > 1 && !self.is_current_closure_local(variable) {
            self.mark_shared(variable, projections);
            return;
        }

        if self.is_shared_place(variable, projections) {
            return;
        }

        self.move_place(variable, projections, location);
    }

    fn move_place(
        &mut self,
        variable: &Variable<TypeCheckExprContext>,
        projections: &[usize],
        location: &Location,
    ) {
        if self.place_is_moved(variable, projections)
            || self.place_has_moved_descendants(variable, projections)
        {
            self.report(CompileError::use_after_move(location.clone()));
            return;
        }

        if self.ancestor_has_borrow(variable, projections)
            || self
                .state_at(variable, projections)
                .is_some_and(|state| self.place_has_borrow(state))
        {
            self.report(CompileError::move_while_borrowed(location.clone()));
        }

        let should_mark_moved = {
            let state = self.state_mut_at(variable, projections);
            if !state.moved {
                state.moved = true;
                true
            } else {
                false
            }
        };
        if should_mark_moved {
            self.adjust_state_summary(variable, projections, 1, 0, 0, 0);
        }
    }

    fn borrow_place(
        &mut self,
        variable: &Variable<TypeCheckExprContext>,
        projections: &[usize],
        location: &Location,
        is_mutable: bool,
    ) {
        if self.place_is_moved(variable, projections)
            || self.place_has_moved_descendants(variable, projections)
        {
            self.report(CompileError::borrow_after_move(location.clone()));
        }

        let conflicts = if is_mutable {
            self.ancestor_has_borrow(variable, projections)
                || self
                    .state_at(variable, projections)
                    .is_some_and(|state| self.place_has_borrow(state))
        } else {
            self.ancestor_has_mutable_borrow(variable, projections)
                || self.state_at(variable, projections).is_some_and(|state| {
                    state.mutable_borrows > 0 || state.mutable_borrows_descendants > 0
                })
        };

        if conflicts {
            self.report(CompileError::mutable_borrow_conflict(location.clone()));
        }

        {
            let state = self.state_mut_at(variable, projections);
            if is_mutable {
                state.mutable_borrows += 1;
            } else {
                state.borrows += 1;
            }
        }
        if is_mutable {
            self.adjust_state_summary(variable, projections, 0, 0, 0, 1);
        } else {
            self.adjust_state_summary(variable, projections, 0, 0, 1, 0);
        }
    }

    fn mark_shared(&mut self, variable: &Variable<TypeCheckExprContext>, projections: &[usize]) {
        let should_mark_shared = {
            let state = self.state_mut_at(variable, projections);
            if !state.shared {
                state.shared = true;
                true
            } else {
                false
            }
        };
        if should_mark_shared {
            self.adjust_state_summary(variable, projections, 0, 1, 0, 0);
        }
    }

    fn share_place(
        &mut self,
        variable: &Variable<TypeCheckExprContext>,
        projections: &[usize],
        location: &Location,
    ) {
        if self.place_is_moved(variable, projections)
            || self.place_has_moved_descendants(variable, projections)
        {
            self.report(CompileError::use_after_move(location.clone()));
        }

        self.mark_shared(variable, projections);
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

    fn assign_variable(
        &mut self,
        variable: &Variable<TypeCheckExprContext>,
        location: &Location,
        value: &LocatedExpr<TypeCheckExprContext>,
    ) {
        let normalized_type = self.normalize_type(variable.var_type().clone());
        let is_shared = is_shared_type(&normalized_type)
            || self.is_shared_place(variable, &[])
            || self
                .state_at(variable, &[])
                .is_some_and(|state| state.shared_descendants > 0);

        if is_shared {
            let value = LocatedExpr {
                location: value.location.clone(),
                value: Expr::Share {
                    value: Box::new(value.clone()),
                },
            };

            self.scan(&value);
        } else {
            self.scan(value);
        }

        if self
            .state_at(variable, &[])
            .is_some_and(|state| self.place_has_borrow(state))
        {
            self.report(CompileError::mutable_borrow_conflict(location.clone()));
            return;
        }

        let state = self.state_mut_at(variable, &[]);
        let shared = state.shared || state.shared_descendants > 0;
        state.moved = false;
        state.shared = shared;
        state.borrows = 0;
        state.mutable_borrows = 0;
        state.moved_descendants = 0;
        state.shared_descendants = 0;
        state.borrows_descendants = 0;
        state.mutable_borrows_descendants = 0;
        state.projections.clear();
    }

    fn snapshot_borrows(state: &VariableState) -> BorrowSnapshots {
        BorrowSnapshots {
            borrows: state.borrows,
            mutable_borrows: state.mutable_borrows,
            projections: state
                .projections
                .iter()
                .map(|(projection, state)| (*projection, Self::snapshot_borrows(state)))
                .collect(),
        }
    }

    fn restore_borrows(state: &mut VariableState, snapshot: Option<&BorrowSnapshots>) {
        state.borrows = snapshot.map_or(0, |snapshot| snapshot.borrows);
        state.mutable_borrows = snapshot.map_or(0, |snapshot| snapshot.mutable_borrows);

        for (projection, child) in &mut state.projections {
            let child_snapshot = snapshot.and_then(|snapshot| snapshot.projections.get(projection));
            Self::restore_borrows(child, child_snapshot);
        }
    }

    fn recompute_summaries(state: &mut VariableState) {
        for child in state.projections.values_mut() {
            Self::recompute_summaries(child);
        }

        state.moved_descendants = 0;
        state.shared_descendants = 0;
        state.borrows_descendants = 0;
        state.mutable_borrows_descendants = 0;

        for child in state.projections.values() {
            state.moved_descendants += usize::from(child.moved) + child.moved_descendants;
            state.shared_descendants += usize::from(child.shared) + child.shared_descendants;
            state.borrows_descendants += child.borrows + child.borrows_descendants;
            state.mutable_borrows_descendants +=
                child.mutable_borrows + child.mutable_borrows_descendants;
        }
    }

    fn scan_call_arguments<'b>(
        &mut self,
        values: impl IntoIterator<Item = &'b LocatedExpr<TypeCheckExprContext>>,
    ) {
        let borrow_snapshot = self
            .states
            .iter()
            .map(|(variable, state)| (variable.clone(), Self::snapshot_borrows(state)))
            .collect::<HashMap<_, _>>();

        for value in values {
            self.scan(value);
        }

        for (variable, state) in &mut self.states {
            Self::restore_borrows(state, borrow_snapshot.get(variable));
            Self::recompute_summaries(state);
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
                Self::merge_state(state, &branch_state);
            }
        }

        for state in self.states.values_mut() {
            Self::recompute_summaries(state);
        }
    }

    fn merge_state(state: &mut VariableState, branch_state: &VariableState) {
        state.moved |= branch_state.moved;
        state.shared |= branch_state.shared;
        state.borrows = state.borrows.max(branch_state.borrows);
        state.mutable_borrows = state.mutable_borrows.max(branch_state.mutable_borrows);

        for (projection, branch_child) in &branch_state.projections {
            let child = state.projections.entry(*projection).or_default();
            Self::merge_state(child, branch_child);
        }
    }

    fn place_for_expr(
        expr: &LocatedExpr<TypeCheckExprContext>,
    ) -> Option<(Variable<TypeCheckExprContext>, Vec<usize>)> {
        match &expr.value {
            Expr::Variable(variable) => Some((variable.clone(), Vec::new())),
            Expr::TupleElement(value, index) => {
                let (variable, mut projections) = Self::place_for_expr(value)?;
                projections.push(*index);
                Some((variable, projections))
            }
            _ => None,
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
                    argon_expr::MethodInstanceType::Record(record_type) => {
                        for argument in &record_type.arguments {
                            self.scan(argument);
                        }
                    }
                    argon_expr::MethodInstanceType::Enum(enum_type) => {
                        for argument in &enum_type.arguments {
                            self.scan(argument);
                        }
                    }
                    argon_expr::MethodInstanceType::Trait(trait_type) => {
                        for argument in &trait_type.arguments {
                            self.scan(argument);
                        }
                    }
                }
                self.scan(receiver);
                self.scan_call_arguments(arguments);
            }
            Expr::StaticMethodCall {
                owner_type,
                arguments,
                ..
            } => {
                match owner_type {
                    argon_expr::MethodInstanceType::Record(t) => {
                        for argument in &t.arguments {
                            self.scan(argument);
                        }
                    }
                    argon_expr::MethodInstanceType::Enum(t) => {
                        for argument in &t.arguments {
                            self.scan(argument);
                        }
                    }
                    argon_expr::MethodInstanceType::Trait(t) => {
                        for argument in &t.arguments {
                            self.scan(argument);
                        }
                    }
                }
                self.scan_call_arguments(arguments);
            }
            Expr::Not(value) | Expr::Raise { ex: value } => {
                self.scan(value);
            }
            Expr::TupleElement(value, _) => {
                if let Some((variable, projections)) = Self::place_for_expr(expr) {
                    let r#type = get_expr_type(expr);
                    self.use_place(&variable, &projections, &r#type, &expr.location);
                } else {
                    self.scan(value);
                }
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
                self.scan_call_arguments([record_value.as_ref(), new_value.as_ref()]);
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
                if let Some((variable, projections)) = Self::place_for_expr(value) {
                    self.share_place(&variable, &projections, &value.location);
                } else {
                    self.scan(value);
                }
            }
            Expr::Borrow { value } => {
                if let Some((variable, projections)) = Self::place_for_expr(value) {
                    self.borrow_place(&variable, &projections, &value.location, false);
                } else {
                    self.scan(value);
                }
            }
            Expr::BorrowMut { value } => {
                if let Some((variable, projections)) = Self::place_for_expr(value) {
                    self.borrow_place(&variable, &projections, &value.location, true);
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
            Expr::Variable(variable) => {
                let r#type = variable.var_type().clone();
                self.use_place(variable, &[], &r#type, &expr.location);
            }
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
