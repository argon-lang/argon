use alloc::{vec, vec::Vec};

use argon_ir::{IrScanner, ProgramPoint, Region, Register, Value};
use hashbrown::HashMap;
use rpds::{HashTrieMap, List};

use crate::type_checker_ir::{ExpectedType, Hole, TypeCheckExprContext};

#[derive(Clone)]
struct KnownRegisterValue {
    value: Value<TypeCheckExprContext>,
    changed_at: ProgramPoint,
}

type RegisterValues = HashTrieMap<Register, KnownRegisterValue>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub(super) struct Condition {
    pub value: Value<TypeCheckExprContext>,
    pub expected: bool,
}

impl Condition {
    pub(super) fn new(value: Value<TypeCheckExprContext>, expected: bool) -> Self {
        Self { value, expected }
    }
}

#[derive(Clone)]
struct GuardedState {
    conditions: List<Condition>,
    register_values: RegisterValues,
}

type ProgramPointState = Vec<GuardedState>;

pub struct Model {
    hole_values: HashMap<Hole, HoleBinding>,
    register_values: HashMap<ProgramPoint, ProgramPointState>,
}

#[derive(Clone, Debug)]
enum HoleBinding {
    Alias(Hole),
    Solved(Region<TypeCheckExprContext>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum BindHoleError {
    AlreadyBound,
    Recursive,
    InvalidSolution,
}

impl Model {
    pub fn new() -> Self {
        Self {
            hole_values: HashMap::new(),
            register_values: HashMap::new(),
        }
    }

    /// Resolves aliases and returns the region assigned to `hole`, if it is solved.
    ///
    /// Cycles cannot be created through the public binding methods. The step limit is
    /// retained here so a corrupt model cannot make recovery loop forever.
    pub fn get_hole_value(&self, hole: &Hole) -> Option<&Region<TypeCheckExprContext>> {
        let mut current = hole;
        for _ in 0..=self.hole_values.len() {
            match self.hole_values.get(current)? {
                HoleBinding::Alias(next) => current = next,
                HoleBinding::Solved(value) => return Some(value),
            }
        }
        None
    }

    pub fn bind_hole_alias(&mut self, hole: Hole, alias: Hole) -> Result<(), BindHoleError> {
        if self.hole_values.contains_key(&hole) {
            return Err(BindHoleError::AlreadyBound);
        }
        if hole == alias || self.alias_reaches(&alias, &hole) {
            return Err(BindHoleError::Recursive);
        }
        self.hole_values.insert(hole, HoleBinding::Alias(alias));
        Ok(())
    }

    /// Validates a prospective solution before making it observable in the model.
    /// The callback is responsible for checking the solution against the hole's
    /// meta-type; recursive references are rejected independently here.
    pub fn bind_hole_value(
        &mut self,
        hole: Hole,
        value: Region<TypeCheckExprContext>,
        validate: impl FnOnce(&ExpectedType, &Region<TypeCheckExprContext>) -> bool,
    ) -> Result<(), BindHoleError> {
        if self.hole_values.contains_key(&hole) {
            return Err(BindHoleError::AlreadyBound);
        }
        if region_holes(&value)
            .iter()
            .any(|nested| nested == &hole || self.alias_reaches(nested, &hole))
        {
            return Err(BindHoleError::Recursive);
        }
        if !validate(hole.hole_type(), &value) {
            return Err(BindHoleError::InvalidSolution);
        }
        self.hole_values.insert(hole, HoleBinding::Solved(value));
        Ok(())
    }

    fn alias_reaches(&self, from: &Hole, target: &Hole) -> bool {
        let mut current = from;
        for _ in 0..=self.hole_values.len() {
            if current == target {
                return true;
            }
            match self.hole_values.get(current) {
                Some(HoleBinding::Alias(next)) => current = next,
                Some(HoleBinding::Solved(_)) | None => return false,
            }
        }
        true
    }

    pub fn add_entry_point(&mut self, at: ProgramPoint) {
        assert!(
            !self.register_values.contains_key(&at),
            "program point was already added to the model"
        );
        self.register_values.insert(
            at,
            vec![GuardedState {
                conditions: List::new(),
                register_values: RegisterValues::new(),
            }],
        );
    }

    pub fn fork_program_point(
        &mut self,
        from: &ProgramPoint,
        at: ProgramPoint,
        new_conditions: impl IntoIterator<Item = Condition>,
    ) {
        assert!(
            !self.register_values.contains_key(&at),
            "program point was already added to the model"
        );
        let new_conditions: Vec<_> = new_conditions.into_iter().collect();
        let states = self
            .register_values
            .get(from)
            .expect("predecessor program point is not in the model")
            .iter()
            .map(|state| {
                let mut conditions = state.conditions.clone();
                for condition in &new_conditions {
                    conditions.push_front_mut(condition.clone());
                }
                GuardedState {
                    conditions,
                    register_values: state.register_values.clone(),
                }
            })
            .collect();
        self.register_values.insert(at, states);
    }

    pub fn set_register_value(
        &mut self,
        at: &ProgramPoint,
        register: Register,
        value: Value<TypeCheckExprContext>,
    ) {
        let states = self
            .register_values
            .get_mut(at)
            .expect("program point is not in the model");

        for state in states {
            if state
                .register_values
                .get(&register)
                .is_some_and(|known| known.value == value)
            {
                continue;
            }

            state.register_values.insert_mut(
                register.clone(),
                KnownRegisterValue {
                    value: value.clone(),
                    changed_at: at.clone(),
                },
            );
        }
    }

    pub fn forget_register_value(&mut self, at: &ProgramPoint, register: &Register) {
        for state in self
            .register_values
            .get_mut(at)
            .expect("program point is not in the model")
        {
            state.register_values.remove_mut(register);
        }
    }

    pub fn get_register_value(
        &self,
        at: &ProgramPoint,
        register: &Register,
    ) -> Option<&Value<TypeCheckExprContext>> {
        let mut states = self.register_values.get(at)?.iter();
        let value = &states.next()?.register_values.get(register)?.value;
        states
            .all(|state| {
                state
                    .register_values
                    .get(register)
                    .is_some_and(|known| &known.value == value)
            })
            .then_some(value)
    }

    pub fn get_register_value_change_point(
        &self,
        at: &ProgramPoint,
        register: &Register,
    ) -> Option<&ProgramPoint> {
        let mut states = self.register_values.get(at)?.iter();
        let changed_at = &states.next()?.register_values.get(register)?.changed_at;
        states
            .all(|state| {
                state
                    .register_values
                    .get(register)
                    .is_some_and(|known| &known.changed_at == changed_at)
            })
            .then_some(changed_at)
    }
}

struct HoleFinder {
    holes: Vec<Hole>,
}

impl IrScanner for HoleFinder {
    type EC = TypeCheckExprContext;

    fn scan_hole(&mut self, hole: &Hole) -> bool {
        self.holes.push(hole.clone());
        true
    }
}

fn region_holes(region: &Region<TypeCheckExprContext>) -> Vec<Hole> {
    let mut finder = HoleFinder { holes: Vec::new() };
    finder.scan_region(region);
    finder.holes
}

#[cfg(test)]
mod tests {
    use super::*;
    use argon_expr::ErasureMode;
    use argon_ir::{Instruction, InstructionNode};
    use parse18_runtime::{FilePosition, Location};

    fn location() -> Location {
        Location {
            file: "test".into(),
            start: FilePosition { line: 0, column: 0 },
            end: FilePosition { line: 0, column: 0 },
        }
    }

    fn empty_region() -> Region<TypeCheckExprContext> {
        Region {
            entry: ProgramPoint::new(),
            instructions: Vec::new(),
            result: Register::new(),
        }
    }

    fn hole() -> Hole {
        Hole::new(location(), super::super::ExpectedType::AnyMetaType)
    }

    #[test]
    fn inherits_values_without_copying_the_map() {
        let mut model = Model::new();
        let entry = ProgramPoint::new();
        let successor = ProgramPoint::new();
        let register = Register::new();

        model.add_entry_point(entry.clone());
        model.set_register_value(&entry, register.clone(), Value::BoolLiteral(true));
        model.fork_program_point(&entry, successor.clone(), []);

        assert!(
            model.register_values[&entry][0]
                .register_values
                .ptr_eq(&model.register_values[&successor][0].register_values),
            "an unchanged successor should structurally share its register map"
        );
        assert_eq!(
            model.get_register_value(&successor, &register),
            Some(&Value::BoolLiteral(true))
        );
        assert_eq!(
            model.get_register_value_change_point(&successor, &register),
            Some(&entry)
        );
    }

    #[test]
    fn changes_are_isolated_and_equal_values_keep_the_original_point() {
        let mut model = Model::new();
        let entry = ProgramPoint::new();
        let same_value = ProgramPoint::new();
        let changed_value = ProgramPoint::new();
        let sibling = ProgramPoint::new();
        let register = Register::new();
        let other_register = Register::new();

        model.add_entry_point(entry.clone());
        model.set_register_value(&entry, register.clone(), Value::BoolLiteral(true));
        model.set_register_value(&entry, other_register.clone(), Value::BoolLiteral(false));
        model.fork_program_point(&entry, same_value.clone(), []);
        model.set_register_value(&same_value, register.clone(), Value::BoolLiteral(true));
        model.fork_program_point(&same_value, changed_value.clone(), []);
        model.set_register_value(&changed_value, register.clone(), Value::BoolLiteral(false));
        model.fork_program_point(&entry, sibling.clone(), []);

        assert_eq!(
            model.get_register_value_change_point(&same_value, &register),
            Some(&entry)
        );
        assert_eq!(
            model.get_register_value_change_point(&changed_value, &register),
            Some(&changed_value)
        );
        assert_eq!(
            model.get_register_value(&entry, &register),
            Some(&Value::BoolLiteral(true))
        );
        assert_eq!(
            model.get_register_value(&sibling, &register),
            Some(&Value::BoolLiteral(true))
        );
        assert_eq!(
            model.get_register_value(&changed_value, &other_register),
            Some(&Value::BoolLiteral(false))
        );
        assert_eq!(
            model.get_register_value_change_point(&changed_value, &other_register),
            Some(&entry)
        );
    }

    #[test]
    fn forgetting_a_value_only_affects_that_snapshot_and_its_descendants() {
        let mut model = Model::new();
        let entry = ProgramPoint::new();
        let forgotten = ProgramPoint::new();
        let descendant = ProgramPoint::new();
        let register = Register::new();

        model.add_entry_point(entry.clone());
        model.set_register_value(&entry, register.clone(), Value::BoolLiteral(true));
        model.fork_program_point(&entry, forgotten.clone(), []);
        model.forget_register_value(&forgotten, &register);
        model.fork_program_point(&forgotten, descendant.clone(), []);

        assert_eq!(
            model.get_register_value(&entry, &register),
            Some(&Value::BoolLiteral(true))
        );
        assert_eq!(model.get_register_value(&forgotten, &register), None);
        assert_eq!(model.get_register_value(&descendant, &register), None);
        assert_eq!(
            model.get_register_value_change_point(&forgotten, &register),
            None
        );
    }

    #[test]
    fn condition_free_forks_share_conditions_and_register_values() {
        let mut model = Model::new();
        let entry = ProgramPoint::new();
        let guarded = ProgramPoint::new();
        let successor = ProgramPoint::new();
        let register = Register::new();

        model.add_entry_point(entry.clone());
        model.set_register_value(&entry, register.clone(), Value::BoolLiteral(true));
        model.fork_program_point(
            &entry,
            guarded.clone(),
            [Condition::new(Value::Register(register), true)],
        );
        model.fork_program_point(&guarded, successor.clone(), []);

        let guarded_state = &model.register_values[&guarded][0];
        let successor_state = &model.register_values[&successor][0];
        assert!(
            guarded_state
                .register_values
                .ptr_eq(&successor_state.register_values)
        );
        assert!(core::ptr::eq(
            guarded_state.conditions.first().unwrap(),
            successor_state.conditions.first().unwrap()
        ));
    }

    #[test]
    fn guarded_forks_are_isolated_and_accept_non_register_values() {
        let mut model = Model::new();
        let entry = ProgramPoint::new();
        let positive = ProgramPoint::new();
        let negative = ProgramPoint::new();
        let predicate = Value::Tuple(vec![Value::BoolLiteral(true)]);

        model.add_entry_point(entry.clone());
        model.fork_program_point(
            &entry,
            positive.clone(),
            [Condition::new(predicate.clone(), true)],
        );
        model.fork_program_point(
            &entry,
            negative.clone(),
            [Condition::new(predicate.clone(), false)],
        );

        assert!(model.register_values[&entry][0].conditions.is_empty());
        assert_eq!(
            model.register_values[&positive][0]
                .conditions
                .iter()
                .cloned()
                .collect::<Vec<_>>(),
            vec![Condition::new(predicate.clone(), true)]
        );
        assert_eq!(
            model.register_values[&negative][0]
                .conditions
                .iter()
                .cloned()
                .collect::<Vec<_>>(),
            vec![Condition::new(predicate, false)]
        );
    }

    #[test]
    fn nested_forks_retain_the_complete_condition_chain() {
        let mut model = Model::new();
        let entry = ProgramPoint::new();
        let outer = ProgramPoint::new();
        let inner = ProgramPoint::new();
        let first = Condition::new(Value::BoolLiteral(true), true);
        let second = Condition::new(Value::StringLiteral("condition".into()), false);
        let third = Condition::new(Value::IntLiteral(3.into()), true);

        model.add_entry_point(entry.clone());
        model.fork_program_point(&entry, outer.clone(), [first.clone()]);
        model.fork_program_point(&outer, inner.clone(), [second.clone(), third.clone()]);

        assert_eq!(
            model.register_values[&inner][0]
                .conditions
                .iter()
                .cloned()
                .collect::<Vec<_>>(),
            vec![third, second, first]
        );
        assert_eq!(
            model.register_values[&outer][0]
                .conditions
                .iter()
                .cloned()
                .collect::<Vec<_>>(),
            vec![Condition::new(Value::BoolLiteral(true), true)]
        );
    }

    #[test]
    fn resolves_aliases_transitively() {
        let mut model = Model::new();
        let first = hole();
        let second = hole();
        let third = hole();
        let solution = empty_region();

        model
            .bind_hole_alias(first.clone(), second.clone())
            .unwrap();
        model.bind_hole_alias(second, third.clone()).unwrap();
        model
            .bind_hole_value(third, solution.clone(), |_, _| true)
            .unwrap();

        assert_eq!(model.get_hole_value(&first), Some(&solution));
    }

    #[test]
    fn rejects_alias_cycles() {
        let mut model = Model::new();
        let first = hole();
        let second = hole();

        model
            .bind_hole_alias(first.clone(), second.clone())
            .unwrap();
        assert_eq!(
            model.bind_hole_alias(second, first),
            Err(BindHoleError::Recursive)
        );
    }

    #[test]
    fn invalid_solutions_are_not_committed() {
        let mut model = Model::new();
        let hole = hole();
        let solution = empty_region();

        assert_eq!(
            model.bind_hole_value(hole.clone(), solution.clone(), |_, _| false),
            Err(BindHoleError::InvalidSolution)
        );
        assert!(model.get_hole_value(&hole).is_none());
        model
            .bind_hole_value(hole.clone(), solution.clone(), |_, _| true)
            .unwrap();
        assert_eq!(model.get_hole_value(&hole), Some(&solution));
    }

    #[test]
    fn rejects_holes_recursive_through_values_instructions_and_aliases() {
        let mut model = Model::new();
        let target = hole();
        let alias = hole();
        model
            .bind_hole_alias(alias.clone(), target.clone())
            .unwrap();

        let entry = ProgramPoint::new();
        let nested = Region {
            entry: entry.clone(),
            instructions: vec![InstructionNode {
                instruction: Instruction::Hole {
                    destination: Register::new(),
                    hole: alias,
                },
                location: location(),
                exit: ProgramPoint::new(),
                erasure_mode: ErasureMode::Erased,
            }],
            result: Register::new(),
        };

        assert_eq!(
            model.bind_hole_value(target, nested, |_, _| true),
            Err(BindHoleError::Recursive)
        );
    }
}
