use argon_ir::{ProgramPoint, Region, Register, Value};
use hashbrown::HashMap;
use rpds::HashTrieMap;

use crate::type_checker_ir::{Hole, TypeCheckExprContext};

#[derive(Clone)]
struct KnownRegisterValue {
    value: Value<TypeCheckExprContext>,
    changed_at: ProgramPoint,
}

type RegisterValues = HashTrieMap<Register, KnownRegisterValue>;

pub struct Model {
    hole_values: HashMap<Hole, Region<TypeCheckExprContext>>,
    register_values: HashMap<ProgramPoint, RegisterValues>,
}

impl Model {
    pub fn new() -> Self {
        Self {
            hole_values: HashMap::new(),
            register_values: HashMap::new(),
        }
    }

    pub fn get_hole_value(&self, hole: &Hole) -> Option<&Region<TypeCheckExprContext>> {
        self.hole_values.get(hole)
    }

    pub fn add_entry_point(&mut self, at: ProgramPoint) {
        assert!(
            !self.register_values.contains_key(&at),
            "program point was already added to the model"
        );
        self.register_values.insert(at, RegisterValues::new());
    }

    pub fn fork_program_point(&mut self, from: &ProgramPoint, at: ProgramPoint) {
        assert!(
            !self.register_values.contains_key(&at),
            "program point was already added to the model"
        );
        let values = self
            .register_values
            .get(from)
            .expect("predecessor program point is not in the model")
            .clone();
        self.register_values.insert(at, values);
    }

    pub fn set_register_value(
        &mut self,
        at: &ProgramPoint,
        register: Register,
        value: Value<TypeCheckExprContext>,
    ) {
        let values = self
            .register_values
            .get_mut(at)
            .expect("program point is not in the model");

        if values
            .get(&register)
            .is_some_and(|known| known.value == value)
        {
            return;
        }

        values.insert_mut(
            register,
            KnownRegisterValue {
                value,
                changed_at: at.clone(),
            },
        );
    }

    pub fn forget_register_value(&mut self, at: &ProgramPoint, register: &Register) {
        self.register_values
            .get_mut(at)
            .expect("program point is not in the model")
            .remove_mut(register);
    }

    pub fn get_register_value(
        &self,
        at: &ProgramPoint,
        register: &Register,
    ) -> Option<&Value<TypeCheckExprContext>> {
        Some(&self.register_values.get(at)?.get(register)?.value)
    }

    pub fn get_register_value_change_point(
        &self,
        at: &ProgramPoint,
        register: &Register,
    ) -> Option<&ProgramPoint> {
        Some(&self.register_values.get(at)?.get(register)?.changed_at)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn inherits_values_without_copying_the_map() {
        let mut model = Model::new();
        let entry = ProgramPoint::new();
        let successor = ProgramPoint::new();
        let register = Register::new();

        model.add_entry_point(entry.clone());
        model.set_register_value(&entry, register.clone(), Value::BoolLiteral(true));
        model.fork_program_point(&entry, successor.clone());

        assert!(
            model.register_values[&entry].ptr_eq(&model.register_values[&successor]),
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
        model.set_register_value(
            &entry,
            other_register.clone(),
            Value::BoolLiteral(false),
        );
        model.fork_program_point(&entry, same_value.clone());
        model.set_register_value(&same_value, register.clone(), Value::BoolLiteral(true));
        model.fork_program_point(&same_value, changed_value.clone());
        model.set_register_value(&changed_value, register.clone(), Value::BoolLiteral(false));
        model.fork_program_point(&entry, sibling.clone());

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
        model.fork_program_point(&entry, forgotten.clone());
        model.forget_register_value(&forgotten, &register);
        model.fork_program_point(&forgotten, descendant.clone());

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
}
