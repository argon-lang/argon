use alloc::{sync::Arc, vec::Vec};
use argon_compiler::{
    DefaultExprContext, Enum, EnumVariant, Function, Instance, Method, ModulePath, Record,
    RecordField, Trait, TubeName,
};
use argon_expr::{ExprContext, LocalVariable};
use argon_util::UniqueIdentifier;
use core::hash::Hash;
use esexpr::core_types::hashbrown::HashMap;

#[derive(Default)]
pub struct TubeIdProvider {
    pub tube_ids: IdStore<TubeName>,
    pub module_ids: IdStore<(TubeName, ModulePath)>,
    pub function_ids: IdStore<Arc<dyn Function>>,
    pub record_ids: IdStore<Arc<dyn Record>>,
    pub enum_ids: IdStore<Arc<dyn Enum>>,
    pub enum_variant_ids: IdStore<Arc<dyn EnumVariant>>,
    pub record_field_ids: IdStore<Arc<dyn RecordField>>,
    pub trait_ids: IdStore<Arc<dyn Trait>>,
    pub method_ids: IdStore<Arc<dyn Method>>,
    pub instance_ids: IdStore<Arc<dyn Instance>>,
    pub local_variable_ids: IdStore<UniqueIdentifier>,
    pub local_import_ids: IdStore<UniqueIdentifier>,
}

pub struct IdStore<T> {
    ids: HashMap<T, usize>,
    values: Vec<T>,
}

impl<T: Eq + Hash + Clone> IdStore<T> {
    pub fn get(&mut self, value: T) -> usize {
        let (id, _) = self.get_with_new(value);
        id
    }

    pub fn get_with_new(&mut self, value: T) -> (usize, bool) {
        if let Some(id) = self.ids.get(&value) {
            return (*id, false);
        }
        let id = self.values.len();
        self.ids.insert(value.clone(), id);
        self.values.push(value);
        (id, true)
    }
}

impl<T> Default for IdStore<T> {
    fn default() -> Self {
        Self {
            ids: HashMap::new(),
            values: Vec::new(),
        }
    }
}
