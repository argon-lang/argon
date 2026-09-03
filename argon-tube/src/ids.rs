use alloc::sync::Arc;
use argon_compiler::{
    DefaultExprContext, Enum, EnumVariant, Function, Instance, Method, ModulePath, Record,
    RecordField, StaticMethod, Trait, TubeName,
};
use argon_expr::BlockLabel;
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
    pub static_method_ids: IdStore<Arc<dyn StaticMethod>>,
    pub instance_ids: IdStore<Arc<dyn Instance>>,
    pub local_variable_ids: IdStore<UniqueIdentifier>,
    pub closure_parameter_ids: IdStore<UniqueIdentifier>,
    pub local_import_ids: IdStore<UniqueIdentifier>,
    pub block_label_ids: IdStore<BlockLabel<DefaultExprContext>>,
}

pub struct IdStore<T> {
    ids: HashMap<T, usize>,
    next_id: usize,
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
        let id = self.claim_id();
        self.ids.insert(value.clone(), id);
        (id, true)
    }

    pub fn claim_id(&mut self) -> usize {
        let id = self.next_id;
        self.next_id += 1;
        id
    }
}

impl<T> Default for IdStore<T> {
    fn default() -> Self {
        Self {
            ids: HashMap::new(),
            next_id: 0,
        }
    }
}
