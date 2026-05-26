use alloc::{sync::Arc, vec::Vec};
use argon_compiler::{Function, ModulePath, TubeName};
use argon_expr::{ExprContext, LocalVariable};
use argon_util::UniqueIdentifier;
use core::hash::Hash;
use esexpr::core_types::hashbrown::HashMap;

#[derive(Default)]
pub struct TubeIdProvider {
    pub tube_ids: IdStore<TubeName>,
    pub module_ids: IdStore<(TubeName, ModulePath)>,
    pub function_ids: IdStore<Arc<dyn Function>>,
    pub local_variable_ids: IdStore<LocalVariableId>,
    pub local_import_ids: IdStore<UniqueIdentifier>,
}

#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub struct LocalVariableId(*const ());

impl LocalVariableId {
    pub fn new<EC: ExprContext + ?Sized>(variable: &Arc<LocalVariable<EC>>) -> Self {
        Self(Arc::as_ptr(variable).cast::<()>())
    }
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
