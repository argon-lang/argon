#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AccessModifierGlobal {
    Public,
    ModulePrivate,
    Internal,
}
