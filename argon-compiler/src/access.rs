use crate::{ModulePath, TubeName, TypeDeclaration};
use hashbrown::HashSet;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AccessModifierGlobal {
    Public,
    ModulePrivate,
    Internal,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AccessModifier {
    Public,
    Private,
    Protected,
    Internal,
    ProtectedOrInternal,
    ProtectedAndInternal,
    ModulePrivate,
}

pub struct AccessToken {
    pub tube: TubeName,
    pub module: ModulePath,
    pub allows_access_to: HashSet<TypeDeclaration>,
}
