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

impl AccessModifier {
    pub fn display(self) -> &'static str {
        match self {
            AccessModifier::Public => "public",
            AccessModifier::Private => "private",
            AccessModifier::Protected => "protected",
            AccessModifier::Internal => "internal",
            AccessModifier::ProtectedOrInternal => "protected internal",
            AccessModifier::ProtectedAndInternal => "protected and internal",
            AccessModifier::ModulePrivate => "module private",
        }
    }

    pub fn join(self, other: Self) -> Self {
        use AccessModifier::*;

        match (self, other) {
            (Public, _) | (_, Public) => Public,
            (ProtectedOrInternal, _) | (_, ProtectedOrInternal) => ProtectedOrInternal,
            (Protected, Internal) | (Internal, Protected) => ProtectedOrInternal,
            (Protected, _) | (_, Protected) => Protected,
            (Internal, _) | (_, Internal) => Internal,
            (ModulePrivate, _) | (_, ModulePrivate) => ModulePrivate,
            (Private, Private) => Private,
            (ProtectedAndInternal, ProtectedAndInternal) => ProtectedAndInternal,
            (ProtectedAndInternal, Private) | (Private, ProtectedAndInternal) => {
                ProtectedAndInternal
            }
        }
    }

    pub fn is_wider_than(self, other: Self) -> bool {
        self != other && self.join(other) == self
    }
}

impl AccessToken {
    pub fn allows_access(
        &self,
        target: &TypeDeclaration,
        declaring_tube: &TubeName,
        declaring_module: &ModulePath,
        access: AccessModifier,
    ) -> bool {
        match access {
            AccessModifier::Public => true,
            AccessModifier::Private => self.allows_access_to.contains(target),
            AccessModifier::Protected => self.allows_access_to.contains(target),
            AccessModifier::Internal => self.tube == *declaring_tube,
            AccessModifier::ProtectedOrInternal => {
                self.tube == *declaring_tube || self.allows_access_to.contains(target)
            }
            AccessModifier::ProtectedAndInternal => {
                self.tube == *declaring_tube && self.allows_access_to.contains(target)
            }
            AccessModifier::ModulePrivate => {
                self.tube == *declaring_tube && self.module == *declaring_module
            }
        }
    }
}
