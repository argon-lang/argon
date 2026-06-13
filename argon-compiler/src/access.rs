use crate::erased_sig::ImportSpecifier;
use crate::{Declaration, MethodOwner, ModulePath, TubeName, TypeDeclaration};
use argon_expr::{Expr, ExprContext, MethodInstanceType};
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

impl From<AccessModifierGlobal> for AccessModifier {
    fn from(global: AccessModifierGlobal) -> Self {
        match global {
            AccessModifierGlobal::Public => AccessModifier::Public,
            AccessModifierGlobal::ModulePrivate => AccessModifier::ModulePrivate,
            AccessModifierGlobal::Internal => AccessModifier::Internal,
        }
    }
}

#[derive(Clone)]
pub struct AccessToken {
    tube: TubeName,
    module: ModulePath,
    allows_access_to: HashSet<TypeDeclaration>,
}

impl AccessToken {
    pub fn new(tube: TubeName, module: ModulePath) -> Self {
        Self {
            tube,
            module,
            allows_access_to: HashSet::new(),
        }
    }

    pub fn add_type_permissions(&mut self, t: TypeDeclaration) {
        self.allows_access_to.insert(t);
    }

    pub fn allows_access(
        &self,
        target: &Declaration,
        instance_type: Option<&MethodOwner>,
        access: AccessModifier,
    ) -> bool {
        match access {
            AccessModifier::Public => true,
            AccessModifier::Private => self.allows_access_private(target),
            AccessModifier::Protected => self.allows_access_protected(target, instance_type),
            AccessModifier::Internal => self.allows_access_internal(target),
            AccessModifier::ProtectedOrInternal => {
                self.allows_access_protected(target, instance_type)
                    || self.allows_access_internal(target)
            }
            AccessModifier::ProtectedAndInternal => {
                self.allows_access_protected(target, instance_type)
                    && self.allows_access_internal(target)
            }
            AccessModifier::ModulePrivate => self.allows_access_module_private(target),
        }
    }

    fn allows_access_private(&self, target: &Declaration) -> bool {
        target
            .clone()
            .closest_type_declaration()
            .is_some_and(|td| self.allows_access_to.contains(&td))
    }

    fn allows_access_protected(
        &self,
        target: &Declaration,
        _instance_type: Option<&MethodOwner>,
    ) -> bool {
        // We can't check anything more for protected yet since
        // subtyping is not yet supported.
        self.allows_access_private(target)
    }

    fn allows_access_internal(&self, target: &Declaration) -> bool {
        let mut import = target.clone().closest_import_specifier();

        loop {
            match import {
                ImportSpecifier::Global { tube, .. } => {
                    return self.tube == tube;
                }
                ImportSpecifier::Local { parent, .. } => {
                    import = *parent;
                }
            }
        }
    }

    fn allows_access_module_private(&self, target: &Declaration) -> bool {
        let mut import = target.clone().closest_import_specifier();

        loop {
            match import {
                ImportSpecifier::Global { tube, module, .. } => {
                    return self.tube == tube && self.module == module;
                }
                ImportSpecifier::Local { parent, .. } => {
                    import = *parent;
                }
            }
        }
    }
}
