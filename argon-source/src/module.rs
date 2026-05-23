use crate::function::SourceFunction;
use argon_compiler::access::{AccessModifierGlobal, AccessToken};
use argon_compiler::erased_sig::{ErasedSignature, ImportSpecifier};
use argon_compiler::scope::{Lookup, Scope};
use argon_compiler::{
    Context, DefaultExprContext, Function, ModuleExportBinding, ModuleExportEntry, ModulePath,
    TubeBuilder, TubeName,
};
use argon_parser::ast;
use argon_parser::ast::{ExportStmt, FunctionDeclarationStmt, Identifier, ImportStmt, Stmt};
use argon_util::InternalCompilerError;
use parse18_runtime::WithLocation;
use std::collections::HashSet;
use std::path::Path;
use std::sync::Arc;

#[derive(Debug, Clone)]
pub struct GlobalScope {}

impl Scope for GlobalScope {
    type ExprContext = DefaultExprContext;

    fn lookup(&mut self, name: &Identifier) -> Lookup<Self::ExprContext> {
        todo!()
    }

    fn lookup_assign(&mut self, name: &Identifier) -> Lookup<Self::ExprContext> {
        todo!()
    }
}

pub trait DeclarationClosure: Sync + Send {
    fn scope(&self) -> GlobalScope;
    fn access_token(&self) -> AccessToken;
    fn import_specifier(&self, name: Identifier, signature: ErasedSignature) -> ImportSpecifier;
}

struct ModuleClosure {
    tube_name: TubeName,
    module_path: ModulePath,
    imports: Vec<ImportStmt>,
}

impl DeclarationClosure for ModuleClosure {
    fn scope(&self) -> GlobalScope {
        GlobalScope {}
    }

    fn access_token(&self) -> AccessToken {
        AccessToken {
            tube: self.tube_name.clone(),
            module: self.module_path.clone(),
            allows_access_to: HashSet::new(),
        }
    }

    fn import_specifier(&self, name: Identifier, signature: ErasedSignature) -> ImportSpecifier {
        ImportSpecifier::Global {
            tube: self.tube_name.clone(),
            module: self.module_path.clone(),
            name,
            signature: Box::new(signature),
        }
    }
}

pub struct ModuleProcessResult {
    path: ModulePath,
    reexports: Vec<ExportStmt>,
}

pub struct DeclarationResult<T> {
    pub(crate) access: AccessModifierGlobal,
    pub(crate) result: Arc<T>,
}

pub fn process_source_file(
    context: Context,
    path: &Path,
    tb: &TubeBuilder,
) -> Option<ModuleProcessResult> {
    let mut file = match std::fs::File::open(path) {
        Ok(file) => file,
        Err(e) => {
            context
                .reporter()
                .report_error(InternalCompilerError::IoError(path.to_path_buf(), e));
            return None;
        }
    };

    let module_decl = argon_parser::parse(&mut file, path, context.reporter());

    let path = ModulePath(module_decl.module_path);

    let module = tb.module(path.clone());

    let mut result = ModuleProcessResult {
        path: path.clone(),
        reexports: Vec::new(),
    };

    let mut imports = Vec::new();

    for stmt in module_decl.stmts {
        match stmt.value {
            Stmt::Import(import) => {
                imports.push(*import);
            }

            Stmt::Export(export) => {
                result.reexports.push(*export);
            }

            Stmt::FunctionDeclaration(decl) => {
                let closure = Box::new(ModuleClosure {
                    tube_name: tb.tube().name().clone(),
                    module_path: path.clone(),
                    imports: imports.clone(),
                });

                let name = decl.name.value.clone();
                let func_res = SourceFunction::from_ast(context.clone(), closure, decl);

                let entry = ModuleExportEntry {
                    access: func_res.access,
                    binding: ModuleExportBinding::Function(func_res.result),
                    is_reexport: false,
                };

                module.add_export(name, entry);
            }

            _ => todo!(
                "Top Level Statement not implemented: {:?} in {:?}",
                stmt.value,
                path
            ),
        }
    }

    Some(result)
}
