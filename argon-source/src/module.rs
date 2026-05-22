use crate::function::SourceFunction;
use argon_compiler::access::{AccessModifierGlobal, AccessToken};
use argon_compiler::{Context, DefaultExprContext, Function, ModuleExportBinding, ModuleExportEntry, ModulePath, TubeBuilder};
use argon_parser::ast;
use argon_parser::ast::{ExportStmt, FunctionDeclarationStmt, IdentifierExpr, ImportStmt, Stmt};
use argon_util::ErrorReporter;
use parse18_runtime::WithLocation;
use std::path::Path;
use std::sync::Arc;
use argon_compiler::scope::{Lookup, Scope};

#[derive(Debug, Clone)]
pub struct GlobalScope {
}

impl Scope for GlobalScope {
    type ExprContext = DefaultExprContext;

    fn lookup(&mut self, name: &IdentifierExpr) -> Lookup<Self::ExprContext> {
        todo!()
    }

    fn lookup_assign(&mut self, name: &IdentifierExpr) -> Lookup<Self::ExprContext> {
        todo!()
    }
}

pub trait DeclarationClosure: Sync + Send {
    fn scope(&self) -> GlobalScope;
    fn access_token(&self) -> AccessToken;
}

struct ModuleClosure {
    imports: Vec<ImportStmt>,
}

impl DeclarationClosure for ModuleClosure {
    fn scope(&self) -> GlobalScope {
        GlobalScope {}
    }

    fn access_token(&self) -> AccessToken {
        todo!()
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
            context.reporter().report_error(e);
            return None;
        }
    };

    let module_decl = argon_parser::parse(&mut file, path, context.reporter());

    let path = ModulePath(module_decl.module_path);

    let module = tb.module(path.clone());

    let mut result = ModuleProcessResult {
        path,
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
                    imports: imports.clone(),
                });

                let name = Some(decl.name.value.clone());
                let func_res = SourceFunction::from_ast(context.clone(), closure, decl);

                let entry = ModuleExportEntry {
                    access: func_res.access,
                    binding: ModuleExportBinding::Function(func_res.result),
                    is_reexport: false,
                };

                module.add_export(name, entry);
            }

            _ => todo!(),
        }
    }

    Some(result)
}
