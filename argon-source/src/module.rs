use crate::function::SourceFunction;
use argon_compiler::access::AccessModifierGlobal;
use argon_compiler::{
    Context, Function, ModuleExportBinding, ModuleExportEntry, ModulePath, TubeBuilder,
};
use argon_parser::ast;
use argon_parser::ast::{ExportStmt, FunctionDeclarationStmt, Stmt};
use argon_util::ErrorReporter;
use parse18_runtime::WithLocation;
use std::path::Path;
use std::sync::Arc;

pub struct GlobalScope;

pub trait ScopeProvider {
    fn make_scope(&self) -> GlobalScope;
}

pub struct ModuleProcessResult {
    path: ModulePath,
    reexports: Vec<ExportStmt>,
}

pub struct DeclarationResult<T> {
    access: AccessModifierGlobal,
    result: Arc<T>,
}

pub fn process_source_file<C: Context>(
    context: &C,
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

    let module_decl = argon_parser::parse(&mut file, path, context.reporter().clone());

    let path = ModulePath(module_decl.module_path);

    let module = tb.module(path.clone());

    let mut result = ModuleProcessResult {
        path: path,
        reexports: Vec::new(),
    };

    let mut imports = Vec::new();

    for stmt in module_decl.stmts {
        match stmt.value {
            Stmt::Import(import) => {
                imports.push(import);
            }

            Stmt::Export(export) => {
                result.reexports.push(*export);
            }

            Stmt::FunctionDeclaration(decl) => {
                let name = Some(decl.name.value.clone());
                let func_res = SourceFunction::from_ast(decl);

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
