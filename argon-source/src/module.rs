use crate::function::SourceFunction;
use argon_compiler::access::{AccessModifierGlobal, AccessToken};
use argon_compiler::erased_sig::{ErasedSignature, ImportSpecifier};
use argon_compiler::scope::{Lookup, OverloadLookup, Overloadable, Scope};
use argon_compiler::{
    Context, DefaultExprContext, Module, ModuleBuilder, ModuleExportBinding, ModuleExportEntry,
    ModulePath, Tube, TubeBuilder, TubeCollection, TubeName,
};
use argon_parser::ast::{ExportStmt, Identifier, ImportPathSegment, ImportStmt, Stmt};
use argon_util::{CompileError, InternalCompilerError};
use nonempty_collections::NEVec;
use parse18_runtime::{Location, WithLocation};
use std::collections::{HashMap, HashSet};
use std::path::Path;
use std::sync::{Arc, OnceLock};

type ResolvedImports = HashMap<Identifier, ResolvedImportGroups>;

#[derive(Clone, Copy)]
enum ResolvedImportGroup {
    SameModule,
    SameTube,
    Other,
}

#[derive(Clone, Default)]
struct ResolvedImportGroups {
    same_module: Vec<ModuleExportEntry>,
    same_tube: Vec<ModuleExportEntry>,
    other: Vec<ModuleExportEntry>,
}

impl ResolvedImportGroups {
    fn extend(
        &mut self,
        group: ResolvedImportGroup,
        exports: impl IntoIterator<Item = ModuleExportEntry>,
    ) {
        match group {
            ResolvedImportGroup::SameModule => self.same_module.extend(exports),
            ResolvedImportGroup::SameTube => self.same_tube.extend(exports),
            ResolvedImportGroup::Other => self.other.extend(exports),
        }
    }

    fn overload_groups(&self, groups: &mut Vec<Vec<Overloadable>>) {
        groups.extend(
            [&self.same_module, &self.same_tube, &self.other]
                .into_iter()
                .filter(|exports| !exports.is_empty())
                .map(|exports| {
                    exports
                        .iter()
                        .map(|entry| match &entry.binding {
                            ModuleExportBinding::Function(f) => Overloadable::Function(f.clone()),
                            ModuleExportBinding::Record(r) => Overloadable::Record(r.clone()),
                            ModuleExportBinding::Enum(e) => Overloadable::Enum(e.clone()),
                            ModuleExportBinding::Trait(t) => Overloadable::Trait(t.clone()),
                            ModuleExportBinding::Instance(i) => Overloadable::Instance(i.clone()),
                        })
                        .collect()
                }),
        )
    }
}

pub struct GlobalScope {
    current_module: Arc<Module>,
    resolved_imports: ResolvedImports,
}

impl Scope for GlobalScope {
    type ExprContext = DefaultExprContext;

    fn lookup(&self, name: &Identifier) -> Lookup<Self::ExprContext> {
        let mut groups = Vec::new();
        if let Some(exp_group) = self.current_module.export_groups().get(name) {
            groups.push(
                exp_group
                    .iter()
                    .map(|entry| match &entry.binding {
                        ModuleExportBinding::Function(f) => Overloadable::Function(f.clone()),
                        ModuleExportBinding::Record(r) => Overloadable::Record(r.clone()),
                        ModuleExportBinding::Enum(e) => Overloadable::Enum(e.clone()),
                        ModuleExportBinding::Trait(t) => Overloadable::Trait(t.clone()),
                        ModuleExportBinding::Instance(i) => Overloadable::Instance(i.clone()),
                    })
                    .collect(),
            );
        }

        if let Some(exports) = self.resolved_imports.get(name) {
            exports.overload_groups(&mut groups);
        };

        if groups.is_empty() {
            Lookup::Empty
        } else {
            Lookup::Overloadable(OverloadLookup::new(groups))
        }
    }

    fn lookup_assign(&self, name: &Identifier) -> Lookup<Self::ExprContext> {
        self.lookup(&Identifier::Update(Box::new(name.clone())))
    }
}

pub struct GlobalScopeBuilder {
    context: Context,
    tube_collection: Arc<TubeCollection>,
    current_tube: Arc<Tube>,
    current_module: Arc<Module>,
    parent: Option<Arc<GlobalScopeBuilder>>,
    imports: Vec<WithLocation<ImportStmt>>,
    resolved_imports: OnceLock<ResolvedImports>,
}

impl GlobalScopeBuilder {
    fn new(
        context: Context,
        tube_collection: Arc<TubeCollection>,
        current_tube: Arc<Tube>,
        current_module: Arc<Module>,
        imports: Vec<WithLocation<ImportStmt>>,
    ) -> Arc<Self> {
        Arc::new(Self {
            context,
            tube_collection,
            current_tube,
            current_module,
            parent: None,
            imports,
            resolved_imports: OnceLock::new(),
        })
    }

    fn build(&self) -> GlobalScope {
        GlobalScope {
            current_module: self.current_module.clone(),
            resolved_imports: self.resolved_imports().clone(),
        }
    }

    fn with_imports(self: Arc<Self>, imports: Vec<WithLocation<ImportStmt>>) -> Arc<Self> {
        Arc::new(Self {
            context: self.context.clone(),
            tube_collection: self.tube_collection.clone(),
            current_tube: self.current_tube.clone(),
            current_module: self.current_module.clone(),
            parent: Some(self.clone()),
            imports,
            resolved_imports: OnceLock::new(),
        })
    }

    fn resolved_imports(&self) -> &ResolvedImports {
        self.resolved_imports.get_or_init(|| {
            let mut resolved = self
                .parent
                .as_ref()
                .map(|parent| parent.resolved_imports().clone())
                .unwrap_or_default();

            for import in &self.imports {
                self.resolve_import(import, &mut resolved);
            }

            resolved
        })
    }

    fn resolve_import(&self, import: &WithLocation<ImportStmt>, resolved: &mut ResolvedImports) {
        match &import.value {
            ImportStmt::Absolute(path) => {
                self.resolve_import_path(
                    self.current_tube.clone(),
                    Vec::new(),
                    path,
                    &import.location,
                    resolved,
                );
            }

            ImportStmt::Relative { up_count, path } => {
                let current_path = &self.current_module.path().0;

                let new_path_len = if *up_count <= current_path.len() {
                    current_path.len() - *up_count
                } else {
                    0
                };

                let module_path = self
                    .current_module
                    .path()
                    .0
                    .iter()
                    .take(new_path_len)
                    .cloned()
                    .collect();

                self.resolve_import_path(
                    self.current_tube.clone(),
                    module_path,
                    path,
                    &import.location,
                    resolved,
                );
            }

            ImportStmt::Tube { tube_name, path } => {
                let tube_name = TubeName(
                    NEVec::try_from_vec(
                        std::iter::once(tube_name.head.clone())
                            .chain(tube_name.tail.iter().cloned())
                            .collect(),
                    )
                    .expect("parser tube names are non-empty"),
                );

                let Some(tube) = self.tube_collection.tube(&tube_name) else {
                    self.context
                        .reporter()
                        .report_error(CompileError::unknown_tube(tube_name.to_string()));
                    return;
                };

                self.resolve_import_path(tube, Vec::new(), path, &import.location, resolved);
            }

            ImportStmt::Member { .. } => todo!("member imports"),
        }
    }

    fn resolve_import_path(
        &self,
        tube: Arc<Tube>,
        module_path: Vec<String>,
        path: &ImportPathSegment,
        location: &Location,
        resolved: &mut ResolvedImports,
    ) {
        match path {
            ImportPathSegment::Cons { id, sub_path } => {
                let mut module_path = module_path;
                module_path.push(id.clone());
                self.resolve_import_path(tube, module_path, sub_path, location, resolved);
            }

            ImportPathSegment::Many { segments } => {
                for segment in segments {
                    self.resolve_import_path(
                        tube.clone(),
                        module_path.clone(),
                        segment,
                        location,
                        resolved,
                    );
                }
            }

            ImportPathSegment::Renaming {
                importing,
                viewed_name,
            } => {
                let viewed_name = viewed_name
                    .value
                    .clone()
                    .unwrap_or_else(|| importing.value.clone());
                self.import_named_export(
                    tube,
                    &ModulePath(module_path),
                    &importing.value,
                    viewed_name,
                    &importing.location,
                    resolved,
                );
            }

            ImportPathSegment::Imported { id } => {
                self.import_named_export(
                    tube,
                    &ModulePath(module_path),
                    &id.value,
                    id.value.clone(),
                    &id.location,
                    resolved,
                );
            }

            ImportPathSegment::Wildcard { location } => {
                self.import_wildcard(tube, &ModulePath(module_path), location, resolved);
            }
        }
    }

    fn import_named_export(
        &self,
        tube: Arc<Tube>,
        module_path: &ModulePath,
        export_name: &Identifier,
        viewed_name: Identifier,
        location: &Location,
        resolved: &mut ResolvedImports,
    ) {
        let Some(module) = self.resolve_module(&tube, module_path, location) else {
            return;
        };

        let Some(exports) = module.named_exports(export_name) else {
            return;
        };

        let visible_exports = exports
            .iter()
            .filter(|entry| self.can_access(&tube, module_path, entry.access))
            .cloned()
            .collect::<Vec<_>>();
        if visible_exports.is_empty() {
            return;
        }

        let group = self.resolved_import_group(&tube, module_path);
        resolved
            .entry(viewed_name)
            .or_default()
            .extend(group, visible_exports);
    }

    fn import_wildcard(
        &self,
        tube: Arc<Tube>,
        module_path: &ModulePath,
        location: &Location,
        resolved: &mut ResolvedImports,
    ) {
        let Some(module) = self.resolve_module(&tube, module_path, location) else {
            return;
        };

        for (name, exports) in module.export_groups().iter() {
            let visible_exports = exports
                .iter()
                .filter(|entry| self.can_access(&tube, module_path, entry.access))
                .cloned()
                .collect::<Vec<_>>();
            if visible_exports.is_empty() {
                continue;
            }

            let group = self.resolved_import_group(&tube, module_path);
            resolved
                .entry(name.clone())
                .or_default()
                .extend(group, visible_exports);
        }
    }

    fn resolve_module(
        &self,
        tube: &Arc<Tube>,
        module_path: &ModulePath,
        location: &Location,
    ) -> Option<Arc<Module>> {
        let module = tube.module(module_path);
        if module.is_none() {
            self.context
                .reporter()
                .report_error(CompileError::unknown_module(
                    location.clone(),
                    tube.name().to_string(),
                    module_path.to_string(),
                ));
        }

        module
    }

    fn can_access(
        &self,
        tube: &Arc<Tube>,
        module_path: &ModulePath,
        access: AccessModifierGlobal,
    ) -> bool {
        match access {
            AccessModifierGlobal::Public => true,
            AccessModifierGlobal::Internal => tube.name() == self.current_tube.name(),
            AccessModifierGlobal::ModulePrivate => {
                tube.name() == self.current_tube.name() && module_path == self.current_module.path()
            }
        }
    }

    fn resolved_import_group(
        &self,
        tube: &Arc<Tube>,
        module_path: &ModulePath,
    ) -> ResolvedImportGroup {
        if tube.name() != self.current_tube.name() {
            ResolvedImportGroup::Other
        } else if module_path == self.current_module.path() {
            ResolvedImportGroup::SameModule
        } else {
            ResolvedImportGroup::SameTube
        }
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
    scope: Arc<GlobalScopeBuilder>,
}

impl DeclarationClosure for ModuleClosure {
    fn scope(&self) -> GlobalScope {
        self.scope.build()
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

pub struct ModuleProcessResult<'a> {
    path: ModulePath,
    reexports: Vec<ExportStmt>,
    context: Context,
    tb: &'a TubeBuilder,
    tube_collection: Arc<TubeCollection>,
}

pub(crate) fn register_module_reexports(results: Vec<ModuleProcessResult<'_>>) {
    let mut registrar = ReexportRegistrar::new(results);
    registrar.register_all();
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum ReexportState {
    Pending,
    Visiting,
    Done,
    Failed,
}

struct ReexportRegistrar<'a> {
    results: HashMap<ModulePath, ModuleProcessResult<'a>>,
    states: HashMap<ModulePath, ReexportState>,
}

impl<'a> ReexportRegistrar<'a> {
    fn new(results: Vec<ModuleProcessResult<'a>>) -> Self {
        let mut states = HashMap::new();
        let mut results_by_path = HashMap::new();

        for result in results {
            states.insert(result.path.clone(), ReexportState::Pending);
            results_by_path.insert(result.path.clone(), result);
        }

        Self {
            results: results_by_path,
            states,
        }
    }

    fn register_all(&mut self) {
        for path in self.results.keys().cloned().collect::<Vec<_>>() {
            self.register_path(&path, None);
        }
    }

    fn register_path(&mut self, path: &ModulePath, location: Option<Location>) -> bool {
        match self
            .states
            .get(path)
            .copied()
            .unwrap_or(ReexportState::Done)
        {
            ReexportState::Done => return true,
            ReexportState::Failed => return false,
            ReexportState::Visiting => {
                if let Some(location) = location {
                    if let Some(result) = self.results.get(path) {
                        result
                            .context
                            .reporter()
                            .report_error(CompileError::circular_reexport(
                                location,
                                path.to_string(),
                            ));
                    }
                }
                return false;
            }
            ReexportState::Pending => {}
        }

        self.states.insert(path.clone(), ReexportState::Visiting);

        let dependencies = self
            .results
            .get(path)
            .map(|result| result.local_reexport_dependencies())
            .unwrap_or_default();
        for (dependency, location) in dependencies {
            if !self.register_path(&dependency, Some(location)) {
                self.states.insert(path.clone(), ReexportState::Failed);
                return false;
            }
        }

        self.register_path_reexports(path);
        self.states.insert(path.clone(), ReexportState::Done);
        true
    }

    fn register_path_reexports(&mut self, path: &ModulePath) {
        let Some(result) = self.results.get(path) else {
            return;
        };

        let module = result.tb.module(result.path.clone());
        let scope = GlobalScopeBuilder::new(
            result.context.clone(),
            result.tube_collection.clone(),
            result.tb.tube(),
            module.module(),
            Vec::new(),
        );

        for reexport in &result.reexports {
            let mut resolved = ResolvedImports::new();
            scope.resolve_import(&reexport.from_import, &mut resolved);

            for (name, groups) in resolved {
                for mut entry in [&groups.same_module, &groups.same_tube, &groups.other]
                    .into_iter()
                    .flat_map(|entries| entries.iter().cloned())
                {
                    entry.is_reexport = true;
                    module.add_export(name.clone(), entry);
                }
            }
        }
    }
}

impl<'a> ModuleProcessResult<'a> {
    fn local_reexport_dependencies(&self) -> Vec<(ModulePath, Location)> {
        self.reexports
            .iter()
            .flat_map(|reexport| self.local_import_dependencies(&reexport.from_import))
            .collect()
    }

    fn local_import_dependencies(
        &self,
        import: &WithLocation<ImportStmt>,
    ) -> Vec<(ModulePath, Location)> {
        match &import.value {
            ImportStmt::Absolute(path) => {
                let mut dependencies = Vec::new();
                Self::collect_import_path_dependencies(Vec::new(), path, &mut dependencies);
                dependencies
            }

            ImportStmt::Relative { up_count, path } => {
                let current_path = &self.path.0;
                let new_path_len = if *up_count <= current_path.len() {
                    current_path.len() - *up_count
                } else {
                    0
                };

                let module_path = current_path.iter().take(new_path_len).cloned().collect();
                let mut dependencies = Vec::new();
                Self::collect_import_path_dependencies(module_path, path, &mut dependencies);
                dependencies
            }

            ImportStmt::Tube { tube_name, path } => {
                let tube_name = TubeName(
                    NEVec::try_from_vec(
                        std::iter::once(tube_name.head.clone())
                            .chain(tube_name.tail.iter().cloned())
                            .collect(),
                    )
                    .expect("parser tube names are non-empty"),
                );

                if &tube_name != self.tb.tube().name() {
                    return Vec::new();
                }

                let mut dependencies = Vec::new();
                Self::collect_import_path_dependencies(Vec::new(), path, &mut dependencies);
                dependencies
            }

            ImportStmt::Member { .. } => Vec::new(),
        }
    }

    fn collect_import_path_dependencies(
        module_path: Vec<String>,
        path: &ImportPathSegment,
        dependencies: &mut Vec<(ModulePath, Location)>,
    ) {
        match path {
            ImportPathSegment::Cons { id, sub_path } => {
                let mut module_path = module_path;
                module_path.push(id.clone());
                Self::collect_import_path_dependencies(module_path, sub_path, dependencies);
            }

            ImportPathSegment::Many { segments } => {
                for segment in segments {
                    Self::collect_import_path_dependencies(
                        module_path.clone(),
                        segment,
                        dependencies,
                    );
                }
            }

            ImportPathSegment::Renaming { importing, .. } => {
                dependencies.push((ModulePath(module_path), importing.location.clone()));
            }

            ImportPathSegment::Imported { id } => {
                dependencies.push((ModulePath(module_path), id.location.clone()));
            }

            ImportPathSegment::Wildcard { location } => {
                dependencies.push((ModulePath(module_path), location.clone()));
            }
        }
    }
}

pub struct DeclarationResult<T> {
    pub(crate) access: AccessModifierGlobal,
    pub(crate) result: Arc<T>,
}

pub fn process_source_file<'a>(
    context: Context,
    path: &Path,
    tb: &'a TubeBuilder,
    tube_collection: Arc<TubeCollection>,
) -> Option<ModuleProcessResult<'a>> {
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

    let mut processor = SourceFileProcessor {
        context: context.clone(),
        tube_collection: tube_collection.clone(),
        tb,
        module,

        result: ModuleProcessResult {
            path,
            reexports: Vec::new(),
            context: context.clone(),
            tb,
            tube_collection: tube_collection.clone(),
        },

        parent_scope: None,
        current_scope: None,
        imports: Vec::new(),
    };

    for stmt in module_decl.stmts {
        processor.process_stmt(stmt);
    }

    Some(processor.result)
}

struct SourceFileProcessor<'a> {
    context: Context,
    tube_collection: Arc<TubeCollection>,
    tb: &'a TubeBuilder,
    module: ModuleBuilder,

    result: ModuleProcessResult<'a>,

    parent_scope: Option<Arc<GlobalScopeBuilder>>,
    current_scope: Option<Arc<GlobalScopeBuilder>>,

    imports: Vec<WithLocation<ImportStmt>>,
}

impl<'a> SourceFileProcessor<'a> {
    fn process_stmt(&mut self, stmt: WithLocation<Stmt>) {
        match stmt.value {
            Stmt::Import(import) => {
                if self.current_scope.is_some() {
                    self.parent_scope = self.current_scope.take();
                }

                self.imports.push(WithLocation {
                    value: *import,
                    location: stmt.location,
                });
            }

            Stmt::Export(export) => {
                self.result.reexports.push(*export);
            }

            Stmt::FunctionDeclaration(decl) => {
                let scope = self.get_scope();

                let closure = Box::new(ModuleClosure {
                    tube_name: self.tb.tube().name().clone(),
                    module_path: self.result.path.clone(),
                    scope,
                });

                let name = decl.name.value.clone();
                let func_res = SourceFunction::from_ast(self.context.clone(), closure, decl);

                let entry = ModuleExportEntry {
                    access: func_res.access,
                    binding: ModuleExportBinding::Function(func_res.result),
                    is_reexport: false,
                };

                self.module.add_export(name, entry);
            }

            _ => todo!(
                "Top Level Statement not implemented: {:?} in {:?}",
                stmt.value,
                self.result.path
            ),
        }
    }

    fn get_scope(&mut self) -> Arc<GlobalScopeBuilder> {
        if let Some(scope) = &self.current_scope {
            return scope.clone();
        }

        let imports = std::mem::take(&mut self.imports);

        let scope = if let Some(parent) = &self.parent_scope {
            parent.clone().with_imports(imports)
        } else {
            GlobalScopeBuilder::new(
                self.context.clone(),
                self.tube_collection.clone(),
                self.tb.tube().clone(),
                self.module.module(),
                imports,
            )
        };

        self.current_scope = Some(scope.clone());
        scope
    }
}
