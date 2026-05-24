use argon_compiler::erased_sig::{
    erase_signature, ErasedSignature, ErasedSignatureType, ImportSpecifier,
};
use argon_compiler::signature::{ParameterBinding, SignatureParameter};
use argon_compiler::{AccessModifierGlobal, BinaryOperatorIdentifier, Builtin, DefaultExprContext, EffectInfo, ErasureMode, Expr, Function, FunctionImplementation, FunctionMetadata, FunctionParameterListType, FunctionSignature, Identifier, ModuleExportBinding, ModuleExportEntry, ModulePath, Tube, TubeCollection, TubeCollectionBuilder, TubeMetadata, TubeName, UnaryOperatorIdentifier, Unload};
use argon_expr::{FunctionArgument, LocalVariable, ParameterVariable, Variable};
use argon_format::tube as tf;
use argon_util::UniqueIdentifier;
use nonempty_collections::NEVec;
use num_bigint::{BigInt, BigUint};
use std::collections::HashMap;
use std::sync::Arc;
use std::sync::OnceLock;

pub fn decode_tube(
    mut tube: impl Iterator<Item = tf::TubeFileEntry>,
    tube_collection_builder: &TubeCollectionBuilder,
) -> Arc<Tube> {
    read_version_entry(tube.next());
    let metadata = read_metadata_entry(tube.next());

    let mut decoder = TubeDecoder::new(tube_collection_builder.tube_collection(), metadata);
    decoder.decode(tube, tube_collection_builder)
}

struct TubeDecoder {
    tube_collection: Arc<TubeCollection>,
    metadata: tf::TubeMetadata,
    module_references: HashMap<BigUint, (BigUint, tf::ModulePath)>,
    function_entries: HashMap<BigUint, FunctionEntry>,
    record_entries: HashMap<BigUint, RecordEntry>,
    enum_entries: HashMap<BigUint, EnumEntry>,
    enum_variant_references: HashMap<BigUint, (BigUint, tf::Identifier)>,
    record_field_references: HashMap<BigUint, (BigUint, tf::Identifier)>,
    enum_variant_record_field_references: HashMap<BigUint, (BigUint, tf::Identifier)>,
    trait_entries: HashMap<BigUint, TraitEntry>,
    trait_method_references: HashMap<BigUint, (BigUint, tf::Identifier, tf::ErasedSignature)>,
    instance_entries: HashMap<BigUint, InstanceEntry>,
    instance_method_references: HashMap<BigUint, (BigUint, tf::Identifier, tf::ErasedSignature)>,
    tube_ids: HashMap<BigUint, TubeName>,
    module_ids: HashMap<BigUint, (TubeName, ModulePath)>,
    functions: HashMap<BigUint, Arc<dyn Function>>,
    local_import_ids: HashMap<BigUint, UniqueIdentifier>,
    local_variables: HashMap<BigUint, Arc<LocalVariable<DefaultExprContext>>>,
}

enum FunctionEntry {
    Definition(tf::FunctionDefinition),
    Reference(tf::ImportSpecifier),
}

#[allow(dead_code)]
enum RecordEntry {
    Definition(tf::RecordDefinition),
    Reference(tf::ImportSpecifier),
}

#[allow(dead_code)]
enum EnumEntry {
    Definition(tf::EnumDefinition),
    Reference(tf::ImportSpecifier),
}

#[allow(dead_code)]
enum TraitEntry {
    Definition(tf::TraitDefinition),
    Reference(tf::ImportSpecifier),
}

#[allow(dead_code)]
enum InstanceEntry {
    Definition(tf::InstanceDefinition),
    Reference(tf::ImportSpecifier),
}

impl TubeDecoder {
    fn new(tube_collection: Arc<TubeCollection>, metadata: tf::TubeMetadata) -> Self {
        Self {
            tube_collection,
            metadata,
            module_references: HashMap::new(),
            function_entries: HashMap::new(),
            record_entries: HashMap::new(),
            enum_entries: HashMap::new(),
            enum_variant_references: HashMap::new(),
            record_field_references: HashMap::new(),
            enum_variant_record_field_references: HashMap::new(),
            trait_entries: HashMap::new(),
            trait_method_references: HashMap::new(),
            instance_entries: HashMap::new(),
            instance_method_references: HashMap::new(),
            tube_ids: HashMap::new(),
            module_ids: HashMap::new(),
            functions: HashMap::new(),
            local_import_ids: HashMap::new(),
            local_variables: HashMap::new(),
        }
    }

    fn decode(&mut self, tube: impl Iterator<Item = tf::TubeFileEntry>, tube_collection_builder: &TubeCollectionBuilder) -> Arc<Tube> {
        self.read_remaining_entries(tube);
        self.create_tube(tube_collection_builder)
    }

    fn read_remaining_entries(&mut self, tube: impl Iterator<Item = tf::TubeFileEntry>) {
        for entry in tube {
            self.store_entry(entry);
        }
    }

    fn store_entry(&mut self, entry: tf::TubeFileEntry) {
        match entry {
            tf::TubeFileEntry::Header { .. } => panic!("extra tube version entry"),
            tf::TubeFileEntry::Metadata { .. } => panic!("extra tube metadata entry"),
            tf::TubeFileEntry::ModuleReference {
                module_id,
                tube_id,
                path,
            } => {
                insert_unique(
                    &mut self.module_references,
                    module_id,
                    (tube_id, *path),
                    "module reference",
                );
            }
            tf::TubeFileEntry::FunctionDefinition { definition } => {
                insert_unique(
                    &mut self.function_entries,
                    definition.function_id.clone(),
                    FunctionEntry::Definition(*definition),
                    "function entry",
                );
            }
            tf::TubeFileEntry::FunctionReference {
                function_id,
                import,
            } => {
                insert_unique(
                    &mut self.function_entries,
                    function_id,
                    FunctionEntry::Reference(*import),
                    "function entry",
                );
            }
            tf::TubeFileEntry::RecordDefinition { definition } => {
                insert_unique(
                    &mut self.record_entries,
                    definition.record_id.clone(),
                    RecordEntry::Definition(*definition),
                    "record entry",
                );
            }
            tf::TubeFileEntry::RecordReference { record_id, import } => {
                insert_unique(
                    &mut self.record_entries,
                    record_id,
                    RecordEntry::Reference(*import),
                    "record entry",
                );
            }
            tf::TubeFileEntry::EnumDefinition { definition } => {
                insert_unique(
                    &mut self.enum_entries,
                    definition.enum_id.clone(),
                    EnumEntry::Definition(*definition),
                    "enum entry",
                );
            }
            tf::TubeFileEntry::EnumReference { enum_id, import } => {
                insert_unique(
                    &mut self.enum_entries,
                    enum_id,
                    EnumEntry::Reference(*import),
                    "enum entry",
                );
            }
            tf::TubeFileEntry::EnumVariantReference {
                variant_id,
                enum_id,
                name,
            } => {
                insert_unique(
                    &mut self.enum_variant_references,
                    variant_id,
                    (enum_id, *name),
                    "enum variant reference",
                );
            }
            tf::TubeFileEntry::RecordFieldReference {
                record_field_id,
                record_id,
                name,
            } => {
                insert_unique(
                    &mut self.record_field_references,
                    record_field_id,
                    (record_id, *name),
                    "record field reference",
                );
            }
            tf::TubeFileEntry::EnumVariantRecordFieldReference {
                record_field_id,
                variant_id,
                name,
            } => {
                insert_unique(
                    &mut self.enum_variant_record_field_references,
                    record_field_id,
                    (variant_id, *name),
                    "enum variant record field reference",
                );
            }
            tf::TubeFileEntry::TraitDefinition { definition } => {
                insert_unique(
                    &mut self.trait_entries,
                    definition.trait_id.clone(),
                    TraitEntry::Definition(*definition),
                    "trait entry",
                );
            }
            tf::TubeFileEntry::TraitReference { trait_id, import } => {
                insert_unique(
                    &mut self.trait_entries,
                    trait_id,
                    TraitEntry::Reference(*import),
                    "trait entry",
                );
            }
            tf::TubeFileEntry::TraitMethodReference {
                method_id,
                trait_id,
                name,
                signature,
            } => {
                insert_unique(
                    &mut self.trait_method_references,
                    method_id,
                    (trait_id, *name, *signature),
                    "trait method reference",
                );
            }
            tf::TubeFileEntry::InstanceDefinition { definition } => {
                insert_unique(
                    &mut self.instance_entries,
                    definition.instance_id.clone(),
                    InstanceEntry::Definition(*definition),
                    "instance entry",
                );
            }
            tf::TubeFileEntry::InstanceReference {
                instance_id,
                import,
            } => {
                insert_unique(
                    &mut self.instance_entries,
                    instance_id,
                    InstanceEntry::Reference(*import),
                    "instance entry",
                );
            }
            tf::TubeFileEntry::InstanceMethodReference {
                method_id,
                instance_id,
                name,
                signature,
            } => {
                insert_unique(
                    &mut self.instance_method_references,
                    method_id,
                    (instance_id, *name, *signature),
                    "instance method reference",
                );
            }
        }
    }

    fn create_tube(&mut self, tube_collection_builder: &TubeCollectionBuilder) -> Arc<Tube> {
        let metadata = self.metadata.clone();
        let tube_name = decode_tube_name(*metadata.name);

        self.tube_ids.insert(BigUint::from(0u32), tube_name.clone());
        let referenced_tubes = metadata
            .referenced_tubes
            .iter()
            .enumerate()
            .map(|(index, name)| {
                let tube_name = decode_tube_name((**name).clone());
                self.tube_ids
                    .insert(BigUint::from(index + 1), tube_name.clone());
                tube_name
            })
            .collect::<Vec<_>>();
        let modules = metadata.modules;

        let platform = metadata
            .platform_metadata
            .into_iter()
            .map(|(platform, expr)| (platform, expr.into_inner()))
            .collect();

        let tb = tube_collection_builder.add_tube(
            tube_name.clone(),
            TubeMetadata { platform },
            referenced_tubes,
        );

        for module in &modules {
            let path = decode_module_path((*module.path).clone());
            self.module_ids.insert(
                BigUint::from(self.module_ids.len()),
                (tube_name.clone(), path.clone()),
            );
            tb.module(path);
        }

        for module in modules {
            let path = decode_module_path(*module.path);
            let mb = tb.module(path);

            for group in module.groups {
                let name = decode_identifier(*group.name);
                for export in group.exports {
                    mb.add_export(name.clone(), self.decode_module_export(*export, false));
                }
            }
        }

        tb.tube()
    }

    fn decode_module_export(
        &mut self,
        module_export: tf::ModuleExport,
        is_reexport: bool,
    ) -> ModuleExportEntry {
        match module_export {
            tf::ModuleExport::Function {
                function_id,
                access,
                ..
            } => ModuleExportEntry {
                access: decode_access_modifier_global(*access),
                binding: ModuleExportBinding::Function(self.function(function_id)),
                is_reexport,
            },
            tf::ModuleExport::Record {
                record_id, access, ..
            } => ModuleExportEntry {
                access: decode_access_modifier_global(*access),
                binding: ModuleExportBinding::Record(self.record(record_id)),
                is_reexport,
            },
            tf::ModuleExport::Enum {
                enum_id, access, ..
            } => ModuleExportEntry {
                access: decode_access_modifier_global(*access),
                binding: ModuleExportBinding::Enum(self.enum_decl(enum_id)),
                is_reexport,
            },
            tf::ModuleExport::Trait {
                trait_id, access, ..
            } => ModuleExportEntry {
                access: decode_access_modifier_global(*access),
                binding: ModuleExportBinding::Trait(self.trait_decl(trait_id)),
                is_reexport,
            },
            tf::ModuleExport::Instance {
                instance_id,
                access,
                ..
            } => ModuleExportEntry {
                access: decode_access_modifier_global(*access),
                binding: ModuleExportBinding::Instance(self.instance(instance_id)),
                is_reexport,
            },
            tf::ModuleExport::Exported { exp } => self.decode_module_export(*exp, true),
        }
    }

    fn decode_function_definition(
        &mut self,
        definition: tf::FunctionDefinition,
    ) -> Arc<dyn Function> {
        let function = Arc::new(DecodedFunction::new(definition.function_id.clone()));

        function
            .metadata
            .set(FunctionMetadata {
                is_inline: definition.inline,
                erasure_mode: decode_erasure_mode(*definition.erasure),
                is_witness: definition.witness,
                effect_info: decode_effect_info(*definition.effects),
            })
            .ok()
            .expect("function defined more than once");

        function
            .import
            .set(self.decode_import_specifier(*definition.import))
            .ok()
            .expect("function import set more than once");

        function
            .signature
            .set(Arc::new(
                self.decode_function_signature(*definition.signature),
            ))
            .ok()
            .expect("function signature set more than once");

        let implementation = definition
            .implementation
            .map(|implementation| Arc::new(self.decode_function_implementation(*implementation)));

        function
            .implementation
            .set(implementation)
            .ok()
            .expect("function implementation set more than once");

        function
    }

    fn function(&mut self, id: BigUint) -> Arc<dyn Function> {
        if let Some(function) = self.functions.get(&id) {
            return function.clone();
        }

        let function = match self
            .function_entries
            .remove(&id)
            .unwrap_or_else(|| panic!("unknown function id {id}"))
        {
            FunctionEntry::Definition(definition) => self.decode_function_definition(definition),
            FunctionEntry::Reference(import) => self.resolve_function_import(import),
        };

        self.functions.insert(id, function.clone());
        function
    }

    fn resolve_function_import(&mut self, import: tf::ImportSpecifier) -> Arc<dyn Function> {
        match self.decode_import_specifier(import) {
            ImportSpecifier::Global {
                tube,
                module,
                name,
                signature,
            } => {
                let tube = self
                    .tube_collection
                    .tube(&tube)
                    .unwrap_or_else(|| panic!("function import references unknown tube {tube}"));
                let module = tube.module(&module).unwrap_or_else(|| {
                    panic!("function import references unknown module {module}")
                });
                let exports = module
                    .named_exports(&name)
                    .unwrap_or_else(|| panic!("function import references unknown export"));

                exports
                    .iter()
                    .find_map(|entry| match &entry.binding {
                        ModuleExportBinding::Function(function)
                            if &erase_signature(function.clone().signature().as_ref())
                                == signature.as_ref() =>
                        {
                            Some(function.clone())
                        }
                        _ => None,
                    })
                    .unwrap_or_else(|| panic!("function import references unknown overload"))
            }
            ImportSpecifier::Local { .. } => todo!("resolve local function import"),
        }
    }

    fn record(&mut self, id: BigUint) -> Arc<dyn argon_compiler::Record> {
        match self
            .record_entries
            .remove(&id)
            .unwrap_or_else(|| panic!("unknown record id {id}"))
        {
            RecordEntry::Definition(_) => todo!("decode record definition"),
            RecordEntry::Reference(_) => todo!("decode record reference"),
        }
    }

    fn enum_decl(&mut self, id: BigUint) -> Arc<dyn argon_compiler::Enum> {
        match self
            .enum_entries
            .remove(&id)
            .unwrap_or_else(|| panic!("unknown enum id {id}"))
        {
            EnumEntry::Definition(_) => todo!("decode enum definition"),
            EnumEntry::Reference(_) => todo!("decode enum reference"),
        }
    }

    fn trait_decl(&mut self, id: BigUint) -> Arc<dyn argon_compiler::Trait> {
        match self
            .trait_entries
            .remove(&id)
            .unwrap_or_else(|| panic!("unknown trait id {id}"))
        {
            TraitEntry::Definition(_) => todo!("decode trait definition"),
            TraitEntry::Reference(_) => todo!("decode trait reference"),
        }
    }

    fn instance(&mut self, id: BigUint) -> Arc<dyn argon_compiler::Instance> {
        match self
            .instance_entries
            .remove(&id)
            .unwrap_or_else(|| panic!("unknown instance id {id}"))
        {
            InstanceEntry::Definition(_) => todo!("decode instance definition"),
            InstanceEntry::Reference(_) => todo!("decode instance reference"),
        }
    }

    fn module(&mut self, id: BigUint) -> (TubeName, ModulePath) {
        if let Some(module) = self.module_ids.get(&id) {
            return module.clone();
        }

        let (tube_id, path) = self
            .module_references
            .remove(&id)
            .unwrap_or_else(|| panic!("unknown module id {id}"));
        let tube_name = self
            .tube_ids
            .get(&tube_id)
            .unwrap_or_else(|| panic!("module reference has unknown tube id {tube_id}"))
            .clone();
        let module = (tube_name, decode_module_path(path));
        self.module_ids.insert(id, module.clone());
        module
    }

    fn local_import_id(&mut self, id: BigUint) -> UniqueIdentifier {
        self.local_import_ids
            .entry(id)
            .or_insert_with(UniqueIdentifier::new)
            .clone()
    }

    fn decode_import_specifier(&mut self, import: tf::ImportSpecifier) -> ImportSpecifier {
        match import {
            tf::ImportSpecifier::Global {
                module_id,
                name,
                sig,
            } => {
                let (tube, module) = self.module(module_id);

                ImportSpecifier::Global {
                    tube,
                    module,
                    name: decode_identifier(*name),
                    signature: Box::new(self.decode_erased_signature(*sig)),
                }
            }
            tf::ImportSpecifier::Local { parent, index } => ImportSpecifier::Local {
                parent: Box::new(self.decode_import_specifier(*parent)),
                id: self.local_import_id(index),
            },
        }
    }

    fn decode_erased_signature(&mut self, sig: tf::ErasedSignature) -> ErasedSignature {
        ErasedSignature {
            parameters: sig
                .params
                .into_iter()
                .map(|param| self.decode_erased_signature_type(*param))
                .collect(),
            result: self.decode_erased_signature_type(*sig.result),
        }
    }

    fn decode_erased_signature_type(
        &mut self,
        sig_type: tf::ErasedSignatureType,
    ) -> ErasedSignatureType {
        match sig_type {
            tf::ErasedSignatureType::Builtin { b, args } => {
                if !args.is_empty() {
                    todo!("decode erased builtin type arguments")
                }
                ErasedSignatureType::Builtin(decode_builtin_type(*b))
            }
            tf::ErasedSignatureType::Function { input, output } => ErasedSignatureType::Function(
                Box::new(self.decode_erased_signature_type(*input)),
                Box::new(self.decode_erased_signature_type(*output)),
            ),
            tf::ErasedSignatureType::Record {
                record_import,
                args,
            } => ErasedSignatureType::Declared(
                self.decode_import_specifier(*record_import),
                args.into_iter()
                    .map(|arg| self.decode_erased_signature_type(*arg))
                    .collect(),
            ),
            tf::ErasedSignatureType::Tuple { elements } => ErasedSignatureType::Tuple(
                elements
                    .into_iter()
                    .map(|element| self.decode_erased_signature_type(*element))
                    .collect(),
            ),
            tf::ErasedSignatureType::Erased {} => ErasedSignatureType::Erased,
        }
    }

    fn decode_function_signature(
        &mut self,
        sig: tf::FunctionSignature,
    ) -> FunctionSignature<DefaultExprContext> {
        FunctionSignature {
            parameters: sig
                .parameters
                .into_iter()
                .map(|param| self.decode_signature_parameter(*param))
                .collect(),
            return_type: self.decode_expr(*sig.return_type),
            ensures_clauses: sig
                .ensures_clauses
                .into_iter()
                .map(|clause| self.decode_expr(*clause))
                .collect(),
        }
    }

    fn decode_signature_parameter(
        &mut self,
        param: tf::SignatureParameter,
    ) -> SignatureParameter<DefaultExprContext> {
        SignatureParameter {
            list_type: decode_parameter_list_type(param.list_type),
            erasure_mode: decode_erasure_mode(*param.erasure),
            bindings: param
                .bindings
                .into_iter()
                .map(|binding| ParameterBinding {
                    name: binding.name.map(|name| decode_identifier(*name)),
                    param_type: self.decode_expr(*binding.param_type),
                })
                .collect(),
            name: param.name.map(|name| decode_identifier(*name)),
            param_type: self.decode_expr(*param.param_type),
        }
    }

    fn decode_function_implementation(
        &mut self,
        implementation: tf::FunctionImplementation,
    ) -> FunctionImplementation {
        match implementation {
            tf::FunctionImplementation::Expr { body } => {
                FunctionImplementation::Expr(self.decode_expr(*body))
            }
            tf::FunctionImplementation::Extern { externs } => {
                let _ = externs;
                todo!()
            }
        }
    }

    fn decode_expr(&mut self, expr: tf::Expr) -> Expr<DefaultExprContext> {
        match expr {
            tf::Expr::Error {} => Expr::Error,
            tf::Expr::ErasedValue {} => todo!("decode erased-value expression"),
            tf::Expr::AnyType {} => Expr::AnyType,
            tf::Expr::And { .. } => todo!("decode and expression"),
            tf::Expr::BindVariable { .. } => todo!("decode bind-variable expression"),
            tf::Expr::BoolLiteral { value } => Expr::BoolLiteral(value),
            tf::Expr::Box { value, .. } => Expr::Box {
                value: Box::new(self.decode_expr(*value)),
            },
            tf::Expr::Builtin { builtin, args } => Expr::Builtin {
                builtin: decode_builtin(builtin),
                arguments: args.into_iter().map(|arg| self.decode_expr(*arg)).collect(),
            },
            tf::Expr::Boxed { t } => Expr::BoxedType {
                t: Box::new(self.decode_expr(*t)),
            },
            tf::Expr::Break { .. } => todo!("decode break expression"),
            tf::Expr::EnumType { .. } | tf::Expr::EnumVariantLiteral { .. } => {
                todo!("decode enum expressions")
            }
            tf::Expr::Next { .. } => todo!("decode next expression"),
            tf::Expr::Finally { .. } => todo!("decode finally expression"),
            tf::Expr::FunctionCall { id, args } => Expr::FunctionCall {
                function: self.function(id),
                arguments: args
                    .into_iter()
                    .map(|arg| FunctionArgument {
                        list_type: FunctionParameterListType::NormalList,
                        arg: self.decode_expr(*arg),
                    })
                    .collect(),
            },
            tf::Expr::FunctionObjectCall { f, a } => Expr::FunctionObjectCall {
                function: Box::new(self.decode_expr(*f)),
                argument: Box::new(self.decode_expr(*a)),
            },
            tf::Expr::FunctionType { a, r } => Expr::FunctionType {
                a: Box::new(self.decode_expr(*a.var_type)),
                r: Box::new(self.decode_expr(*r)),
            },
            tf::Expr::IfElse {
                condition,
                true_body,
                false_body,
                when_true_witness,
                when_false_witness,
            } => Expr::IfElse {
                when_true_var: when_true_witness
                    .map(|var| Variable::Local(self.decode_local_var(*var))),
                when_false_var: when_false_witness
                    .map(|var| Variable::Local(self.decode_local_var(*var))),
                condition: Box::new(self.decode_expr(*condition)),
                when_true: Box::new(self.decode_expr(*true_body)),
                when_false: Box::new(self.decode_expr(*false_body)),
            },
            tf::Expr::InstanceMethodCall { .. }
            | tf::Expr::InstanceSingletonType { .. }
            | tf::Expr::NewInstance { .. } => todo!("decode instance expressions"),
            tf::Expr::IntLiteral { i } => Expr::IntLiteral(i),
            tf::Expr::Is { .. } => todo!("decode is expression"),
            tf::Expr::Lambda { .. } => todo!("decode lambda expression"),
            tf::Expr::Loop { .. } => todo!("decode loop expression"),
            tf::Expr::Match { .. } => todo!("decode match expression"),
            tf::Expr::Or { .. } => todo!("decode or expression"),
            tf::Expr::Raise { ex } => Expr::Raise {
                ex: Box::new(self.decode_expr(*ex)),
            },
            tf::Expr::RecordType { .. }
            | tf::Expr::RecordFieldLoad { .. }
            | tf::Expr::RecordFieldStore { .. }
            | tf::Expr::RecordLiteral { .. } => todo!("decode record expressions"),
            tf::Expr::Redo { .. } => todo!("decode redo expression"),
            tf::Expr::RefCellType { .. }
            | tf::Expr::RefCellCreate { .. }
            | tf::Expr::RefCellLoad { .. }
            | tf::Expr::RefCellStore { .. } => todo!("decode ref-cell expressions"),
            tf::Expr::Sequence { head, tail } => {
                let exprs = std::iter::once(self.decode_expr(*head))
                    .chain(tail.into_iter().map(|expr| self.decode_expr(*expr)))
                    .collect::<Vec<_>>();
                Expr::Sequence(
                    NEVec::try_from_vec(exprs).expect("tube sequence expression is non-empty"),
                )
            }
            tf::Expr::StringLiteral { s } => Expr::StringLiteral(s.into_boxed_str()),
            tf::Expr::TraitType { .. } => todo!("decode trait type expression"),
            tf::Expr::Tuple { items } => Expr::Tuple {
                items: items
                    .into_iter()
                    .map(|item| self.decode_expr(*item))
                    .collect(),
            },
            tf::Expr::TupleElement { index, tuple } => {
                Expr::TupleElement(Box::new(self.decode_expr(*tuple)), to_usize_index(index))
            }
            tf::Expr::TypeN { n } => Expr::Type(Box::new(self.decode_expr(*n))),
            tf::Expr::TypeBigN { n } => Expr::BigType(n.into()),
            tf::Expr::Unbox { value, .. } => Expr::Unbox {
                value: Box::new(self.decode_expr(*value)),
            },
            tf::Expr::Variable { v } => Expr::Variable(self.decode_var(*v)),
            tf::Expr::VariableStore { v, value } => {
                Expr::VariableStore(self.decode_var(*v), Box::new(self.decode_expr(*value)))
            }
        }
    }

    fn decode_var(&mut self, var: tf::Var) -> Variable<DefaultExprContext> {
        match var {
            tf::Var::LocalVar { id } => Variable::Local(
                self.local_variables
                    .get(&id)
                    .cloned()
                    .expect("local variable reference has unknown id"),
            ),
            tf::Var::ParameterVar {
                owner,
                parameter_index,
                name,
                var_type,
                erasure,
                witness,
            } => Variable::Parameter(Arc::new(ParameterVariable {
                owner: self.decode_expression_owner(*owner),
                parameter_index: usize::try_from(parameter_index)
                    .expect("parameter index does not fit usize"),
                var_type: self.decode_expr(*var_type),
                name: name.map(|name| decode_identifier(*name)),
                erasure_mode: decode_erasure_mode(*erasure),
                is_witness: witness,
            })),
            tf::Var::InstanceParameterVar { .. } => todo!("decode instance parameter variable"),
            tf::Var::LambdaParameterVar { .. } => todo!("decode lambda parameter variable"),
        }
    }

    fn decode_expression_owner(
        &mut self,
        owner: tf::ExpressionOwner,
    ) -> argon_expr::ExpressionOwner<DefaultExprContext> {
        match owner {
            tf::ExpressionOwner::Func { index } => {
                let function: Arc<dyn Function> = self.function(index);
                argon_expr::ExpressionOwner::Function(function)
            }
            tf::ExpressionOwner::Rec { .. } => todo!("decode record expression owner"),
            tf::ExpressionOwner::Enum { .. } => todo!("decode enum expression owner"),
            tf::ExpressionOwner::Trait { .. } => todo!("decode trait expression owner"),
            tf::ExpressionOwner::Instance { .. } => todo!("decode instance expression owner"),
            tf::ExpressionOwner::EnumVariant { .. } => {
                todo!("decode enum variant expression owner")
            }
            tf::ExpressionOwner::Method { .. } => todo!("decode method expression owner"),
        }
    }

    fn decode_local_var(
        &mut self,
        variable: tf::LocalVar,
    ) -> Arc<LocalVariable<DefaultExprContext>> {
        let id = variable.id;
        let local_variable = Arc::new(LocalVariable {
            name: variable.name.map(|name| decode_identifier(*name)),
            var_type: self.decode_expr(*variable.var_type),
            erasure_mode: if variable.erased {
                ErasureMode::Erased
            } else {
                ErasureMode::Concrete
            },
            is_witness: variable.witness,
            is_mutable: variable.mutable,
        });

        self.local_variables.insert(id, local_variable.clone());
        local_variable
    }
}

struct DecodedFunction {
    id: BigUint,
    metadata: OnceLock<FunctionMetadata>,
    import: OnceLock<ImportSpecifier>,
    signature: OnceLock<Arc<FunctionSignature<DefaultExprContext>>>,
    implementation: OnceLock<Option<Arc<FunctionImplementation>>>,
}

impl DecodedFunction {
    fn new(id: BigUint) -> Self {
        Self {
            id,
            metadata: OnceLock::new(),
            import: OnceLock::new(),
            signature: OnceLock::new(),
            implementation: OnceLock::new(),
        }
    }
}

impl Unload for DecodedFunction {
    fn unload(&self) {}
}

impl Function for DecodedFunction {
    fn metadata(&self) -> &FunctionMetadata {
        self.metadata.get().unwrap_or(&DEFAULT_FUNCTION_METADATA)
    }

    fn import_specifier(self: Arc<Self>) -> ImportSpecifier {
        self.import
            .get()
            .unwrap_or_else(|| panic!("decoded function {} has no import specifier", self.id))
            .clone()
    }

    fn signature(self: Arc<Self>) -> Arc<FunctionSignature<DefaultExprContext>> {
        self.signature
            .get()
            .unwrap_or_else(|| todo!("decoded function reference has no full signature"))
            .clone()
    }

    fn implementation(self: Arc<Self>) -> Option<Arc<FunctionImplementation>> {
        self.implementation.get().cloned().flatten()
    }
}

static DEFAULT_FUNCTION_METADATA: FunctionMetadata = FunctionMetadata {
    is_inline: false,
    erasure_mode: ErasureMode::Concrete,
    is_witness: false,
    effect_info: EffectInfo::Pure,
};

fn read_version_entry(entry: Option<tf::TubeFileEntry>) {
    let Some(tf::TubeFileEntry::Header { header }) = entry else {
        panic!("first tube entry must be a version entry");
    };

    if header.format_version_major != BigInt::from(0)
        || header.format_version_minor != BigInt::from(0)
    {
        panic!(
            "unsupported tube format version {}.{}",
            header.format_version_major, header.format_version_minor
        );
    }
}

fn read_metadata_entry(entry: Option<tf::TubeFileEntry>) -> tf::TubeMetadata {
    let Some(tf::TubeFileEntry::Metadata { metadata }) = entry else {
        panic!("second tube entry must be a metadata entry");
    };

    *metadata
}

fn insert_unique<T>(map: &mut HashMap<BigUint, T>, key: BigUint, value: T, kind: &str) {
    if map.insert(key.clone(), value).is_some() {
        panic!("duplicate {kind} id {key}");
    }
}

fn to_usize_index(n: BigUint) -> usize {
    usize::try_from(n).expect("tube id does not fit usize")
}

fn decode_module_path(path: tf::ModulePath) -> ModulePath {
    ModulePath(path.path)
}

fn decode_tube_name(name: tf::TubeName) -> TubeName {
    TubeName(NEVec::try_from_vec(std::iter::once(name.head).chain(name.tail).collect()).unwrap())
}

fn decode_identifier(id: tf::Identifier) -> Identifier {
    match id {
        tf::Identifier::Named { s } => Identifier::Named(s),
        tf::Identifier::BinOp { op } => Identifier::BinaryOp(decode_binary_operator(op)),
        tf::Identifier::UnOp { op } => Identifier::UnaryOp(decode_unary_operator(op)),
        tf::Identifier::Extension { inner } => {
            Identifier::Extension(Box::new(decode_identifier(*inner)))
        }
        tf::Identifier::Inverse { inner } => {
            Identifier::Inverse(Box::new(decode_identifier(*inner)))
        }
        tf::Identifier::Update { inner } => Identifier::Update(Box::new(decode_identifier(*inner))),
    }
}

fn decode_binary_operator(op: tf::BinaryOperator) -> BinaryOperatorIdentifier {
    match op {
        tf::BinaryOperator::Plus => BinaryOperatorIdentifier::Plus,
        tf::BinaryOperator::Minus => BinaryOperatorIdentifier::Minus,
        tf::BinaryOperator::Mul => BinaryOperatorIdentifier::Mul,
        tf::BinaryOperator::Div => BinaryOperatorIdentifier::Div,
        tf::BinaryOperator::Equal => BinaryOperatorIdentifier::Equal,
        tf::BinaryOperator::NotEqual => BinaryOperatorIdentifier::NotEqual,
        tf::BinaryOperator::LessThan => BinaryOperatorIdentifier::LessThan,
        tf::BinaryOperator::LessThanEq => BinaryOperatorIdentifier::LessThanEq,
        tf::BinaryOperator::GreaterThan => BinaryOperatorIdentifier::GreaterThan,
        tf::BinaryOperator::GreaterThanEq => BinaryOperatorIdentifier::GreaterThanEq,
        tf::BinaryOperator::BitOr => BinaryOperatorIdentifier::BitOr,
        tf::BinaryOperator::BitXor => BinaryOperatorIdentifier::BitXOr,
        tf::BinaryOperator::BitAnd => BinaryOperatorIdentifier::BitAnd,
        tf::BinaryOperator::ShiftLeft => BinaryOperatorIdentifier::ShiftLeft,
        tf::BinaryOperator::ShiftRight => BinaryOperatorIdentifier::ShiftRight,
        tf::BinaryOperator::Concat => BinaryOperatorIdentifier::Concat,
    }
}

fn decode_unary_operator(op: tf::UnaryOperator) -> UnaryOperatorIdentifier {
    match op {
        tf::UnaryOperator::Plus => UnaryOperatorIdentifier::Plus,
        tf::UnaryOperator::Minus => UnaryOperatorIdentifier::Minus,
        tf::UnaryOperator::BitNot => UnaryOperatorIdentifier::BitNot,
        tf::UnaryOperator::LogicalNot => UnaryOperatorIdentifier::LogicalNot,
    }
}

fn decode_access_modifier_global(access: tf::AccessModifierGlobal) -> AccessModifierGlobal {
    match access {
        tf::AccessModifierGlobal::Public {} => AccessModifierGlobal::Public,
        tf::AccessModifierGlobal::Internal {} => AccessModifierGlobal::Internal,
        tf::AccessModifierGlobal::ModulePrivate {} => AccessModifierGlobal::ModulePrivate,
    }
}

fn decode_erasure_mode(mode: tf::ErasureMode) -> ErasureMode {
    match mode {
        tf::ErasureMode::Concrete {} => ErasureMode::Concrete,
        tf::ErasureMode::Erased {} => ErasureMode::Erased,
        tf::ErasureMode::Token {} => ErasureMode::Token,
    }
}

fn decode_effect_info(effect: tf::EffectInfo) -> EffectInfo {
    match effect {
        tf::EffectInfo::Pure {} => EffectInfo::Pure,
        tf::EffectInfo::Effectful {} => EffectInfo::Effectful,
    }
}

fn decode_parameter_list_type(
    list_type: tf::FunctionParameterListType,
) -> FunctionParameterListType {
    match list_type {
        tf::FunctionParameterListType::NormalList => FunctionParameterListType::NormalList,
        tf::FunctionParameterListType::InferrableList => FunctionParameterListType::InferrableList,
        tf::FunctionParameterListType::QuoteList => FunctionParameterListType::QuoteList,
        tf::FunctionParameterListType::RequiresList => FunctionParameterListType::RequiresList,
    }
}

fn decode_builtin(builtin: tf::Builtin) -> Builtin {
    match builtin {
        tf::Builtin::IntType => Builtin::IntType,
        tf::Builtin::BoolType => Builtin::BoolType,
        tf::Builtin::StringType => Builtin::StringType,
        tf::Builtin::NeverType => Builtin::NeverType,
        tf::Builtin::ArrayType => Builtin::ArrayType,
        tf::Builtin::ConjunctionType => Builtin::ConjunctionType,
        tf::Builtin::DisjunctionType => Builtin::DisjunctionType,
        tf::Builtin::IntNegate => Builtin::IntNegate,
        tf::Builtin::IntBitNot => Builtin::IntBitNot,
        tf::Builtin::IntAdd => Builtin::IntAdd,
        tf::Builtin::IntSub => Builtin::IntSub,
        tf::Builtin::IntMul => Builtin::IntMul,
        tf::Builtin::IntBitAnd => Builtin::IntBitAnd,
        tf::Builtin::IntBitOr => Builtin::IntBitOr,
        tf::Builtin::IntBitXor => Builtin::IntBitXor,
        tf::Builtin::IntBitShiftLeft => Builtin::IntBitShiftLeft,
        tf::Builtin::IntBitShiftRight => Builtin::IntBitShiftRight,
        tf::Builtin::IntEq => Builtin::IntEq,
        tf::Builtin::IntNe => Builtin::IntNe,
        tf::Builtin::IntLt => Builtin::IntLt,
        tf::Builtin::IntLe => Builtin::IntLe,
        tf::Builtin::IntGt => Builtin::IntGt,
        tf::Builtin::IntGe => Builtin::IntGe,
        tf::Builtin::StringConcat => Builtin::StringConcat,
        tf::Builtin::StringEq => Builtin::StringEq,
        tf::Builtin::StringNe => Builtin::StringNe,
        tf::Builtin::BoolNot => Builtin::BoolNot,
        tf::Builtin::BoolEq => Builtin::BoolEq,
        tf::Builtin::BoolNe => Builtin::BoolNe,
        tf::Builtin::ArrayCreateUnsafeUninitialized => Builtin::ArrayCreateUnsafeUninitialized,
        tf::Builtin::ArrayLength => Builtin::ArrayLength,
        tf::Builtin::ArrayGet => Builtin::ArrayGet,
        tf::Builtin::ArraySet => Builtin::ArraySet,
        tf::Builtin::EqualTo => Builtin::EqualToType,
        tf::Builtin::EqualToRefl => todo!("decode equal-to-refl builtin"),
    }
}

fn decode_builtin_type(builtin_type: tf::BuiltinType) -> Builtin {
    match builtin_type {
        tf::BuiltinType::Int {} => Builtin::IntType,
        tf::BuiltinType::Bool {} => Builtin::BoolType,
        tf::BuiltinType::String {} => Builtin::StringType,
        tf::BuiltinType::Never {} => Builtin::NeverType,
        tf::BuiltinType::Array {} => Builtin::ArrayType,
        tf::BuiltinType::Conjunction {} => Builtin::ConjunctionType,
        tf::BuiltinType::Disjunction {} => Builtin::DisjunctionType,
    }
}
