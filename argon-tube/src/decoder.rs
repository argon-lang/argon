use alloc::{boxed::Box, sync::Arc, vec::Vec};
use argon_compiler::erased_sig::{
    ErasedSignature, ErasedSignatureType, ImportSpecifier, erase_signature,
};
use argon_compiler::platform::PlatformExtern;
use argon_compiler::signature::{ParameterBinding, SignatureParameter};
use argon_compiler::{
    AccessModifierGlobal, BinaryOperatorIdentifier, Builtin, Context, DefaultExprContext,
    EffectInfo, Enum, EnumVariant, EnumVariantMetadata, ErasureMode, Expr, Function,
    FunctionImplementation, FunctionMetadata, FunctionParameterListType, FunctionSignature,
    Identifier, Instance, Method, MethodEntry, MethodInstanceParameter, MethodMetadata,
    MethodOwner, MethodSlot, ModuleExportBinding, ModuleExportEntry, ModulePath, Record,
    RecordField, RecordFieldMetadata, RecordFieldOwner, Trait, Tube, TubeCollection,
    TubeCollectionBuilder, TubeMetadata, TubeName, UnaryOperatorIdentifier, Unload,
    access::AccessModifier,
};
use argon_expr::{
    BlockLabel, BlockLabelKind, ClosureParameterVariable, EnumType, InstanceParameterVariable,
    LocalVariable, MethodInstanceType, ParameterVariable, Pattern, RecordFieldLiteral,
    RecordFieldPattern, RecordType, TraitType, Variable,
};
use argon_format::tube as tf;
use argon_util::UniqueIdentifier;
use argon_util::sync::{Mutex, RwLock, mutex_lock, rwlock_read, rwlock_write};
use core::fmt::Debug;
use core::iter;
use hashbrown::HashMap;
use mitsein::vec1::Vec1;
use num_bigint::{BigInt, BigUint};

pub fn decode_tube(
    context: Context,
    mut tube: impl Iterator<Item = tf::TubeFileEntry>,
    tube_collection_builder: &TubeCollectionBuilder,
) -> Arc<Tube> {
    read_version_entry(tube.next());
    let metadata = read_metadata_entry(tube.next());

    let mut decoder =
        TubeDecoder::new(context, tube_collection_builder.tube_collection(), metadata);
    decoder.read_remaining_entries(tube);
    Arc::new(decoder).create_tube(tube_collection_builder)
}

struct TubeDecoder {
    context: Context,
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
    method_entries: HashMap<BigUint, MethodEntryDefinition>,
    instance_entries: HashMap<BigUint, InstanceEntry>,
    instance_method_references: HashMap<BigUint, (BigUint, tf::Identifier, tf::ErasedSignature)>,
    tube_ids: RwLock<HashMap<BigUint, TubeName>>,
    module_ids: RwLock<HashMap<BigUint, (TubeName, ModulePath)>>,
    functions: RwLock<HashMap<BigUint, Arc<dyn Function>>>,
    records: RwLock<HashMap<BigUint, Arc<dyn Record>>>,
    enums: RwLock<HashMap<BigUint, Arc<dyn Enum>>>,
    enum_variants: RwLock<HashMap<BigUint, Arc<dyn EnumVariant>>>,
    traits: RwLock<HashMap<BigUint, Arc<dyn Trait>>>,
    instances: RwLock<HashMap<BigUint, Arc<dyn Instance>>>,
    methods: RwLock<HashMap<BigUint, Arc<dyn Method>>>,
    record_fields: RwLock<HashMap<BigUint, Arc<dyn RecordField>>>,
    local_import_ids: RwLock<HashMap<BigUint, UniqueIdentifier>>,
    local_variables: RwLock<HashMap<BigUint, Box<LocalVariable<DefaultExprContext>>>>,
    closure_parameters: RwLock<HashMap<BigUint, Box<ClosureParameterVariable<DefaultExprContext>>>>,
    block_labels: RwLock<HashMap<BigUint, Box<BlockLabel<DefaultExprContext>>>>,
}

#[derive(Clone)]
enum FunctionEntry {
    Definition(tf::FunctionDefinition),
    Reference(tf::ImportSpecifier),
}

#[allow(dead_code)]
#[derive(Clone)]
enum RecordEntry {
    Definition(tf::RecordDefinition),
    Reference(tf::ImportSpecifier),
}

#[allow(dead_code)]
#[derive(Clone)]
enum EnumEntry {
    Definition(tf::EnumDefinition),
    Reference(tf::ImportSpecifier),
}

#[allow(dead_code)]
#[derive(Clone)]
enum TraitEntry {
    Definition(tf::TraitDefinition),
    Reference(tf::ImportSpecifier),
}

#[allow(dead_code)]
#[derive(Clone)]
enum InstanceEntry {
    Definition(tf::InstanceDefinition),
    Reference(tf::ImportSpecifier),
}

#[derive(Clone)]
enum MethodEntryDefinition {
    Trait {
        trait_id: BigUint,
        entry: tf::MethodEntry,
    },
    Instance {
        instance_id: BigUint,
        entry: tf::MethodEntry,
    },
}

impl TubeDecoder {
    fn new(
        context: Context,
        tube_collection: Arc<TubeCollection>,
        metadata: tf::TubeMetadata,
    ) -> Self {
        Self {
            context,
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
            method_entries: HashMap::new(),
            instance_entries: HashMap::new(),
            instance_method_references: HashMap::new(),
            tube_ids: RwLock::new(HashMap::new()),
            module_ids: RwLock::new(HashMap::new()),
            functions: RwLock::new(HashMap::new()),
            records: RwLock::new(HashMap::new()),
            enums: RwLock::new(HashMap::new()),
            enum_variants: RwLock::new(HashMap::new()),
            traits: RwLock::new(HashMap::new()),
            instances: RwLock::new(HashMap::new()),
            methods: RwLock::new(HashMap::new()),
            record_fields: RwLock::new(HashMap::new()),
            local_import_ids: RwLock::new(HashMap::new()),
            local_variables: RwLock::new(HashMap::new()),
            closure_parameters: RwLock::new(HashMap::new()),
            block_labels: RwLock::new(HashMap::new()),
        }
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
                let definition = *definition;
                for method in &definition.methods {
                    insert_unique(
                        &mut self.method_entries,
                        method.id.clone(),
                        MethodEntryDefinition::Trait {
                            trait_id: definition.trait_id.clone(),
                            entry: (**method).clone(),
                        },
                        "method entry",
                    );
                }
                insert_unique(
                    &mut self.trait_entries,
                    definition.trait_id.clone(),
                    TraitEntry::Definition(definition),
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
                let definition = *definition;
                for method in &definition.methods {
                    insert_unique(
                        &mut self.method_entries,
                        method.id.clone(),
                        MethodEntryDefinition::Instance {
                            instance_id: definition.instance_id.clone(),
                            entry: (**method).clone(),
                        },
                        "method entry",
                    );
                }
                insert_unique(
                    &mut self.instance_entries,
                    definition.instance_id.clone(),
                    InstanceEntry::Definition(definition),
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

    fn create_tube(self: &Arc<Self>, tube_collection_builder: &TubeCollectionBuilder) -> Arc<Tube> {
        let metadata = self.metadata.clone();
        let tube_name = decode_tube_name(*metadata.name);

        rwlock_write(&self.tube_ids).insert(BigUint::from(0u32), tube_name.clone());
        let referenced_tubes = metadata
            .referenced_tubes
            .iter()
            .enumerate()
            .map(|(index, name)| {
                let tube_name = decode_tube_name((**name).clone());
                rwlock_write(&self.tube_ids).insert(BigUint::from(index + 1), tube_name.clone());
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
            let mut module_ids = rwlock_write(&self.module_ids);
            let id = BigUint::from(module_ids.len());
            module_ids.insert(id, (tube_name.clone(), path.clone()));
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
        self: &Arc<Self>,
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
        self: &Arc<Self>,
        definition: tf::FunctionDefinition,
    ) -> Arc<dyn Function> {
        Arc::new(DecodedFunction::new(self.clone(), definition))
    }

    fn function(self: &Arc<Self>, id: BigUint) -> Arc<dyn Function> {
        if let Some(function) = rwlock_read(&self.functions).get(&id).cloned() {
            return function;
        }

        let function = match self
            .function_entries
            .get(&id)
            .unwrap_or_else(|| panic!("unknown function id {id}"))
            .clone()
        {
            FunctionEntry::Definition(definition) => {
                let mut functions = rwlock_write(&self.functions);
                if let Some(function) = functions.get(&id) {
                    return function.clone();
                }

                let function = self.decode_function_definition(definition);
                functions.insert(id, function.clone());
                return function;
            }
            FunctionEntry::Reference(import) => self.resolve_function_import(import),
        };

        rwlock_write(&self.functions).insert(id, function.clone());
        function
    }

    fn resolve_function_import(self: &Arc<Self>, import: tf::ImportSpecifier) -> Arc<dyn Function> {
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

                let export_groups = module.export_groups();
                let exports = export_groups
                    .get(&name)
                    .unwrap_or_else(|| panic!("function import references unknown export"));

                exports
                    .iter()
                    .find_map(|entry| match &entry.binding {
                        ModuleExportBinding::Function(function)
                            if &erase_signature(
                                self.context.clone(),
                                function.clone().signature().as_ref(),
                            ) == signature.as_ref() =>
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

    fn record(self: &Arc<Self>, id: BigUint) -> Arc<dyn argon_compiler::Record> {
        if let Some(record) = rwlock_read(&self.records).get(&id).cloned() {
            return record;
        }

        let record = match self
            .record_entries
            .get(&id)
            .unwrap_or_else(|| panic!("unknown record id {id}"))
            .clone()
        {
            RecordEntry::Definition(definition) => {
                let mut records = rwlock_write(&self.records);
                if let Some(record) = records.get(&id) {
                    return record.clone();
                }

                let record: Arc<dyn Record> =
                    Arc::new(DecodedRecord::new(self.clone(), definition));
                records.insert(id, record.clone());
                return record;
            }
            RecordEntry::Reference(import) => self.resolve_record_import(import),
        };

        rwlock_write(&self.records).insert(id, record.clone());
        record
    }

    fn resolve_record_import(self: &Arc<Self>, import: tf::ImportSpecifier) -> Arc<dyn Record> {
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
                    .unwrap_or_else(|| panic!("record import references unknown tube {tube}"));
                let module = tube
                    .module(&module)
                    .unwrap_or_else(|| panic!("record import references unknown module {module}"));

                let export_groups = module.export_groups();
                let exports = export_groups
                    .get(&name)
                    .unwrap_or_else(|| panic!("record import references unknown export"));

                exports
                    .iter()
                    .find_map(|entry| match &entry.binding {
                        ModuleExportBinding::Record(record)
                            if &erase_signature(
                                self.context.clone(),
                                record.clone().signature().as_ref(),
                            ) == signature.as_ref() =>
                        {
                            Some(record.clone())
                        }
                        _ => None,
                    })
                    .unwrap_or_else(|| panic!("record import references unknown overload"))
            }
            ImportSpecifier::Local { .. } => todo!("resolve local record import"),
        }
    }

    fn record_field(self: &Arc<Self>, id: BigUint) -> Arc<dyn RecordField> {
        if let Some(record_field) = rwlock_read(&self.record_fields).get(&id).cloned() {
            return record_field;
        }

        let record_field = if let Some((record_id, name)) = self.record_field_references.get(&id) {
            let name = decode_identifier(name.clone());
            let record = self.record(record_id.clone());
            record
                .clone()
                .fields()
                .iter()
                .find(|field| field.metadata().name == name)
                .unwrap_or_else(|| panic!("record field reference has unknown field {name:?}"))
                .clone()
        } else if let Some((variant_id, name)) = self.enum_variant_record_field_references.get(&id)
        {
            let name = decode_identifier(name.clone());
            let variant = self.enum_variant(variant_id.clone());
            variant
                .clone()
                .fields()
                .iter()
                .find(|field| field.metadata().name == name)
                .unwrap_or_else(|| {
                    panic!("enum variant field reference has unknown field {name:?}")
                })
                .clone()
        } else {
            panic!("unknown record field id {id}");
        };

        rwlock_write(&self.record_fields).insert(id, record_field.clone());
        record_field
    }

    fn enum_decl(self: &Arc<Self>, id: BigUint) -> Arc<dyn argon_compiler::Enum> {
        if let Some(enum_) = rwlock_read(&self.enums).get(&id).cloned() {
            return enum_;
        }

        let enum_ = match self
            .enum_entries
            .get(&id)
            .unwrap_or_else(|| panic!("unknown enum id {id}"))
            .clone()
        {
            EnumEntry::Definition(definition) => {
                let mut enums = rwlock_write(&self.enums);
                if let Some(enum_) = enums.get(&id) {
                    return enum_.clone();
                }

                let enum_: Arc<dyn Enum> = Arc::new(DecodedEnum::new(self.clone(), definition));
                enums.insert(id, enum_.clone());
                return enum_;
            }
            EnumEntry::Reference(import) => self.resolve_enum_import(import),
        };

        rwlock_write(&self.enums).insert(id, enum_.clone());
        enum_
    }

    fn resolve_enum_import(self: &Arc<Self>, import: tf::ImportSpecifier) -> Arc<dyn Enum> {
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
                    .unwrap_or_else(|| panic!("enum import references unknown tube {tube}"));
                let module = tube
                    .module(&module)
                    .unwrap_or_else(|| panic!("enum import references unknown module {module}"));

                let export_groups = module.export_groups();
                let exports = export_groups
                    .get(&name)
                    .unwrap_or_else(|| panic!("enum import references unknown export"));

                exports
                    .iter()
                    .find_map(|entry| match &entry.binding {
                        ModuleExportBinding::Enum(enum_)
                            if &erase_signature(
                                self.context.clone(),
                                enum_.clone().signature().as_ref(),
                            ) == signature.as_ref() =>
                        {
                            Some(enum_.clone())
                        }
                        _ => None,
                    })
                    .unwrap_or_else(|| panic!("enum import references unknown overload"))
            }
            ImportSpecifier::Local { .. } => todo!("resolve local enum import"),
        }
    }

    fn enum_variant(self: &Arc<Self>, id: BigUint) -> Arc<dyn EnumVariant> {
        if let Some(variant) = rwlock_read(&self.enum_variants).get(&id).cloned() {
            return variant;
        }

        let (enum_id, name) = self
            .enum_variant_references
            .get(&id)
            .unwrap_or_else(|| panic!("unknown enum variant id {id}"))
            .clone();
        let name = decode_identifier(name);
        let enum_ = self.enum_decl(enum_id);
        let variant = enum_
            .clone()
            .variants()
            .iter()
            .find(|variant| variant.metadata().name == name)
            .unwrap_or_else(|| panic!("enum variant reference has unknown variant {name:?}"))
            .clone();

        rwlock_write(&self.enum_variants).insert(id, variant.clone());
        variant
    }

    fn trait_decl(self: &Arc<Self>, id: BigUint) -> Arc<dyn argon_compiler::Trait> {
        if let Some(trait_) = rwlock_read(&self.traits).get(&id).cloned() {
            return trait_;
        }

        let trait_ = match self
            .trait_entries
            .get(&id)
            .unwrap_or_else(|| panic!("unknown trait id {id}"))
            .clone()
        {
            TraitEntry::Definition(definition) => {
                let mut traits = rwlock_write(&self.traits);
                if let Some(trait_) = traits.get(&id) {
                    return trait_.clone();
                }

                let trait_: Arc<dyn Trait> = Arc::new(DecodedTrait::new(self.clone(), definition));
                traits.insert(id, trait_.clone());
                return trait_;
            }
            TraitEntry::Reference(import) => self.resolve_trait_import(import),
        };

        rwlock_write(&self.traits).insert(id, trait_.clone());
        trait_
    }

    fn resolve_trait_import(self: &Arc<Self>, import: tf::ImportSpecifier) -> Arc<dyn Trait> {
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
                    .unwrap_or_else(|| panic!("trait import references unknown tube {tube}"));
                let module = tube
                    .module(&module)
                    .unwrap_or_else(|| panic!("trait import references unknown module {module}"));

                let export_groups = module.export_groups();
                let exports = export_groups
                    .get(&name)
                    .unwrap_or_else(|| panic!("trait import references unknown export"));

                exports
                    .iter()
                    .find_map(|entry| match &entry.binding {
                        ModuleExportBinding::Trait(trait_)
                            if &erase_signature(
                                self.context.clone(),
                                trait_.clone().signature().as_ref(),
                            ) == signature.as_ref() =>
                        {
                            Some(trait_.clone())
                        }
                        _ => None,
                    })
                    .unwrap_or_else(|| panic!("trait import references unknown overload"))
            }
            ImportSpecifier::Local { .. } => todo!("resolve local trait import"),
        }
    }

    fn method(self: &Arc<Self>, id: BigUint) -> Arc<dyn Method> {
        if let Some(method) = rwlock_read(&self.methods).get(&id) {
            return method.clone();
        }

        let method = if let Some(entry) = self.method_entries.get(&id).cloned() {
            match entry {
                MethodEntryDefinition::Trait { trait_id, entry } => Arc::new(DecodedMethod::new(
                    self.clone(),
                    MethodOwner::Trait(self.trait_decl(trait_id)),
                    entry,
                ))
                    as Arc<dyn Method>,
                MethodEntryDefinition::Instance { instance_id, entry } => {
                    Arc::new(DecodedMethod::new(
                        self.clone(),
                        MethodOwner::Instance(self.instance(instance_id)),
                        entry,
                    )) as Arc<dyn Method>
                }
            }
        } else if let Some((trait_id, name, signature)) = self.trait_method_references.get(&id) {
            self.resolve_method_reference(
                self.trait_decl(trait_id.clone()).methods(),
                name.clone(),
                signature.clone(),
            )
        } else if let Some((instance_id, name, signature)) =
            self.instance_method_references.get(&id)
        {
            self.resolve_method_reference(
                self.instance(instance_id.clone()).methods(),
                name.clone(),
                signature.clone(),
            )
        } else {
            panic!("unknown method id {id}");
        };

        rwlock_write(&self.methods).insert(id, method.clone());
        method
    }

    fn resolve_method_reference(
        self: &Arc<Self>,
        methods: Arc<Vec<MethodEntry>>,
        name: tf::Identifier,
        signature: tf::ErasedSignature,
    ) -> Arc<dyn Method> {
        let name = decode_identifier(name);
        let signature = self.decode_erased_signature(signature);
        methods
            .iter()
            .find_map(|entry| {
                let method = entry.method.clone();
                if method.metadata().name == name
                    && erase_signature(self.context.clone(), method.clone().signature().as_ref())
                        == signature
                {
                    Some(method)
                } else {
                    None
                }
            })
            .unwrap_or_else(|| panic!("method reference has unknown overload {name:?}"))
    }

    fn instance(self: &Arc<Self>, id: BigUint) -> Arc<dyn argon_compiler::Instance> {
        if let Some(instance) = rwlock_read(&self.instances).get(&id).cloned() {
            return instance;
        }

        let instance = match self
            .instance_entries
            .get(&id)
            .unwrap_or_else(|| panic!("unknown instance id {id}"))
            .clone()
        {
            InstanceEntry::Definition(definition) => {
                let mut instances = rwlock_write(&self.instances);
                if let Some(instance) = instances.get(&id) {
                    return instance.clone();
                }

                let instance: Arc<dyn Instance> =
                    Arc::new(DecodedInstance::new(self.clone(), definition));
                instances.insert(id, instance.clone());
                return instance;
            }
            InstanceEntry::Reference(import) => self.resolve_instance_import(import),
        };

        rwlock_write(&self.instances).insert(id, instance.clone());
        instance
    }

    fn resolve_instance_import(self: &Arc<Self>, import: tf::ImportSpecifier) -> Arc<dyn Instance> {
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
                    .unwrap_or_else(|| panic!("instance import references unknown tube {tube}"));
                let module = tube.module(&module).unwrap_or_else(|| {
                    panic!("instance import references unknown module {module}")
                });

                let export_groups = module.export_groups();
                let exports = export_groups
                    .get(&name)
                    .unwrap_or_else(|| panic!("instance import references unknown export"));

                exports
                    .iter()
                    .find_map(|entry| match &entry.binding {
                        ModuleExportBinding::Instance(instance)
                            if &erase_signature(
                                self.context.clone(),
                                instance.clone().signature().as_ref(),
                            ) == signature.as_ref() =>
                        {
                            Some(instance.clone())
                        }
                        _ => None,
                    })
                    .unwrap_or_else(|| panic!("instance import references unknown overload"))
            }
            ImportSpecifier::Local { .. } => todo!("resolve local instance import"),
        }
    }

    fn module(self: &Arc<Self>, id: BigUint) -> (TubeName, ModulePath) {
        if let Some(module) = rwlock_read(&self.module_ids).get(&id).cloned() {
            return module;
        }

        let (tube_id, path) = self
            .module_references
            .get(&id)
            .unwrap_or_else(|| panic!("unknown module id {id}"))
            .clone();
        let tube_name = rwlock_read(&self.tube_ids)
            .get(&tube_id)
            .unwrap_or_else(|| panic!("module reference has unknown tube id {tube_id}"))
            .clone();
        let module = (tube_name, decode_module_path(path));
        rwlock_write(&self.module_ids).insert(id, module.clone());
        module
    }

    fn local_import_id(self: &Arc<Self>, id: BigUint) -> UniqueIdentifier {
        if let Some(unique_id) = rwlock_read(&self.local_import_ids).get(&id).cloned() {
            return unique_id;
        }

        let mut local_import_ids = rwlock_write(&self.local_import_ids);
        local_import_ids
            .entry(id)
            .or_insert_with(UniqueIdentifier::new)
            .clone()
    }

    fn decode_import_specifier(self: &Arc<Self>, import: tf::ImportSpecifier) -> ImportSpecifier {
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

    fn decode_erased_signature(self: &Arc<Self>, sig: tf::ErasedSignature) -> ErasedSignature {
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
        self: &Arc<Self>,
        sig_type: tf::ErasedSignatureType,
    ) -> ErasedSignatureType {
        match sig_type {
            tf::ErasedSignatureType::Int {} => ErasedSignatureType::Int,
            tf::ErasedSignatureType::Bool {} => ErasedSignatureType::Bool,
            tf::ErasedSignatureType::String {} => ErasedSignatureType::String,
            tf::ErasedSignatureType::Never {} => ErasedSignatureType::Never,
            tf::ErasedSignatureType::Array { element_type } => ErasedSignatureType::Array(
                Box::new(self.decode_erased_signature_type(*element_type)),
            ),
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
        self: &Arc<Self>,
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
        self: &Arc<Self>,
        param: tf::SignatureParameter,
    ) -> SignatureParameter<DefaultExprContext> {
        SignatureParameter {
            list_type: decode_parameter_list_type(*param.list_type),
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
        self: &Arc<Self>,
        implementation: tf::FunctionImplementation,
    ) -> FunctionImplementation {
        match implementation {
            tf::FunctionImplementation::Expr { body } => {
                FunctionImplementation::Expr(self.decode_expr(*body))
            }
            tf::FunctionImplementation::Extern { externs } => {
                FunctionImplementation::Extern(PlatformExtern {
                    externs: externs
                        .externs
                        .into_iter()
                        .map(|(platform, expr)| (platform, expr.into_inner()))
                        .collect(),
                })
            }
        }
    }

    fn decode_method_implementation(
        self: &Arc<Self>,
        implementation: tf::MethodImplementation,
    ) -> FunctionImplementation {
        match implementation {
            tf::MethodImplementation::Abstract {} => FunctionImplementation::Expr(Expr::Error),
            tf::MethodImplementation::Expr { body } => {
                FunctionImplementation::Expr(self.decode_expr(*body))
            }
            tf::MethodImplementation::Extern { externs } => {
                FunctionImplementation::Extern(PlatformExtern {
                    externs: externs
                        .externs
                        .into_iter()
                        .map(|(platform, expr)| (platform, expr.into_inner()))
                        .collect(),
                })
            }
        }
    }

    fn decode_expr(self: &Arc<Self>, expr: tf::Expr) -> Expr<DefaultExprContext> {
        match expr {
            tf::Expr::Error {} => Expr::Error,
            tf::Expr::ErasedValue {} => todo!("decode erased-value expression"),
            tf::Expr::And { a, b } => Expr::And(
                Box::new(self.decode_expr(*a)),
                Box::new(self.decode_expr(*b)),
            ),
            tf::Expr::BindVariable { v, value } => {
                let value = self.decode_expr(*value);
                let variable = self.decode_local_var(*v);
                Expr::VariableBinding(variable, Box::new(value))
            }
            tf::Expr::BoolLiteral { value } => Expr::BoolLiteral(value),
            tf::Expr::Box { t, value } => Expr::Box {
                t: Box::new(self.decode_expr(*t)),
                value: Box::new(self.decode_expr(*value)),
            },
            tf::Expr::Builtin { builtin, args } => Expr::Builtin {
                builtin: decode_builtin(builtin),
                arguments: args.into_iter().map(|arg| self.decode_expr(*arg)).collect(),
            },
            tf::Expr::Boxed { t } => Expr::BoxedType(Box::new(self.decode_expr(*t))),
            tf::Expr::Block { label, body } => Expr::Block {
                label: self.decode_block_label(*label),
                body: Box::new(self.decode_expr(*body)),
            },
            tf::Expr::Break { block_id, value } => Expr::Break {
                label: self.decode_block_id(*block_id),
                value: Box::new(self.decode_expr(*value)),
            },
            tf::Expr::BreakIf {
                block_id,
                value,
                condition,
            } => Expr::BreakIf {
                label: self.decode_block_id(*block_id),
                value: Box::new(self.decode_expr(*value)),
                condition: Box::new(self.decode_expr(*condition)),
            },
            tf::Expr::EnumType { enum_type } => Expr::EnumType(EnumType {
                enum_: self.enum_decl(enum_type.id),
                arguments: enum_type
                    .args
                    .into_iter()
                    .map(|arg| self.decode_expr(*arg))
                    .collect(),
            }),
            tf::Expr::EnumVariantLiteral {
                enum_type,
                variant_id,
                args,
                fields,
            } => Expr::EnumVariantLiteral {
                enum_type: EnumType {
                    enum_: self.enum_decl(enum_type.id),
                    arguments: enum_type
                        .args
                        .into_iter()
                        .map(|arg| self.decode_expr(*arg))
                        .collect(),
                },
                variant: self.enum_variant(variant_id),
                arguments: args.into_iter().map(|arg| self.decode_expr(*arg)).collect(),
                fields: fields
                    .into_iter()
                    .map(|field| {
                        let record_field = self.record_field(field.field_id);
                        RecordFieldLiteral {
                            name: record_field.metadata().name.clone(),
                            value: self.decode_expr(*field.value),
                        }
                    })
                    .collect(),
            },
            tf::Expr::Finally { action, ensuring } => Expr::Finally {
                block_body: Box::new(self.decode_expr(*action)),
                finally_body: Box::new(self.decode_expr(*ensuring)),
            },
            tf::Expr::FunctionCall { id, args } => Expr::FunctionCall {
                function: self.function(id),
                arguments: args.into_iter().map(|arg| self.decode_expr(*arg)).collect(),
            },
            tf::Expr::FunctionObjectCall { f, a } => Expr::FunctionObjectCall {
                function: Box::new(self.decode_expr(*f)),
                argument: Box::new(self.decode_expr(*a)),
            },
            tf::Expr::FunctionType { a, r } => Expr::FunctionType {
                a: Box::new(self.decode_closure_parameter_var(*a)),
                r: Box::new(self.decode_expr(*r)),
            },
            tf::Expr::Condition {
                value,
                when_true_witness,
                when_false_witness,
            } => Expr::Condition {
                value: Box::new(self.decode_expr(*value)),
                when_true_witness: when_true_witness.map(|var| self.decode_local_var(*var)),
                when_false_witness: when_false_witness.map(|var| self.decode_local_var(*var)),
            },
            tf::Expr::IfElse {
                condition,
                true_body,
                false_body,
            } => Expr::IfElse {
                condition: Box::new(self.decode_expr(*condition)),
                when_true: Box::new(self.decode_expr(*true_body)),
                when_false: Box::new(self.decode_expr(*false_body)),
            },
            tf::Expr::InstanceMethodCall {
                method_id,
                instance_type,
                instance,
                args,
            } => Expr::MethodCall {
                method: self.method(method_id),
                instance_type: self.decode_method_instance_type(*instance_type),
                receiver: Box::new(self.decode_expr(*instance)),
                arguments: args.into_iter().map(|arg| self.decode_expr(*arg)).collect(),
            },
            tf::Expr::InstanceSingletonType { .. } => todo!("decode instance expressions"),
            tf::Expr::NewInstance { instance_id, args } => Expr::NewInstance {
                instance: self.instance(instance_id),
                arguments: args.into_iter().map(|arg| self.decode_expr(*arg)).collect(),
            },
            tf::Expr::IntLiteral { i } => Expr::IntLiteral(i),
            tf::Expr::Is { value, pattern } => Expr::Is {
                value: Box::new(self.decode_expr(*value)),
                pattern: Box::new(self.decode_pattern(*pattern)),
            },
            tf::Expr::Closure {
                v,
                return_type,
                body,
            } => Expr::Closure {
                v: Box::new(self.decode_closure_parameter_var(*v)),
                return_type: Box::new(self.decode_expr(*return_type)),
                body: Box::new(self.decode_expr(*body)),
            },
            tf::Expr::Match { .. } => todo!("decode match expression"),
            tf::Expr::Or { a, b } => Expr::Or(
                Box::new(self.decode_expr(*a)),
                Box::new(self.decode_expr(*b)),
            ),
            tf::Expr::Not { a } => Expr::Not(Box::new(self.decode_expr(*a))),
            tf::Expr::Raise { ex } => Expr::Raise {
                ex: Box::new(self.decode_expr(*ex)),
            },
            tf::Expr::RecordType { record_type } => Expr::RecordType(RecordType {
                record: self.record(record_type.id),
                arguments: record_type
                    .args
                    .into_iter()
                    .map(|arg| self.decode_expr(*arg))
                    .collect(),
            }),
            tf::Expr::RecordFieldLoad {
                record,
                field_id,
                record_value,
            } => Expr::RecordFieldLoad {
                record_type: Box::new(Expr::RecordType(RecordType {
                    record: self.record(record.id),
                    arguments: record
                        .args
                        .into_iter()
                        .map(|arg| self.decode_expr(*arg))
                        .collect(),
                })),
                field: self.record_field(field_id),
                record_value: Box::new(self.decode_expr(*record_value)),
            },
            tf::Expr::RecordFieldStore {
                record,
                field_id,
                record_value,
                field_value,
            } => Expr::RecordFieldStore {
                record_type: Box::new(Expr::RecordType(RecordType {
                    record: self.record(record.id),
                    arguments: record
                        .args
                        .into_iter()
                        .map(|arg| self.decode_expr(*arg))
                        .collect(),
                })),
                field: self.record_field(field_id),
                record_value: Box::new(self.decode_expr(*record_value)),
                new_value: Box::new(self.decode_expr(*field_value)),
            },
            tf::Expr::RecordLiteral { record, fields } => Expr::RecordLiteral {
                record_type: RecordType {
                    record: self.record(record.id),
                    arguments: record
                        .args
                        .into_iter()
                        .map(|arg| self.decode_expr(*arg))
                        .collect(),
                },
                fields: fields
                    .into_iter()
                    .map(|field| {
                        let record_field = self.record_field(field.field_id);
                        RecordFieldLiteral {
                            name: record_field.metadata().name.clone(),
                            value: self.decode_expr(*field.value),
                        }
                    })
                    .collect(),
            },
            tf::Expr::Retry { block_id } => Expr::Retry {
                label: self.decode_block_id(*block_id),
            },
            tf::Expr::RefCellType { .. }
            | tf::Expr::RefCellCreate { .. }
            | tf::Expr::RefCellLoad { .. }
            | tf::Expr::RefCellStore { .. } => todo!("decode ref-cell expressions"),
            tf::Expr::Sequence { head, tail } => {
                let exprs = iter::once(self.decode_expr(*head))
                    .chain(tail.into_iter().map(|expr| self.decode_expr(*expr)))
                    .collect::<Vec<_>>();
                Expr::Sequence(
                    Vec1::try_from(exprs).expect("tube sequence expression is non-empty"),
                )
            }
            tf::Expr::StringLiteral { s } => Expr::StringLiteral(s.into_boxed_str()),
            tf::Expr::TraitType { trait_type } => {
                Expr::TraitType(self.decode_trait_type(*trait_type))
            }
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
            tf::Expr::Unbox { t, value } => Expr::Unbox {
                t: Box::new(self.decode_expr(*t)),
                value: Box::new(self.decode_expr(*value)),
            },
            tf::Expr::Variable { v } => Expr::Variable(self.decode_var(*v)),
            tf::Expr::VariableStore { v, value } => {
                Expr::VariableStore(self.decode_var(*v), Box::new(self.decode_expr(*value)))
            }
        }
    }

    fn decode_pattern(self: &Arc<Self>, pattern: tf::Pattern) -> Pattern<DefaultExprContext> {
        match pattern {
            tf::Pattern::Error {} => Pattern::Error,
            tf::Pattern::Discard { t } => Pattern::Discard {
                t: Box::new(self.decode_expr(*t)),
            },
            tf::Pattern::Tuple { items } => Pattern::Tuple(
                items
                    .into_iter()
                    .map(|item| self.decode_pattern(*item))
                    .collect(),
            ),
            tf::Pattern::Binding { v, pattern } => {
                let variable = *self.decode_local_var(*v);
                Pattern::Binding(variable, Box::new(self.decode_pattern(*pattern)))
            }
            tf::Pattern::EnumVariant {
                enum_type,
                variant_id,
                args,
                fields,
            } => Pattern::EnumVariant {
                enum_type: EnumType {
                    enum_: self.enum_decl(enum_type.id),
                    arguments: enum_type
                        .args
                        .into_iter()
                        .map(|arg| self.decode_expr(*arg))
                        .collect(),
                },
                variant: self.enum_variant(variant_id),
                args: args
                    .into_iter()
                    .map(|arg| self.decode_pattern(*arg))
                    .collect(),
                fields: fields
                    .into_iter()
                    .map(|field| self.decode_record_field_pattern(*field))
                    .collect(),
            },
            tf::Pattern::String { s } => Pattern::String(s),
            tf::Pattern::Int { i } => Pattern::Int(i),
            tf::Pattern::Bool { b } => Pattern::Bool(b),
        }
    }

    fn decode_record_field_pattern(
        self: &Arc<Self>,
        field: tf::RecordFieldPattern,
    ) -> RecordFieldPattern<DefaultExprContext> {
        RecordFieldPattern {
            field: self.record_field(field.field_id),
            pattern: self.decode_pattern(*field.pattern),
        }
    }

    fn decode_trait_type(
        self: &Arc<Self>,
        trait_type: tf::TraitType,
    ) -> TraitType<DefaultExprContext> {
        TraitType {
            trait_: self.trait_decl(trait_type.id),
            arguments: trait_type
                .args
                .into_iter()
                .map(|arg| self.decode_expr(*arg))
                .collect(),
        }
    }

    fn decode_method_instance_type(
        self: &Arc<Self>,
        instance_type: tf::MethodInstanceType,
    ) -> MethodInstanceType<DefaultExprContext> {
        match instance_type {
            tf::MethodInstanceType::TraitType { trait_type } => {
                MethodInstanceType::Trait(self.decode_trait_type(*trait_type))
            }
            tf::MethodInstanceType::InstanceSingletonType { .. } => {
                todo!("decode instance singleton method instance types")
            }
        }
    }

    fn decode_var(self: &Arc<Self>, var: tf::Var) -> Variable<DefaultExprContext> {
        match var {
            tf::Var::LocalVar { id } => Variable::Local(
                rwlock_read(&self.local_variables)
                    .get(&id)
                    .map(|variable| variable.clone())
                    .expect("local variable reference has unknown id"),
            ),
            tf::Var::ParameterVar {
                owner,
                parameter_index,
                name,
                var_type,
                erasure,
                witness,
            } => Variable::Parameter(Box::new(ParameterVariable {
                owner: self.decode_expression_owner(*owner),
                parameter_index: usize::try_from(parameter_index)
                    .expect("parameter index does not fit usize"),
                var_type: self.decode_expr(*var_type),
                name: name.map(|name| decode_identifier(*name)),
                erasure_mode: decode_erasure_mode(*erasure),
                is_witness: witness,
            })),
            tf::Var::InstanceParameterVar {
                owner,
                name,
                var_type,
            } => Variable::InstanceParameter(Box::new(InstanceParameterVariable {
                owner: self.decode_expression_owner(*owner),
                var_type: self.decode_expr(*var_type),
                name: name.map(|name| decode_identifier(*name)),
            })),
            tf::Var::ClosureParameterVar { id } => {
                Variable::ClosureParameter(self.decode_closure_parameter_ref(id))
            }
        }
    }

    fn decode_closure_parameter_var(
        self: &Arc<Self>,
        variable: tf::ClosureParameterVar,
    ) -> ClosureParameterVariable<DefaultExprContext> {
        let variable_id = variable.id;
        let variable = ClosureParameterVariable {
            id: UniqueIdentifier::new(),
            var_type: self.decode_expr(*variable.var_type),
            name: variable.name.map(|name| decode_identifier(*name)),
            is_mutable: variable.mutable,
            erasure_mode: decode_erasure_mode(*variable.erasure),
            is_witness: variable.witness,
        };
        rwlock_write(&self.closure_parameters).insert(variable_id, Box::new(variable.clone()));
        variable
    }

    fn decode_closure_parameter_ref(
        self: &Arc<Self>,
        id: BigUint,
    ) -> Box<ClosureParameterVariable<DefaultExprContext>> {
        rwlock_read(&self.closure_parameters)
            .get(&id)
            .cloned()
            .unwrap_or_else(|| {
                Box::new(ClosureParameterVariable {
                    id: UniqueIdentifier::new(),
                    var_type: Expr::Error,
                    name: None,
                    is_mutable: false,
                    erasure_mode: ErasureMode::Concrete,
                    is_witness: false,
                })
            })
    }

    fn decode_expression_owner(
        self: &Arc<Self>,
        owner: tf::ExpressionOwner,
    ) -> argon_expr::ExpressionOwner<DefaultExprContext> {
        match owner {
            tf::ExpressionOwner::Func { index } => {
                let function: Arc<dyn Function> = self.function(index);
                argon_expr::ExpressionOwner::Function(function)
            }
            tf::ExpressionOwner::Rec { index } => {
                argon_expr::ExpressionOwner::Record(self.record(index))
            }
            tf::ExpressionOwner::Enum { index } => {
                argon_expr::ExpressionOwner::Enum(self.enum_decl(index))
            }
            tf::ExpressionOwner::Trait { .. } => todo!("decode trait expression owner"),
            tf::ExpressionOwner::Instance { index } => {
                argon_expr::ExpressionOwner::Instance(self.instance(index))
            }
            tf::ExpressionOwner::EnumVariant { index } => {
                argon_expr::ExpressionOwner::EnumVariant(self.enum_variant(index))
            }
            tf::ExpressionOwner::Method { index } => {
                argon_expr::ExpressionOwner::Method(self.method(index))
            }
        }
    }

    fn decode_local_var(
        self: &Arc<Self>,
        variable: tf::LocalVar,
    ) -> Box<LocalVariable<DefaultExprContext>> {
        let id = variable.id;
        let local_variable = Box::new(LocalVariable {
            id: UniqueIdentifier::new(),
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

        rwlock_write(&self.local_variables).insert(id, local_variable.clone());
        local_variable
    }

    fn decode_block_label(
        self: &Arc<Self>,
        label: tf::BlockLabel,
    ) -> Box<BlockLabel<DefaultExprContext>> {
        let id = label.id;
        if let Some(existing) = rwlock_read(&self.block_labels).get(&id).cloned() {
            return existing;
        }

        let block_label = Box::new(BlockLabel {
            id: UniqueIdentifier::new(),
            name: label.name.map(|name| decode_identifier(*name)),
            kind: decode_block_label_kind(label.kind),
            block_result_type: self.decode_expr(*label.block_result_type),
        });

        rwlock_write(&self.block_labels).insert(id, block_label.clone());
        block_label
    }

    fn decode_block_id(
        self: &Arc<Self>,
        block_id: tf::BlockId,
    ) -> Box<BlockLabel<DefaultExprContext>> {
        rwlock_read(&self.block_labels)
            .get(&block_id.id)
            .cloned()
            .expect("block jump references unknown block label id")
    }
}

struct DecodedFunction {
    decoder: Arc<TubeDecoder>,
    definition: tf::FunctionDefinition,
    metadata: FunctionMetadata,
    import: Mutex<Option<ImportSpecifier>>,
    signature: Mutex<Option<Arc<FunctionSignature<DefaultExprContext>>>>,
    implementation: Mutex<Option<Option<Arc<FunctionImplementation>>>>,
}

impl DecodedFunction {
    fn new(decoder: Arc<TubeDecoder>, definition: tf::FunctionDefinition) -> Self {
        let metadata = FunctionMetadata {
            is_inline: definition.inline,
            erasure_mode: decode_erasure_mode((*definition.erasure).clone()),
            is_witness: definition.witness,
            effect_info: decode_effect_info((*definition.effects).clone()),
        };

        Self {
            decoder,
            definition,
            metadata,
            import: Mutex::new(None),
            signature: Mutex::new(None),
            implementation: Mutex::new(None),
        }
    }
}

impl Debug for DecodedFunction {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "function {:?}", self.definition.import)
    }
}

impl Unload for DecodedFunction {
    fn unload(&self) {
        clear_cached(&self.import);
        clear_cached(&self.signature);
        clear_cached(&self.implementation);
    }
}

impl Function for DecodedFunction {
    fn metadata(&self) -> &FunctionMetadata {
        &self.metadata
    }

    fn import_specifier(self: Arc<Self>) -> ImportSpecifier {
        get_or_init_cached(&self.import, || {
            self.decoder
                .decode_import_specifier((*self.definition.import).clone())
        })
    }

    fn signature(self: Arc<Self>) -> Arc<FunctionSignature<DefaultExprContext>> {
        get_or_init_cached(&self.signature, || {
            Arc::new(
                self.decoder
                    .decode_function_signature((*self.definition.signature).clone()),
            )
        })
    }

    fn implementation(self: Arc<Self>) -> Option<Arc<FunctionImplementation>> {
        get_or_init_cached(&self.implementation, || {
            self.definition
                .implementation
                .clone()
                .map(|implementation| {
                    Arc::new(self.decoder.decode_function_implementation(*implementation))
                })
        })
    }
}

struct DecodedTrait {
    decoder: Arc<TubeDecoder>,
    definition: tf::TraitDefinition,
    import: Mutex<Option<ImportSpecifier>>,
    signature: Mutex<Option<Arc<FunctionSignature<DefaultExprContext>>>>,
    methods: Mutex<Option<Arc<Vec<MethodEntry>>>>,
}

impl DecodedTrait {
    fn new(decoder: Arc<TubeDecoder>, definition: tf::TraitDefinition) -> Self {
        Self {
            decoder,
            definition,
            import: Mutex::new(None),
            signature: Mutex::new(None),
            methods: Mutex::new(None),
        }
    }
}

impl Debug for DecodedTrait {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "trait {:?}", self.definition.import)
    }
}

impl Unload for DecodedTrait {
    fn unload(&self) {
        clear_cached(&self.import);
        clear_cached(&self.signature);
        clear_cached(&self.methods);
    }
}

impl Trait for DecodedTrait {
    fn import_specifier(self: Arc<Self>) -> ImportSpecifier {
        get_or_init_cached(&self.import, || {
            self.decoder
                .decode_import_specifier((*self.definition.import).clone())
        })
    }

    fn signature(self: Arc<Self>) -> Arc<FunctionSignature<DefaultExprContext>> {
        get_or_init_cached(&self.signature, || {
            Arc::new(
                self.decoder
                    .decode_function_signature((*self.definition.signature).clone()),
            )
        })
    }

    fn methods(self: Arc<Self>) -> Arc<Vec<MethodEntry>> {
        get_or_init_cached(&self.methods, || {
            Arc::new(
                self.definition
                    .methods
                    .iter()
                    .map(|entry| MethodEntry {
                        access: decode_access_modifier((*entry.access).clone()),
                        method: self.decoder.method(entry.id.clone()),
                    })
                    .collect(),
            )
        })
    }
}

struct DecodedMethod {
    decoder: Arc<TubeDecoder>,
    owner: MethodOwner,
    entry: tf::MethodEntry,
    metadata: MethodMetadata,
    signature: Mutex<Option<Arc<FunctionSignature<DefaultExprContext>>>>,
    implementation: Mutex<Option<Option<Arc<FunctionImplementation>>>>,
}

impl DecodedMethod {
    fn new(decoder: Arc<TubeDecoder>, owner: MethodOwner, entry: tf::MethodEntry) -> Self {
        let definition = &entry.method;
        let slot = decode_method_slot((*definition.slot).clone());
        let metadata = MethodMetadata {
            access: decode_access_modifier((*entry.access).clone()),
            name: decode_identifier((*definition.name).clone()),
            is_abstract: definition.implementation.is_none(),
            is_inline: definition.inline,
            erasure_mode: if definition.erased {
                ErasureMode::Erased
            } else {
                ErasureMode::Concrete
            },
            is_witness: definition.witness,
            slot,
            effect_info: decode_effect_info((*definition.effects).clone()),
            instance_parameter: decode_instance_parameter((*definition.instance_parameter).clone()),
        };

        Self {
            decoder,
            owner,
            entry,
            metadata,
            signature: Mutex::new(None),
            implementation: Mutex::new(None),
        }
    }
}

impl Debug for DecodedMethod {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "method {:?}", self.entry.method.name)
    }
}

impl Unload for DecodedMethod {
    fn unload(&self) {
        clear_cached(&self.signature);
        clear_cached(&self.implementation);
    }
}

impl Method for DecodedMethod {
    fn owner(self: Arc<Self>) -> MethodOwner {
        self.owner.clone()
    }

    fn metadata(&self) -> &MethodMetadata {
        &self.metadata
    }

    fn signature(self: Arc<Self>) -> Arc<FunctionSignature<DefaultExprContext>> {
        get_or_init_cached(&self.signature, || {
            Arc::new(
                self.decoder
                    .decode_function_signature((*self.entry.method.signature).clone()),
            )
        })
    }

    fn implementation(self: Arc<Self>) -> Option<Arc<FunctionImplementation>> {
        get_or_init_cached(&self.implementation, || {
            self.entry
                .method
                .implementation
                .clone()
                .map(|implementation| {
                    Arc::new(self.decoder.decode_method_implementation(*implementation))
                })
        })
    }
}

struct DecodedInstance {
    decoder: Arc<TubeDecoder>,
    definition: tf::InstanceDefinition,
    import: Mutex<Option<ImportSpecifier>>,
    signature: Mutex<Option<Arc<FunctionSignature<DefaultExprContext>>>>,
    methods: Mutex<Option<Arc<Vec<MethodEntry>>>>,
}

impl DecodedInstance {
    fn new(decoder: Arc<TubeDecoder>, definition: tf::InstanceDefinition) -> Self {
        Self {
            decoder,
            definition,
            import: Mutex::new(None),
            signature: Mutex::new(None),
            methods: Mutex::new(None),
        }
    }
}

impl Debug for DecodedInstance {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "instance {:?}", self.definition.import)
    }
}

impl Unload for DecodedInstance {
    fn unload(&self) {
        clear_cached(&self.import);
        clear_cached(&self.signature);
        clear_cached(&self.methods);
    }
}

impl Instance for DecodedInstance {
    fn import_specifier(self: Arc<Self>) -> ImportSpecifier {
        get_or_init_cached(&self.import, || {
            self.decoder
                .decode_import_specifier((*self.definition.import).clone())
        })
    }

    fn erasure_mode(&self) -> ErasureMode {
        decode_erasure_mode((*self.definition.erasure).clone())
    }

    fn signature(self: Arc<Self>) -> Arc<FunctionSignature<DefaultExprContext>> {
        get_or_init_cached(&self.signature, || {
            Arc::new(
                self.decoder
                    .decode_function_signature((*self.definition.signature).clone()),
            )
        })
    }

    fn methods(self: Arc<Self>) -> Arc<Vec<MethodEntry>> {
        get_or_init_cached(&self.methods, || {
            Arc::new(
                self.definition
                    .methods
                    .iter()
                    .map(|entry| MethodEntry {
                        access: decode_access_modifier((*entry.access).clone()),
                        method: self.decoder.method(entry.id.clone()),
                    })
                    .collect(),
            )
        })
    }
}

struct DecodedEnum {
    decoder: Arc<TubeDecoder>,
    definition: tf::EnumDefinition,
    import: Mutex<Option<ImportSpecifier>>,
    signature: Mutex<Option<Arc<FunctionSignature<DefaultExprContext>>>>,
    variants: Mutex<Option<Arc<Vec<Arc<dyn EnumVariant>>>>>,
}

impl DecodedEnum {
    fn new(decoder: Arc<TubeDecoder>, definition: tf::EnumDefinition) -> Self {
        Self {
            decoder,
            definition,
            import: Mutex::new(None),
            signature: Mutex::new(None),
            variants: Mutex::new(None),
        }
    }
}

impl Debug for DecodedEnum {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "enum {:?}", self.definition.import)
    }
}

impl Unload for DecodedEnum {
    fn unload(&self) {
        clear_cached(&self.import);
        clear_cached(&self.signature);
        clear_cached(&self.variants);
    }
}

impl Enum for DecodedEnum {
    fn import_specifier(self: Arc<Self>) -> ImportSpecifier {
        get_or_init_cached(&self.import, || {
            self.decoder
                .decode_import_specifier((*self.definition.import).clone())
        })
    }

    fn signature(self: Arc<Self>) -> Arc<FunctionSignature<DefaultExprContext>> {
        get_or_init_cached(&self.signature, || {
            Arc::new(
                self.decoder
                    .decode_function_signature((*self.definition.signature).clone()),
            )
        })
    }

    fn variants(self: Arc<Self>) -> Arc<Vec<Arc<dyn EnumVariant>>> {
        get_or_init_cached(&self.variants, || {
            Arc::new(
                self.definition
                    .variants
                    .iter()
                    .map(|variant| {
                        Arc::new(DecodedEnumVariant::new(self.clone(), (**variant).clone()))
                            as Arc<dyn EnumVariant>
                    })
                    .collect(),
            )
        })
    }
}

struct DecodedEnumVariant {
    owner: Arc<DecodedEnum>,
    definition: tf::EnumVariantDefinition,
    metadata: EnumVariantMetadata,
    signature: Mutex<Option<Arc<FunctionSignature<DefaultExprContext>>>>,
    fields: Mutex<Option<Arc<Vec<Arc<dyn RecordField>>>>>,
}

impl DecodedEnumVariant {
    fn new(owner: Arc<DecodedEnum>, definition: tf::EnumVariantDefinition) -> Self {
        let metadata = EnumVariantMetadata {
            name: decode_identifier((*definition.name).clone()),
        };

        Self {
            owner,
            definition,
            metadata,
            signature: Mutex::new(None),
            fields: Mutex::new(None),
        }
    }
}

impl Debug for DecodedEnumVariant {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "enum variant {:?}", self.metadata.name)
    }
}

impl Unload for DecodedEnumVariant {
    fn unload(&self) {
        clear_cached(&self.signature);
        clear_cached(&self.fields);
    }
}

impl EnumVariant for DecodedEnumVariant {
    fn owning_enum(self: Arc<Self>) -> Arc<dyn Enum> {
        self.owner.clone()
    }

    fn metadata(&self) -> &EnumVariantMetadata {
        &self.metadata
    }

    fn signature(self: Arc<Self>) -> Arc<FunctionSignature<DefaultExprContext>> {
        get_or_init_cached(&self.signature, || {
            Arc::new(
                self.owner
                    .decoder
                    .decode_function_signature((*self.definition.signature).clone()),
            )
        })
    }

    fn fields(self: Arc<Self>) -> Arc<Vec<Arc<dyn RecordField>>> {
        get_or_init_cached(&self.fields, || {
            Arc::new(
                self.definition
                    .fields
                    .iter()
                    .map(|field| {
                        Arc::new(DecodedEnumVariantField::new(
                            self.clone(),
                            (**field).clone(),
                        )) as Arc<dyn RecordField>
                    })
                    .collect(),
            )
        })
    }
}

struct DecodedEnumVariantField {
    owner: Arc<DecodedEnumVariant>,
    definition: tf::RecordFieldDefinition,
    metadata: RecordFieldMetadata,
    field_type: Mutex<Option<Arc<Expr<DefaultExprContext>>>>,
}

impl DecodedEnumVariantField {
    fn new(owner: Arc<DecodedEnumVariant>, definition: tf::RecordFieldDefinition) -> Self {
        let metadata = RecordFieldMetadata {
            is_mutable: definition.mutable,
            name: decode_identifier((*definition.name).clone()),
        };

        Self {
            owner,
            definition,
            metadata,
            field_type: Mutex::new(None),
        }
    }
}

impl Debug for DecodedEnumVariantField {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "enum variant field {:?}", self.metadata.name)
    }
}

impl Unload for DecodedEnumVariantField {
    fn unload(&self) {
        clear_cached(&self.field_type);
    }
}

impl RecordField for DecodedEnumVariantField {
    fn owning_record(&self) -> RecordFieldOwner {
        RecordFieldOwner::EnumVariant(self.owner.clone())
    }

    fn metadata(&self) -> &RecordFieldMetadata {
        &self.metadata
    }

    fn field_type(self: Arc<Self>) -> Arc<Expr<DefaultExprContext>> {
        get_or_init_cached(&self.field_type, || {
            Arc::new(
                self.owner
                    .owner
                    .decoder
                    .decode_expr((*self.definition.field_type).clone()),
            )
        })
    }
}

struct DecodedRecord {
    decoder: Arc<TubeDecoder>,
    definition: tf::RecordDefinition,
    import: Mutex<Option<ImportSpecifier>>,
    signature: Mutex<Option<Arc<FunctionSignature<DefaultExprContext>>>>,
    fields: Mutex<Option<Arc<Vec<Arc<dyn RecordField>>>>>,
}

impl DecodedRecord {
    fn new(decoder: Arc<TubeDecoder>, definition: tf::RecordDefinition) -> Self {
        Self {
            decoder,
            definition,
            import: Mutex::new(None),
            signature: Mutex::new(None),
            fields: Mutex::new(None),
        }
    }
}

impl Debug for DecodedRecord {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "record {:?}", self.definition.import)
    }
}

impl Unload for DecodedRecord {
    fn unload(&self) {
        clear_cached(&self.import);
        clear_cached(&self.signature);
        clear_cached(&self.fields);
    }
}

impl Record for DecodedRecord {
    fn import_specifier(self: Arc<Self>) -> ImportSpecifier {
        get_or_init_cached(&self.import, || {
            self.decoder
                .decode_import_specifier((*self.definition.import).clone())
        })
    }

    fn signature(self: Arc<Self>) -> Arc<FunctionSignature<DefaultExprContext>> {
        get_or_init_cached(&self.signature, || {
            Arc::new(
                self.decoder
                    .decode_function_signature((*self.definition.signature).clone()),
            )
        })
    }

    fn fields(self: Arc<Self>) -> Arc<Vec<Arc<dyn RecordField>>> {
        get_or_init_cached(&self.fields, || {
            Arc::new(
                self.definition
                    .fields
                    .iter()
                    .map(|field| {
                        Arc::new(DecodedRecordField::new(self.clone(), (**field).clone()))
                            as Arc<dyn RecordField>
                    })
                    .collect(),
            )
        })
    }
}

struct DecodedRecordField {
    owner: Arc<DecodedRecord>,
    definition: tf::RecordFieldDefinition,
    metadata: RecordFieldMetadata,
    field_type: Mutex<Option<Arc<Expr<DefaultExprContext>>>>,
}

impl DecodedRecordField {
    fn new(owner: Arc<DecodedRecord>, definition: tf::RecordFieldDefinition) -> Self {
        let metadata = RecordFieldMetadata {
            is_mutable: definition.mutable,
            name: decode_identifier((*definition.name).clone()),
        };

        Self {
            owner,
            definition,
            metadata,
            field_type: Mutex::new(None),
        }
    }
}

impl Debug for DecodedRecordField {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "record field {:?}", self.metadata.name)
    }
}

impl Unload for DecodedRecordField {
    fn unload(&self) {
        clear_cached(&self.field_type);
    }
}

impl RecordField for DecodedRecordField {
    fn owning_record(&self) -> RecordFieldOwner {
        RecordFieldOwner::Record(self.owner.clone())
    }

    fn metadata(&self) -> &RecordFieldMetadata {
        &self.metadata
    }

    fn field_type(self: Arc<Self>) -> Arc<Expr<DefaultExprContext>> {
        get_or_init_cached(&self.field_type, || {
            Arc::new(
                self.owner
                    .decoder
                    .decode_expr((*self.definition.field_type).clone()),
            )
        })
    }
}

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

fn get_or_init_cached<T: Clone, F: FnOnce() -> T>(cache: &Mutex<Option<T>>, init: F) -> T {
    let mut cache = mutex_lock(cache);
    if let Some(value) = cache.as_ref() {
        return value.clone();
    }

    let value = init();
    *cache = Some(value.clone());
    value
}

fn clear_cached<T>(cache: &Mutex<Option<T>>) {
    *mutex_lock(cache) = None;
}

fn to_usize_index(n: BigUint) -> usize {
    usize::try_from(n).expect("tube id does not fit usize")
}

fn decode_module_path(path: tf::ModulePath) -> ModulePath {
    ModulePath(path.path)
}

fn decode_tube_name(name: tf::TubeName) -> TubeName {
    TubeName(Vec1::try_from(iter::once(name.head).chain(name.tail).collect::<Vec<_>>()).unwrap())
}

fn decode_identifier(id: tf::Identifier) -> Identifier {
    match id {
        tf::Identifier::Named { s } => Identifier::Named(s),
        tf::Identifier::BinOp { op } => Identifier::BinaryOp(decode_binary_operator(op)),
        tf::Identifier::UnOp { op } => Identifier::UnaryOp(decode_unary_operator(op)),
        tf::Identifier::Index {} => Identifier::Index,
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
    }
}

fn decode_access_modifier_global(access: tf::AccessModifierGlobal) -> AccessModifierGlobal {
    match access {
        tf::AccessModifierGlobal::Public {} => AccessModifierGlobal::Public,
        tf::AccessModifierGlobal::Internal {} => AccessModifierGlobal::Internal,
        tf::AccessModifierGlobal::ModulePrivate {} => AccessModifierGlobal::ModulePrivate,
    }
}

fn decode_access_modifier(access: tf::AccessModifier) -> AccessModifier {
    match access {
        tf::AccessModifier::Public {} => AccessModifier::Public,
        tf::AccessModifier::Private {} => AccessModifier::Private,
        tf::AccessModifier::Protected {} => AccessModifier::Protected,
        tf::AccessModifier::Internal {} => AccessModifier::Internal,
        tf::AccessModifier::ProtectedOrInternal {} => AccessModifier::ProtectedOrInternal,
        tf::AccessModifier::ProtectedAndInternal {} => AccessModifier::ProtectedAndInternal,
        tf::AccessModifier::ModulePrivate {} => AccessModifier::ModulePrivate,
    }
}

fn decode_method_slot(slot: tf::MethodSlot) -> MethodSlot {
    match slot {
        tf::MethodSlot::Abstract {} => MethodSlot::Abstract,
        tf::MethodSlot::AbstractOverride {} => MethodSlot::AbstractOverride,
        tf::MethodSlot::Virtual {} => MethodSlot::Virtual,
        tf::MethodSlot::Override {} => MethodSlot::Override,
        tf::MethodSlot::Final {} => MethodSlot::Final,
        tf::MethodSlot::FinalOverride {} => MethodSlot::FinalOverride,
    }
}

fn decode_instance_parameter(parameter: tf::InstanceParameter) -> MethodInstanceParameter {
    MethodInstanceParameter {
        name: parameter.name.map(|name| decode_identifier(*name)),
    }
}

fn decode_erasure_mode(mode: tf::ErasureMode) -> ErasureMode {
    match mode {
        tf::ErasureMode::Concrete {} => ErasureMode::Concrete,
        tf::ErasureMode::Erased {} => ErasureMode::Erased,
        tf::ErasureMode::Token {} => ErasureMode::Token,
    }
}

fn decode_block_label_kind(kind: tf::BlockLabelKind) -> BlockLabelKind {
    match kind {
        tf::BlockLabelKind::Block => BlockLabelKind::Block,
        tf::BlockLabelKind::Loop => BlockLabelKind::Loop,
        tf::BlockLabelKind::WhileOuter => BlockLabelKind::WhileOuter,
        tf::BlockLabelKind::WhileInner => BlockLabelKind::WhileInner,
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
        tf::FunctionParameterListType::NormalList {} => FunctionParameterListType::NormalList,
        tf::FunctionParameterListType::InferrableList { level } => {
            FunctionParameterListType::InferrableList(level)
        }
        tf::FunctionParameterListType::RequiresList {} => FunctionParameterListType::RequiresList,
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
