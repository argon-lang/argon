use std::collections::{HashMap, HashSet};

use argon_format_vm::vm as vf;
use num_bigint::BigUint;

pub struct PreparedInlineFunctionBody {
    pub body: vf::FunctionBody,
    pub argument_registers: Vec<vf::RegisterId>,
    pub next_register: BigUint,
    pub next_block_id: BigUint,
}

pub fn prepare_function_body_for_inline(
    body: &vf::FunctionBody,
    argument_types: &[vf::Token],
    token_args: &[Box<vf::Token>],
    first_register: BigUint,
    first_block_id: BigUint,
    return_register: Option<&vf::RegisterId>,
) -> Option<PreparedInlineFunctionBody> {
    let mut variables = Vec::new();
    let mut register_replacements = HashMap::new();
    let mut argument_registers = Vec::new();
    let mut next_register = first_register;
    let (block_replacements, next_block_id) =
        block_replacements_for_inline(&body.region, first_block_id);

    for (arg_index, argument_type) in argument_types.iter().enumerate() {
        let register = vf::RegisterId {
            id: next_register.clone(),
        };
        register_replacements.insert(BigUint::from(arg_index), register.id.clone());
        argument_registers.push(register);
        variables.push(Box::new(vf::VariableDeclaration {
            r#type: Box::new(substitute_token_arguments(argument_type, token_args)?),
        }));
        next_register += 1u32;
    }

    for (local_index, variable) in body.variables.variables.iter().enumerate() {
        register_replacements.insert(
            BigUint::from(argument_types.len() + local_index),
            next_register.clone(),
        );
        variables.push(Box::new(vf::VariableDeclaration {
            r#type: Box::new(substitute_token_arguments(&variable.r#type, token_args)?),
        }));
        next_register += 1u32;
    }

    Some(PreparedInlineFunctionBody {
        body: vf::FunctionBody {
            variables: Box::new(vf::VariableDeclarations { variables }),
            region: Box::new(prepare_region_for_inline(
                &body.region,
                token_args,
                &register_replacements,
                &block_replacements,
                return_register,
            )?),
        },
        argument_registers,
        next_register,
        next_block_id,
    })
}

pub fn next_available_block_id(region: &vf::Region) -> BigUint {
    let mut next_block_id = BigUint::from(0u32);
    collect_block_ids(region, &mut |block_id| {
        if block_id.id >= next_block_id {
            next_block_id = &block_id.id + 1u32;
        }
    });
    next_block_id
}

pub struct TubeModel {
    pub header: vf::TubeHeader,
    pub metadata: vf::TubeMetadata,
    pub modules: Vec<ModuleModel>,

    pub module_info: HashMap<BigUint, ModuleInfo>,
    pub function_info: HashMap<BigUint, FunctionInfo>,
    pub record_info: HashMap<BigUint, RecordInfo>,
    pub record_field_info: HashMap<BigUint, RecordFieldInfo>,
    pub enum_info: HashMap<BigUint, EnumInfo>,
    pub enum_variant_info: HashMap<BigUint, EnumVariantInfo>,
    pub trait_info: HashMap<BigUint, TraitInfo>,
    pub method_info: HashMap<BigUint, MethodInfo>,
    pub instance_info: HashMap<BigUint, InstanceInfo>,
}

impl TubeModel {
    pub fn from_entries(entries: impl IntoIterator<Item = vf::TubeFileEntry>) -> Self {
        let mut entries = entries.into_iter();

        let Some(vf::TubeFileEntry::Header { header }) = entries.next() else {
            panic!("missing tube header");
        };

        let Some(vf::TubeFileEntry::Metadata { metadata }) = entries.next() else {
            panic!("missing tube metadata");
        };

        let modules = metadata
            .modules
            .iter()
            .map(|module| ModuleModel {
                path: (*module.path).clone(),
                exports: Vec::new(),
            })
            .collect::<Vec<_>>();

        let mut model = Self {
            header: *header,
            metadata: *metadata,
            modules,
            module_info: HashMap::new(),
            function_info: HashMap::new(),
            record_info: HashMap::new(),
            record_field_info: HashMap::new(),
            enum_info: HashMap::new(),
            enum_variant_info: HashMap::new(),
            trait_info: HashMap::new(),
            method_info: HashMap::new(),
            instance_info: HashMap::new(),
        };

        for (module_index, module) in model.modules.iter().enumerate() {
            model.module_info.insert(
                BigUint::from(module_index),
                ModuleInfo {
                    tube_id: BigUint::from(0u32),
                    path: module.path.clone(),
                },
            );
        }

        for entry in entries {
            model.store_entry(entry);
        }

        model
    }

    pub fn into_entries(self) -> Vec<vf::TubeFileEntry> {
        let mut entries = vec![
            vf::TubeFileEntry::Header {
                header: Box::new(self.header),
            },
            vf::TubeFileEntry::Metadata {
                metadata: Box::new(vf::TubeMetadata {
                    modules: self
                        .modules
                        .iter()
                        .map(|module| {
                            Box::new(vf::Module {
                                path: Box::new(module.path.clone()),
                            })
                        })
                        .collect(),
                    ..self.metadata
                }),
            },
        ];

        let current_module_count = self.modules.len();
        for (module_id, module_info) in sorted_entries(&self.module_info) {
            if module_info.tube_id == BigUint::from(0u32)
                && module_id < &BigUint::from(current_module_count)
            {
                continue;
            }

            entries.push(vf::TubeFileEntry::ModuleReference {
                module_id: module_id.clone(),
                tube_id: module_info.tube_id.clone(),
                path: Box::new(module_info.path.clone()),
            });
        }

        let mut function_definitions = HashSet::new();
        let mut record_definitions = HashSet::new();
        let mut enum_definitions = HashSet::new();
        let mut enum_variant_definitions = HashSet::new();
        let mut record_field_definitions = HashSet::new();
        let mut trait_definitions = HashSet::new();
        let mut method_definitions = HashSet::new();
        let mut instance_definitions = HashSet::new();

        for module in self.modules {
            for export in module.exports {
                match export {
                    ModuleExportEntry::FunctionDefinition(definition) => {
                        function_definitions.insert(definition.function_id.clone());
                        entries.push(vf::TubeFileEntry::FunctionDefinition {
                            definition: Box::new(definition),
                        });
                    }
                    ModuleExportEntry::RecordDefinition(definition) => {
                        record_definitions.insert(definition.record_id.clone());
                        record_field_definitions
                            .extend(definition.fields.iter().map(|field| field.field_id.clone()));
                        entries.push(vf::TubeFileEntry::RecordDefinition {
                            definition: Box::new(definition),
                        });
                    }
                    ModuleExportEntry::EnumDefinition(definition) => {
                        enum_definitions.insert(definition.enum_id.clone());
                        for variant in &definition.variants {
                            enum_variant_definitions.insert(variant.variant_id.clone());
                            record_field_definitions
                                .extend(variant.fields.iter().map(|field| field.field_id.clone()));
                        }
                        entries.push(vf::TubeFileEntry::EnumDefinition {
                            definition: Box::new(definition),
                        });
                    }
                    ModuleExportEntry::TraitDefinition(definition) => {
                        trait_definitions.insert(definition.trait_id.clone());
                        method_definitions.extend(definition_methods(&definition.methods));
                        entries.push(vf::TubeFileEntry::TraitDefinition {
                            definition: Box::new(definition),
                        });
                    }
                    ModuleExportEntry::InstanceDefinition(definition) => {
                        instance_definitions.insert(definition.instance_id.clone());
                        method_definitions.extend(definition_methods(&definition.methods));
                        entries.push(vf::TubeFileEntry::InstanceDefinition {
                            definition: Box::new(definition),
                        });
                    }
                }
            }
        }

        for (function_id, info) in sorted_entries(&self.function_info) {
            if function_definitions.contains(function_id) {
                continue;
            }

            entries.push(vf::TubeFileEntry::FunctionReference {
                function_id: function_id.clone(),
                import: Box::new(info.import_specifier.clone()),
                signature: Box::new(info.signature.clone()),
            });
        }

        for (record_id, info) in sorted_entries(&self.record_info) {
            if record_definitions.contains(record_id) {
                continue;
            }

            entries.push(vf::TubeFileEntry::RecordReference {
                record_id: record_id.clone(),
                import: Box::new(info.import_specifier.clone()),
                signature: Box::new(info.signature.clone()),
            });
        }

        for (enum_id, info) in sorted_entries(&self.enum_info) {
            if enum_definitions.contains(enum_id) {
                continue;
            }

            entries.push(vf::TubeFileEntry::EnumReference {
                enum_id: enum_id.clone(),
                import: Box::new(info.import_specifier.clone()),
                signature: Box::new(info.signature.clone()),
            });
        }

        for (variant_id, info) in sorted_entries(&self.enum_variant_info) {
            if enum_variant_definitions.contains(variant_id) {
                continue;
            }

            entries.push(vf::TubeFileEntry::EnumVariantReference {
                variant_id: variant_id.clone(),
                enum_id: info.enum_id.clone(),
                name: Box::new(info.name.clone()),
                signature: Box::new(info.signature.clone()),
            });
        }

        for (record_field_id, info) in sorted_entries(&self.record_field_info) {
            if record_field_definitions.contains(record_field_id) {
                continue;
            }

            match &info.owner {
                RecordFieldOwner::Record => entries.push(vf::TubeFileEntry::RecordFieldReference {
                    record_field_id: record_field_id.clone(),
                    record_id: info.owner_id.clone(),
                    name: Box::new(info.name.clone()),
                    field_type: Box::new(info.field_type.clone()),
                }),
                RecordFieldOwner::EnumVariant => {
                    entries.push(vf::TubeFileEntry::EnumVariantRecordFieldReference {
                        record_field_id: record_field_id.clone(),
                        variant_id: info.owner_id.clone(),
                        name: Box::new(info.name.clone()),
                        field_type: Box::new(info.field_type.clone()),
                    })
                }
            }
        }

        for (trait_id, info) in sorted_entries(&self.trait_info) {
            if trait_definitions.contains(trait_id) {
                continue;
            }

            entries.push(vf::TubeFileEntry::TraitReference {
                trait_id: trait_id.clone(),
                import: Box::new(info.import_specifier.clone()),
                signature: Box::new(info.signature.clone()),
            });
        }

        for (instance_id, info) in sorted_entries(&self.instance_info) {
            if instance_definitions.contains(instance_id) {
                continue;
            }

            entries.push(vf::TubeFileEntry::InstanceReference {
                instance_id: instance_id.clone(),
                import: Box::new(info.import_specifier.clone()),
                signature: Box::new(info.signature.clone()),
            });
        }

        for (method_id, info) in sorted_entries(&self.method_info) {
            if method_definitions.contains(method_id) {
                continue;
            }

            match &info.owner {
                MethodOwner::Trait(trait_id) => {
                    entries.push(vf::TubeFileEntry::TraitMethodReference {
                        method_id: method_id.clone(),
                        trait_id: trait_id.clone(),
                        name: Box::new(info.name.clone()),
                        erased_signature: Box::new(info.erased_signature.clone()),
                        signature: Box::new(info.signature.clone()),
                    })
                }
                MethodOwner::Instance(instance_id) => {
                    entries.push(vf::TubeFileEntry::InstanceMethodReference {
                        method_id: method_id.clone(),
                        instance_id: instance_id.clone(),
                        name: Box::new(info.name.clone()),
                        erased_signature: Box::new(info.erased_signature.clone()),
                        signature: Box::new(info.signature.clone()),
                    })
                }
            }
        }

        entries
    }

    fn store_entry(&mut self, entry: vf::TubeFileEntry) {
        match entry {
            vf::TubeFileEntry::Header { .. } => panic!("extra tube header"),
            vf::TubeFileEntry::Metadata { .. } => panic!("extra tube metadata"),
            vf::TubeFileEntry::ModuleReference {
                module_id,
                tube_id,
                path,
            } => {
                self.module_info.insert(
                    module_id,
                    ModuleInfo {
                        tube_id,
                        path: *path,
                    },
                );
            }
            vf::TubeFileEntry::FunctionDefinition { definition } => {
                let definition = *definition;
                let import = (*definition.import).clone();
                self.function_info.insert(
                    definition.function_id.clone(),
                    FunctionInfo {
                        import_specifier: import.clone(),
                        signature: (*definition.signature).clone(),
                    },
                );
                self.push_export(&import, ModuleExportEntry::FunctionDefinition(definition));
            }
            vf::TubeFileEntry::FunctionReference {
                function_id,
                import,
                signature,
            } => {
                self.function_info.insert(
                    function_id,
                    FunctionInfo {
                        import_specifier: *import,
                        signature: *signature,
                    },
                );
            }
            vf::TubeFileEntry::RecordDefinition { definition } => {
                let definition = *definition;
                let import = (*definition.import).clone();
                for field in &definition.fields {
                    self.record_field_info.insert(
                        field.field_id.clone(),
                        RecordFieldInfo {
                            owner: RecordFieldOwner::Record,
                            owner_id: definition.record_id.clone(),
                            name: (*field.name).clone(),
                            field_type: (*field.field_type).clone(),
                        },
                    );
                }
                self.record_info.insert(
                    definition.record_id.clone(),
                    RecordInfo {
                        import_specifier: import.clone(),
                        signature: (*definition.signature).clone(),
                    },
                );
                self.push_export(&import, ModuleExportEntry::RecordDefinition(definition));
            }
            vf::TubeFileEntry::RecordReference {
                record_id,
                import,
                signature,
            } => {
                self.record_info.insert(
                    record_id,
                    RecordInfo {
                        import_specifier: *import,
                        signature: *signature,
                    },
                );
            }
            vf::TubeFileEntry::EnumDefinition { definition } => {
                let definition = *definition;
                let import = (*definition.import).clone();
                for variant in &definition.variants {
                    self.enum_variant_info.insert(
                        variant.variant_id.clone(),
                        EnumVariantInfo {
                            enum_id: definition.enum_id.clone(),
                            name: (*variant.name).clone(),
                            signature: (*variant.signature).clone(),
                        },
                    );
                    for field in &variant.fields {
                        self.record_field_info.insert(
                            field.field_id.clone(),
                            RecordFieldInfo {
                                owner: RecordFieldOwner::EnumVariant,
                                owner_id: variant.variant_id.clone(),
                                name: (*field.name).clone(),
                                field_type: (*field.field_type).clone(),
                            },
                        );
                    }
                }
                self.enum_info.insert(
                    definition.enum_id.clone(),
                    EnumInfo {
                        import_specifier: import.clone(),
                        signature: (*definition.signature).clone(),
                    },
                );
                self.push_export(&import, ModuleExportEntry::EnumDefinition(definition));
            }
            vf::TubeFileEntry::EnumReference {
                enum_id,
                import,
                signature,
            } => {
                self.enum_info.insert(
                    enum_id,
                    EnumInfo {
                        import_specifier: *import,
                        signature: *signature,
                    },
                );
            }
            vf::TubeFileEntry::EnumVariantReference {
                variant_id,
                enum_id,
                name,
                signature,
            } => {
                self.enum_variant_info.insert(
                    variant_id,
                    EnumVariantInfo {
                        enum_id,
                        name: *name,
                        signature: *signature,
                    },
                );
            }
            vf::TubeFileEntry::RecordFieldReference {
                record_field_id,
                record_id,
                name,
                field_type,
            } => {
                self.record_field_info.insert(
                    record_field_id,
                    RecordFieldInfo {
                        owner: RecordFieldOwner::Record,
                        owner_id: record_id,
                        name: *name,
                        field_type: *field_type,
                    },
                );
            }
            vf::TubeFileEntry::EnumVariantRecordFieldReference {
                record_field_id,
                variant_id,
                name,
                field_type,
            } => {
                self.record_field_info.insert(
                    record_field_id,
                    RecordFieldInfo {
                        owner: RecordFieldOwner::EnumVariant,
                        owner_id: variant_id,
                        name: *name,
                        field_type: *field_type,
                    },
                );
            }
            vf::TubeFileEntry::TraitDefinition { definition } => {
                let definition = *definition;
                let import = (*definition.import).clone();
                for method in &definition.methods {
                    self.method_info.insert(
                        method.method_id.clone(),
                        MethodInfo {
                            owner: MethodOwner::Trait(definition.trait_id.clone()),
                            parent_import_specifier: import.clone(),
                            name: (*method.name).clone(),
                            erased_signature: (*method.erased_signature).clone(),
                            signature: (*method.signature).clone(),
                            definition: Some((**method).clone()),
                        },
                    );
                }
                self.trait_info.insert(
                    definition.trait_id.clone(),
                    TraitInfo {
                        import_specifier: import.clone(),
                        signature: (*definition.signature).clone(),
                    },
                );
                self.push_export(&import, ModuleExportEntry::TraitDefinition(definition));
            }
            vf::TubeFileEntry::TraitReference {
                trait_id,
                import,
                signature,
            } => {
                self.trait_info.insert(
                    trait_id,
                    TraitInfo {
                        import_specifier: *import,
                        signature: *signature,
                    },
                );
            }
            vf::TubeFileEntry::TraitMethodReference {
                method_id,
                trait_id,
                name,
                erased_signature,
                signature,
            } => {
                self.method_info.insert(
                    method_id,
                    MethodInfo {
                        owner: MethodOwner::Trait(trait_id.clone()),
                        parent_import_specifier: self
                            .trait_info
                            .get(&trait_id)
                            .map(|info| info.import_specifier.clone())
                            .unwrap_or(vf::ImportSpecifier::Local {
                                parent: Box::new(vf::ImportSpecifier::Global {
                                    module_id: BigUint::from(0u32),
                                    name: Box::new((*name).clone()),
                                    sig: Box::new((*erased_signature).clone()),
                                }),
                                index: BigUint::from(0u32),
                            }),
                        name: *name,
                        erased_signature: *erased_signature,
                        signature: *signature,
                        definition: None,
                    },
                );
            }
            vf::TubeFileEntry::InstanceDefinition { definition } => {
                let definition = *definition;
                let import = (*definition.import).clone();
                for method in &definition.methods {
                    self.method_info.insert(
                        method.method_id.clone(),
                        MethodInfo {
                            owner: MethodOwner::Instance(definition.instance_id.clone()),
                            parent_import_specifier: import.clone(),
                            name: (*method.name).clone(),
                            erased_signature: (*method.erased_signature).clone(),
                            signature: (*method.signature).clone(),
                            definition: Some((**method).clone()),
                        },
                    );
                }
                self.instance_info.insert(
                    definition.instance_id.clone(),
                    InstanceInfo {
                        import_specifier: import.clone(),
                        signature: (*definition.signature).clone(),
                    },
                );
                self.push_export(&import, ModuleExportEntry::InstanceDefinition(definition));
            }
            vf::TubeFileEntry::InstanceReference {
                instance_id,
                import,
                signature,
            } => {
                self.instance_info.insert(
                    instance_id,
                    InstanceInfo {
                        import_specifier: *import,
                        signature: *signature,
                    },
                );
            }
            vf::TubeFileEntry::InstanceMethodReference {
                method_id,
                instance_id,
                name,
                erased_signature,
                signature,
            } => {
                self.method_info.insert(
                    method_id,
                    MethodInfo {
                        owner: MethodOwner::Instance(instance_id.clone()),
                        parent_import_specifier: self
                            .instance_info
                            .get(&instance_id)
                            .map(|info| info.import_specifier.clone())
                            .unwrap_or(vf::ImportSpecifier::Local {
                                parent: Box::new(vf::ImportSpecifier::Global {
                                    module_id: BigUint::from(0u32),
                                    name: Box::new((*name).clone()),
                                    sig: Box::new((*erased_signature).clone()),
                                }),
                                index: BigUint::from(0u32),
                            }),
                        name: *name,
                        erased_signature: *erased_signature,
                        signature: *signature,
                        definition: None,
                    },
                );
            }
        }
    }

    fn push_export(&mut self, import: &vf::ImportSpecifier, export: ModuleExportEntry) {
        let Some(module_id) = import_module_id(import) else {
            return;
        };

        let Some(module_info) = self.module_info.get(module_id) else {
            return;
        };

        let Some(module) = self
            .modules
            .iter_mut()
            .find(|module| module.path == module_info.path)
        else {
            return;
        };

        module.exports.push(export);
    }

    pub fn tube_name_for_id(&self, tube_id: &BigUint) -> Option<&vf::TubeName> {
        if tube_id == &BigUint::from(0u32) {
            return Some(&self.metadata.name);
        }

        let index = usize::try_from(tube_id - BigUint::from(1u32)).ok()?;
        self.metadata
            .referenced_tubes
            .get(index)
            .map(|tube| tube.name.as_ref())
    }

    fn ensure_tube_reference(&mut self, tube_name: &vf::TubeName) -> BigUint {
        if self.metadata.name.as_ref() == tube_name {
            return BigUint::from(0u32);
        }

        if let Some(index) = self
            .metadata
            .referenced_tubes
            .iter()
            .position(|tube_ref| tube_ref.name.as_ref() == tube_name)
        {
            return BigUint::from(index + 1);
        }

        self.metadata
            .referenced_tubes
            .push(Box::new(vf::TubeReference {
                name: Box::new(tube_name.clone()),
                metadata: None,
            }));

        BigUint::from(self.metadata.referenced_tubes.len())
    }

    pub fn convert_module_id_from(
        &mut self,
        source_tube: &TubeModel,
        module_id: &BigUint,
    ) -> Option<BigUint> {
        let module = source_tube.module_info.get(module_id)?;
        let source_tube_name = source_tube.tube_name_for_id(&module.tube_id)?;

        self.module_info
            .iter()
            .find_map(|(target_module_id, target_module)| {
                let target_tube_name = self.tube_name_for_id(&target_module.tube_id)?;
                (target_tube_name == source_tube_name && target_module.path == module.path)
                    .then(|| target_module_id.clone())
            })
            .or_else(|| {
                let module_id = next_id(&self.module_info);
                let tube_id = self.ensure_tube_reference(source_tube_name);
                self.module_info.insert(
                    module_id.clone(),
                    ModuleInfo {
                        tube_id,
                        path: module.path.clone(),
                    },
                );
                Some(module_id)
            })
    }

    pub fn convert_import_specifier_from(
        &mut self,
        source_tube: &TubeModel,
        import: &vf::ImportSpecifier,
    ) -> Option<vf::ImportSpecifier> {
        match import {
            vf::ImportSpecifier::Global {
                module_id,
                name,
                sig,
            } => Some(vf::ImportSpecifier::Global {
                module_id: self.convert_module_id_from(source_tube, module_id)?,
                name: name.clone(),
                sig: Box::new(self.convert_erased_signature_from(source_tube, sig)?),
            }),
            vf::ImportSpecifier::Local { parent, index } => Some(vf::ImportSpecifier::Local {
                parent: Box::new(self.convert_import_specifier_from(source_tube, parent)?),
                index: index.clone(),
            }),
        }
    }

    pub fn convert_function_signature_from(
        &mut self,
        source_tube: &TubeModel,
        signature: &vf::FunctionSignature,
    ) -> Option<vf::FunctionSignature> {
        Some(vf::FunctionSignature {
            token_parameters: signature
                .token_parameters
                .iter()
                .map(|parameter| {
                    Some(Box::new(vf::SignatureTokenParameter {
                        name: parameter.name.clone(),
                        kind: Box::new(self.convert_token_from(source_tube, &parameter.kind)?),
                    }))
                })
                .collect::<Option<Vec<_>>>()?,
            parameters: signature
                .parameters
                .iter()
                .map(|parameter| {
                    Some(Box::new(vf::SignatureParameter {
                        name: parameter.name.clone(),
                        param_type: Box::new(
                            self.convert_token_from(source_tube, &parameter.param_type)?,
                        ),
                    }))
                })
                .collect::<Option<Vec<_>>>()?,
            return_type: Box::new(self.convert_token_from(source_tube, &signature.return_type)?),
        })
    }

    pub fn convert_erased_signature_from(
        &mut self,
        source_tube: &TubeModel,
        signature: &vf::ErasedSignature,
    ) -> Option<vf::ErasedSignature> {
        Some(vf::ErasedSignature {
            params: signature
                .params
                .iter()
                .map(|param| {
                    self.convert_erased_signature_type_from(source_tube, param)
                        .map(Box::new)
                })
                .collect::<Option<Vec<_>>>()?,
            result: Box::new(
                self.convert_erased_signature_type_from(source_tube, &signature.result)?,
            ),
        })
    }

    pub fn convert_erased_signature_type_from(
        &mut self,
        source_tube: &TubeModel,
        signature_type: &vf::ErasedSignatureType,
    ) -> Option<vf::ErasedSignatureType> {
        match signature_type {
            vf::ErasedSignatureType::Int {} => Some(vf::ErasedSignatureType::Int {}),
            vf::ErasedSignatureType::Bool {} => Some(vf::ErasedSignatureType::Bool {}),
            vf::ErasedSignatureType::String {} => Some(vf::ErasedSignatureType::String {}),
            vf::ErasedSignatureType::Never {} => Some(vf::ErasedSignatureType::Never {}),
            vf::ErasedSignatureType::Array { element_type } => {
                Some(vf::ErasedSignatureType::Array {
                    element_type: Box::new(
                        self.convert_erased_signature_type_from(source_tube, element_type)?,
                    ),
                })
            }
            vf::ErasedSignatureType::Function { input, output } => {
                Some(vf::ErasedSignatureType::Function {
                    input: Box::new(self.convert_erased_signature_type_from(source_tube, input)?),
                    output: Box::new(self.convert_erased_signature_type_from(source_tube, output)?),
                })
            }
            vf::ErasedSignatureType::Record {
                record_import,
                args,
            } => Some(vf::ErasedSignatureType::Record {
                record_import: Box::new(
                    self.convert_import_specifier_from(source_tube, record_import)?,
                ),
                args: args
                    .iter()
                    .map(|arg| {
                        self.convert_erased_signature_type_from(source_tube, arg)
                            .map(Box::new)
                    })
                    .collect::<Option<Vec<_>>>()?,
            }),
            vf::ErasedSignatureType::Tuple { elements } => Some(vf::ErasedSignatureType::Tuple {
                elements: elements
                    .iter()
                    .map(|element| {
                        self.convert_erased_signature_type_from(source_tube, element)
                            .map(Box::new)
                    })
                    .collect::<Option<Vec<_>>>()?,
            }),
            vf::ErasedSignatureType::Erased {} => Some(vf::ErasedSignatureType::Erased {}),
        }
    }

    pub fn convert_token_from(
        &mut self,
        source_tube: &TubeModel,
        token: &vf::Token,
    ) -> Option<vf::Token> {
        match token {
            vf::Token::Builtin { b } => Some(vf::Token::Builtin {
                b: Box::new(self.convert_builtin_type_from(source_tube, b)?),
            }),
            vf::Token::Function { input, output } => Some(vf::Token::Function {
                input: Box::new(self.convert_token_from(source_tube, input)?),
                output: Box::new(self.convert_token_from(source_tube, output)?),
            }),
            vf::Token::FunctionErased { output } => Some(vf::Token::FunctionErased {
                output: Box::new(self.convert_token_from(source_tube, output)?),
            }),
            vf::Token::FunctionToken { token_kind, output } => Some(vf::Token::FunctionToken {
                token_kind: Box::new(self.convert_token_from(source_tube, token_kind)?),
                output: Box::new(self.convert_token_from(source_tube, output)?),
            }),
            vf::Token::InstanceValue { instance_id, args } => Some(vf::Token::InstanceValue {
                instance_id: self.convert_instance_id_from(source_tube, instance_id)?,
                args: self.convert_token_args_from(source_tube, args)?,
            }),
            vf::Token::Record { record_id, args } => Some(vf::Token::Record {
                record_id: self.convert_record_id_from(source_tube, record_id)?,
                args: self.convert_token_args_from(source_tube, args)?,
            }),
            vf::Token::Enum { enum_id, args } => Some(vf::Token::Enum {
                enum_id: self.convert_enum_id_from(source_tube, enum_id)?,
                args: self.convert_token_args_from(source_tube, args)?,
            }),
            vf::Token::ParentTokenParameter { index } => Some(vf::Token::ParentTokenParameter {
                index: index.clone(),
            }),
            vf::Token::RefCell { inner } => Some(vf::Token::RefCell {
                inner: Box::new(self.convert_token_from(source_tube, inner)?),
            }),
            vf::Token::Trait { trait_id, args } => Some(vf::Token::Trait {
                trait_id: self.convert_trait_id_from(source_tube, trait_id)?,
                args: self.convert_token_args_from(source_tube, args)?,
            }),
            vf::Token::InstanceType { instance_id, args } => Some(vf::Token::InstanceType {
                instance_id: self.convert_instance_id_from(source_tube, instance_id)?,
                args: self.convert_token_args_from(source_tube, args)?,
            }),
            vf::Token::Tuple { elements } => Some(vf::Token::Tuple {
                elements: self.convert_token_args_from(source_tube, elements)?,
            }),
            vf::Token::TokenParameter { index } => Some(vf::Token::TokenParameter {
                index: index.clone(),
            }),
            vf::Token::TypeInfo {} => Some(vf::Token::TypeInfo {}),
            vf::Token::Boxed {} => Some(vf::Token::Boxed {}),
        }
    }

    pub fn convert_builtin_type_from(
        &mut self,
        source_tube: &TubeModel,
        builtin_type: &vf::BuiltinType,
    ) -> Option<vf::BuiltinType> {
        match builtin_type {
            vf::BuiltinType::Int {} => Some(vf::BuiltinType::Int {}),
            vf::BuiltinType::Bool {} => Some(vf::BuiltinType::Bool {}),
            vf::BuiltinType::String {} => Some(vf::BuiltinType::String {}),
            vf::BuiltinType::Never {} => Some(vf::BuiltinType::Never {}),
            vf::BuiltinType::Array { element_type } => Some(vf::BuiltinType::Array {
                element_type: Box::new(self.convert_token_from(source_tube, element_type)?),
            }),
            vf::BuiltinType::Conjunction { lhs, rhs } => Some(vf::BuiltinType::Conjunction {
                lhs: Box::new(self.convert_token_from(source_tube, lhs)?),
                rhs: Box::new(self.convert_token_from(source_tube, rhs)?),
            }),
            vf::BuiltinType::Disjunction { lhs, rhs } => Some(vf::BuiltinType::Disjunction {
                lhs: Box::new(self.convert_token_from(source_tube, lhs)?),
                rhs: Box::new(self.convert_token_from(source_tube, rhs)?),
            }),
        }
    }

    pub fn convert_function_body_from(
        &mut self,
        source_tube: &TubeModel,
        body: &vf::FunctionBody,
    ) -> Option<vf::FunctionBody> {
        Some(vf::FunctionBody {
            variables: Box::new(vf::VariableDeclarations {
                variables: body
                    .variables
                    .variables
                    .iter()
                    .map(|variable| {
                        Some(Box::new(vf::VariableDeclaration {
                            r#type: Box::new(
                                self.convert_token_from(source_tube, &variable.r#type)?,
                            ),
                        }))
                    })
                    .collect::<Option<Vec<_>>>()?,
            }),
            region: Box::new(self.convert_region_from(source_tube, &body.region)?),
        })
    }

    pub fn convert_region_from(
        &mut self,
        source_tube: &TubeModel,
        region: &vf::Region,
    ) -> Option<vf::Region> {
        match region {
            vf::Region::BasicBlock { instructions } => Some(vf::Region::BasicBlock {
                instructions: instructions
                    .iter()
                    .map(|instruction| {
                        self.convert_instruction_from(source_tube, instruction)
                            .map(Box::new)
                    })
                    .collect::<Option<Vec<_>>>()?,
            }),
            vf::Region::Sequence { regions } => Some(vf::Region::Sequence {
                regions: regions
                    .iter()
                    .map(|region| self.convert_region_from(source_tube, region).map(Box::new))
                    .collect::<Option<Vec<_>>>()?,
            }),
            vf::Region::Block {
                block_id,
                flags,
                region,
            } => Some(vf::Region::Block {
                block_id: block_id.clone(),
                flags: flags.clone(),
                region: Box::new(self.convert_region_from(source_tube, region)?),
            }),
            vf::Region::IfElse {
                when_true_block_id,
                when_false_block_id,
                condition,
                when_true,
                when_false,
            } => Some(vf::Region::IfElse {
                when_true_block_id: when_true_block_id.clone(),
                when_false_block_id: when_false_block_id.clone(),
                condition: Box::new(self.convert_region_from(source_tube, condition)?),
                when_true: Box::new(self.convert_region_from(source_tube, when_true)?),
                when_false: Box::new(self.convert_region_from(source_tube, when_false)?),
            }),
            vf::Region::Finally { action, ensuring } => Some(vf::Region::Finally {
                action: Box::new(self.convert_region_from(source_tube, action)?),
                ensuring: Box::new(self.convert_region_from(source_tube, ensuring)?),
            }),
        }
    }

    pub fn convert_instruction_from(
        &mut self,
        source_tube: &TubeModel,
        instruction: &vf::Instruction,
    ) -> Option<vf::Instruction> {
        match instruction {
            vf::Instruction::BlockBreak { block_id } => Some(vf::Instruction::BlockBreak {
                block_id: block_id.clone(),
            }),
            vf::Instruction::BlockBreakIf {
                block_id,
                condition,
            } => Some(vf::Instruction::BlockBreakIf {
                block_id: block_id.clone(),
                condition: condition.clone(),
            }),
            vf::Instruction::BlockBreakUnless {
                block_id,
                condition,
            } => Some(vf::Instruction::BlockBreakUnless {
                block_id: block_id.clone(),
                condition: condition.clone(),
            }),
            vf::Instruction::BlockRetry { block_id } => Some(vf::Instruction::BlockRetry {
                block_id: block_id.clone(),
            }),
            vf::Instruction::Box { dest, value } => Some(vf::Instruction::Box {
                dest: dest.clone(),
                value: value.clone(),
            }),
            vf::Instruction::Builtin { op } => Some(vf::Instruction::Builtin {
                op: Box::new(self.convert_builtin_op_from(source_tube, op)?),
            }),
            vf::Instruction::ConstBool { dest, value } => Some(vf::Instruction::ConstBool {
                dest: dest.clone(),
                value: *value,
            }),
            vf::Instruction::ConstInt { dest, value } => Some(vf::Instruction::ConstInt {
                dest: dest.clone(),
                value: value.clone(),
            }),
            vf::Instruction::ConstString { dest, value } => Some(vf::Instruction::ConstString {
                dest: dest.clone(),
                value: value.clone(),
            }),
            vf::Instruction::EnumVariantLiteral {
                dest,
                enum_type,
                variant_id,
                token_args,
                args,
                fields,
            } => Some(vf::Instruction::EnumVariantLiteral {
                dest: dest.clone(),
                enum_type: Box::new(self.convert_token_from(source_tube, enum_type)?),
                variant_id: self.convert_enum_variant_id_from(source_tube, variant_id)?,
                token_args: self.convert_token_args_from(source_tube, token_args)?,
                args: args.clone(),
                fields: self.convert_record_field_literals_from(source_tube, fields)?,
            }),
            vf::Instruction::FunctionCall {
                function_id,
                dest,
                token_args,
                args,
            } => Some(vf::Instruction::FunctionCall {
                function_id: self.convert_function_id_from(source_tube, function_id)?,
                dest: dest.clone(),
                token_args: self.convert_token_args_from(source_tube, token_args)?,
                args: args.clone(),
            }),
            vf::Instruction::FunctionObjectCall {
                dest,
                function,
                arg,
            } => Some(vf::Instruction::FunctionObjectCall {
                dest: dest.clone(),
                function: function.clone(),
                arg: arg.clone(),
            }),
            vf::Instruction::FunctionObjectTokenCall {
                dest,
                function,
                arg,
            } => Some(vf::Instruction::FunctionObjectTokenCall {
                dest: dest.clone(),
                function: function.clone(),
                arg: Box::new(self.convert_token_from(source_tube, arg)?),
            }),
            vf::Instruction::FunctionObjectErasedCall { dest, function } => {
                Some(vf::Instruction::FunctionObjectErasedCall {
                    dest: dest.clone(),
                    function: function.clone(),
                })
            }
            vf::Instruction::InstanceMethodCall {
                dest,
                method_id,
                instance_type,
                instance_object,
                token_args,
                args,
            } => Some(vf::Instruction::InstanceMethodCall {
                dest: dest.clone(),
                method_id: self.convert_method_id_from(source_tube, method_id)?,
                instance_type: Box::new(self.convert_token_from(source_tube, instance_type)?),
                instance_object: instance_object.clone(),
                token_args: self.convert_token_args_from(source_tube, token_args)?,
                args: args.clone(),
            }),
            vf::Instruction::IsEnumVariantOrBreak {
                not_variant_block_id,
                enum_type,
                variant_id,
                value,
                args,
                field_extractors,
            } => Some(vf::Instruction::IsEnumVariantOrBreak {
                not_variant_block_id: not_variant_block_id.clone(),
                enum_type: Box::new(self.convert_token_from(source_tube, enum_type)?),
                variant_id: self.convert_enum_variant_id_from(source_tube, variant_id)?,
                value: value.clone(),
                args: args.clone(),
                field_extractors: field_extractors
                    .iter()
                    .map(|extractor| {
                        self.convert_field_extractor_from(source_tube, extractor)
                            .map(Box::new)
                    })
                    .collect::<Option<Vec<_>>>()?,
            }),
            vf::Instruction::LoadReference { dest, r#ref } => {
                Some(vf::Instruction::LoadReference {
                    dest: dest.clone(),
                    r#ref: r#ref.clone(),
                })
            }
            vf::Instruction::LoadToken { dest, token } => Some(vf::Instruction::LoadToken {
                dest: dest.clone(),
                token: Box::new(self.convert_token_from(source_tube, token)?),
            }),
            vf::Instruction::LoadInstanceField {
                dest,
                instance_object,
                instance_type,
                parameter_index,
            } => Some(vf::Instruction::LoadInstanceField {
                dest: dest.clone(),
                instance_object: instance_object.clone(),
                instance_type: Box::new(self.convert_token_from(source_tube, instance_type)?),
                parameter_index: parameter_index.clone(),
            }),
            vf::Instruction::Move { dest, src } => Some(vf::Instruction::Move {
                dest: dest.clone(),
                src: src.clone(),
            }),
            vf::Instruction::NewInstance {
                instance_id,
                dest,
                token_args,
                args,
            } => Some(vf::Instruction::NewInstance {
                instance_id: self.convert_instance_id_from(source_tube, instance_id)?,
                dest: dest.clone(),
                token_args: self.convert_token_args_from(source_tube, token_args)?,
                args: args.clone(),
            }),
            vf::Instruction::NewReference { dest, value } => Some(vf::Instruction::NewReference {
                dest: dest.clone(),
                value: value.clone(),
            }),
            vf::Instruction::PartiallyAppliedFunction {
                function_id,
                dest,
                token_args,
                args,
            } => Some(vf::Instruction::PartiallyAppliedFunction {
                function_id: self.convert_function_id_from(source_tube, function_id)?,
                dest: dest.clone(),
                token_args: self.convert_token_args_from(source_tube, token_args)?,
                args: args.clone(),
            }),
            vf::Instruction::PartiallyAppliedTokenFunction {
                function_id,
                dest,
                token_args,
                args,
            } => Some(vf::Instruction::PartiallyAppliedTokenFunction {
                function_id: self.convert_function_id_from(source_tube, function_id)?,
                dest: dest.clone(),
                token_args: self.convert_token_args_from(source_tube, token_args)?,
                args: args.clone(),
            }),
            vf::Instruction::PartiallyAppliedFunctionErased {
                function_id,
                dest,
                token_args,
                args,
            } => Some(vf::Instruction::PartiallyAppliedFunctionErased {
                function_id: self.convert_function_id_from(source_tube, function_id)?,
                dest: dest.clone(),
                token_args: self.convert_token_args_from(source_tube, token_args)?,
                args: args.clone(),
            }),
            vf::Instruction::Raise { exception } => Some(vf::Instruction::Raise {
                exception: exception.clone(),
            }),
            vf::Instruction::RecordFieldLoad {
                field_id,
                dest,
                record_value,
            } => Some(vf::Instruction::RecordFieldLoad {
                field_id: self.convert_record_field_id_from(source_tube, field_id)?,
                dest: dest.clone(),
                record_value: record_value.clone(),
            }),
            vf::Instruction::RecordFieldStore {
                field_id,
                record_value,
                field_value,
            } => Some(vf::Instruction::RecordFieldStore {
                field_id: self.convert_record_field_id_from(source_tube, field_id)?,
                record_value: record_value.clone(),
                field_value: field_value.clone(),
            }),
            vf::Instruction::RecordLiteral {
                dest,
                record_type,
                fields,
            } => Some(vf::Instruction::RecordLiteral {
                dest: dest.clone(),
                record_type: Box::new(self.convert_token_from(source_tube, record_type)?),
                fields: self.convert_record_field_literals_from(source_tube, fields)?,
            }),
            vf::Instruction::Return { src } => Some(vf::Instruction::Return { src: src.clone() }),
            vf::Instruction::Tuple { dest, values } => Some(vf::Instruction::Tuple {
                dest: dest.clone(),
                values: values.clone(),
            }),
            vf::Instruction::TupleElement {
                element_index,
                dest,
                src,
            } => Some(vf::Instruction::TupleElement {
                element_index: element_index.clone(),
                dest: dest.clone(),
                src: src.clone(),
            }),
            vf::Instruction::Unbox {
                dest,
                r#type,
                value,
            } => Some(vf::Instruction::Unbox {
                dest: dest.clone(),
                r#type: Box::new(self.convert_token_from(source_tube, r#type)?),
                value: value.clone(),
            }),
            vf::Instruction::Unreachable {} => Some(vf::Instruction::Unreachable {}),
            vf::Instruction::UpdateReference { r#ref, value } => {
                Some(vf::Instruction::UpdateReference {
                    r#ref: r#ref.clone(),
                    value: value.clone(),
                })
            }
        }
    }

    pub fn convert_builtin_op_from(
        &mut self,
        source_tube: &TubeModel,
        op: &vf::BuiltinOp,
    ) -> Option<vf::BuiltinOp> {
        match op {
            vf::BuiltinOp::IntNegate { dest, value } => Some(vf::BuiltinOp::IntNegate {
                dest: dest.clone(),
                value: value.clone(),
            }),
            vf::BuiltinOp::IntBitNot { dest, value } => Some(vf::BuiltinOp::IntBitNot {
                dest: dest.clone(),
                value: value.clone(),
            }),
            vf::BuiltinOp::IntAdd { dest, lhs, rhs } => Some(vf::BuiltinOp::IntAdd {
                dest: dest.clone(),
                lhs: lhs.clone(),
                rhs: rhs.clone(),
            }),
            vf::BuiltinOp::IntSub { dest, lhs, rhs } => Some(vf::BuiltinOp::IntSub {
                dest: dest.clone(),
                lhs: lhs.clone(),
                rhs: rhs.clone(),
            }),
            vf::BuiltinOp::IntMul { dest, lhs, rhs } => Some(vf::BuiltinOp::IntMul {
                dest: dest.clone(),
                lhs: lhs.clone(),
                rhs: rhs.clone(),
            }),
            vf::BuiltinOp::IntBitAnd { dest, lhs, rhs } => Some(vf::BuiltinOp::IntBitAnd {
                dest: dest.clone(),
                lhs: lhs.clone(),
                rhs: rhs.clone(),
            }),
            vf::BuiltinOp::IntBitOr { dest, lhs, rhs } => Some(vf::BuiltinOp::IntBitOr {
                dest: dest.clone(),
                lhs: lhs.clone(),
                rhs: rhs.clone(),
            }),
            vf::BuiltinOp::IntBitXor { dest, lhs, rhs } => Some(vf::BuiltinOp::IntBitXor {
                dest: dest.clone(),
                lhs: lhs.clone(),
                rhs: rhs.clone(),
            }),
            vf::BuiltinOp::IntBitShiftLeft { dest, lhs, rhs } => {
                Some(vf::BuiltinOp::IntBitShiftLeft {
                    dest: dest.clone(),
                    lhs: lhs.clone(),
                    rhs: rhs.clone(),
                })
            }
            vf::BuiltinOp::IntBitShiftRight { dest, lhs, rhs } => {
                Some(vf::BuiltinOp::IntBitShiftRight {
                    dest: dest.clone(),
                    lhs: lhs.clone(),
                    rhs: rhs.clone(),
                })
            }
            vf::BuiltinOp::IntEq { dest, lhs, rhs } => Some(vf::BuiltinOp::IntEq {
                dest: dest.clone(),
                lhs: lhs.clone(),
                rhs: rhs.clone(),
            }),
            vf::BuiltinOp::IntLt { dest, lhs, rhs } => Some(vf::BuiltinOp::IntLt {
                dest: dest.clone(),
                lhs: lhs.clone(),
                rhs: rhs.clone(),
            }),
            vf::BuiltinOp::IntLe { dest, lhs, rhs } => Some(vf::BuiltinOp::IntLe {
                dest: dest.clone(),
                lhs: lhs.clone(),
                rhs: rhs.clone(),
            }),
            vf::BuiltinOp::IntGt { dest, lhs, rhs } => Some(vf::BuiltinOp::IntGt {
                dest: dest.clone(),
                lhs: lhs.clone(),
                rhs: rhs.clone(),
            }),
            vf::BuiltinOp::IntGe { dest, lhs, rhs } => Some(vf::BuiltinOp::IntGe {
                dest: dest.clone(),
                lhs: lhs.clone(),
                rhs: rhs.clone(),
            }),
            vf::BuiltinOp::StringConcat { dest, args } => Some(vf::BuiltinOp::StringConcat {
                dest: dest.clone(),
                args: args.clone(),
            }),
            vf::BuiltinOp::StringEq { dest, lhs, rhs } => Some(vf::BuiltinOp::StringEq {
                dest: dest.clone(),
                lhs: lhs.clone(),
                rhs: rhs.clone(),
            }),
            vf::BuiltinOp::BoolNot { dest, value } => Some(vf::BuiltinOp::BoolNot {
                dest: dest.clone(),
                value: value.clone(),
            }),
            vf::BuiltinOp::BoolEq { dest, lhs, rhs } => Some(vf::BuiltinOp::BoolEq {
                dest: dest.clone(),
                lhs: lhs.clone(),
                rhs: rhs.clone(),
            }),
            vf::BuiltinOp::ArrayCreateUnsafeUninitialized {
                element_type,
                dest,
                length,
            } => Some(vf::BuiltinOp::ArrayCreateUnsafeUninitialized {
                element_type: Box::new(self.convert_token_from(source_tube, element_type)?),
                dest: dest.clone(),
                length: length.clone(),
            }),
            vf::BuiltinOp::ArrayLength {
                element_type,
                dest,
                array,
            } => Some(vf::BuiltinOp::ArrayLength {
                element_type: Box::new(self.convert_token_from(source_tube, element_type)?),
                dest: dest.clone(),
                array: array.clone(),
            }),
            vf::BuiltinOp::ArrayGet {
                element_type,
                dest,
                array,
                index,
            } => Some(vf::BuiltinOp::ArrayGet {
                element_type: Box::new(self.convert_token_from(source_tube, element_type)?),
                dest: dest.clone(),
                array: array.clone(),
                index: index.clone(),
            }),
            vf::BuiltinOp::ArraySet {
                element_type,
                array,
                index,
                value,
            } => Some(vf::BuiltinOp::ArraySet {
                element_type: Box::new(self.convert_token_from(source_tube, element_type)?),
                array: array.clone(),
                index: index.clone(),
                value: value.clone(),
            }),
        }
    }

    pub fn convert_function_id_from(
        &mut self,
        source_tube: &TubeModel,
        function_id: &BigUint,
    ) -> Option<BigUint> {
        let source_info = source_tube.function_info.get(function_id)?;
        let target_import =
            self.convert_import_specifier_from(source_tube, &source_info.import_specifier)?;

        if let Some(function_id) = find_id_by_import(&self.function_info, &target_import) {
            return Some(function_id);
        }

        let function_id = next_id(&self.function_info);
        let signature = self.convert_function_signature_from(source_tube, &source_info.signature)?;
        self.function_info.insert(
            function_id.clone(),
            FunctionInfo {
                import_specifier: target_import,
                signature,
            },
        );
        Some(function_id)
    }

    pub fn convert_record_id_from(
        &mut self,
        source_tube: &TubeModel,
        record_id: &BigUint,
    ) -> Option<BigUint> {
        let source_info = source_tube.record_info.get(record_id)?;
        let target_import =
            self.convert_import_specifier_from(source_tube, &source_info.import_specifier)?;

        if let Some(record_id) = find_id_by_import(&self.record_info, &target_import) {
            return Some(record_id);
        }

        let record_id = next_id(&self.record_info);
        let signature = self.convert_function_signature_from(source_tube, &source_info.signature)?;
        self.record_info.insert(
            record_id.clone(),
            RecordInfo {
                import_specifier: target_import,
                signature,
            },
        );
        Some(record_id)
    }

    pub fn convert_enum_id_from(
        &mut self,
        source_tube: &TubeModel,
        enum_id: &BigUint,
    ) -> Option<BigUint> {
        let source_info = source_tube.enum_info.get(enum_id)?;
        let target_import =
            self.convert_import_specifier_from(source_tube, &source_info.import_specifier)?;

        if let Some(enum_id) = find_id_by_import(&self.enum_info, &target_import) {
            return Some(enum_id);
        }

        let enum_id = next_id(&self.enum_info);
        let signature = self.convert_function_signature_from(source_tube, &source_info.signature)?;
        self.enum_info.insert(
            enum_id.clone(),
            EnumInfo {
                import_specifier: target_import,
                signature,
            },
        );
        Some(enum_id)
    }

    pub fn convert_trait_id_from(
        &mut self,
        source_tube: &TubeModel,
        trait_id: &BigUint,
    ) -> Option<BigUint> {
        let source_info = source_tube.trait_info.get(trait_id)?;
        let target_import =
            self.convert_import_specifier_from(source_tube, &source_info.import_specifier)?;

        if let Some(trait_id) = find_id_by_import(&self.trait_info, &target_import) {
            return Some(trait_id);
        }

        let trait_id = next_id(&self.trait_info);
        let signature = self.convert_function_signature_from(source_tube, &source_info.signature)?;
        self.trait_info.insert(
            trait_id.clone(),
            TraitInfo {
                import_specifier: target_import,
                signature,
            },
        );
        Some(trait_id)
    }

    pub fn convert_instance_id_from(
        &mut self,
        source_tube: &TubeModel,
        instance_id: &BigUint,
    ) -> Option<BigUint> {
        let source_info = source_tube.instance_info.get(instance_id)?;
        let target_import =
            self.convert_import_specifier_from(source_tube, &source_info.import_specifier)?;

        if let Some(instance_id) = find_id_by_import(&self.instance_info, &target_import) {
            return Some(instance_id);
        }

        let instance_id = next_id(&self.instance_info);
        let signature = self.convert_function_signature_from(source_tube, &source_info.signature)?;
        self.instance_info.insert(
            instance_id.clone(),
            InstanceInfo {
                import_specifier: target_import,
                signature,
            },
        );
        Some(instance_id)
    }

    pub fn convert_enum_variant_id_from(
        &mut self,
        source_tube: &TubeModel,
        variant_id: &BigUint,
    ) -> Option<BigUint> {
        let source_info = source_tube.enum_variant_info.get(variant_id)?;
        let target_enum_id = self.convert_enum_id_from(source_tube, &source_info.enum_id)?;

        if let Some(variant_id) = self
            .enum_variant_info
            .iter()
            .find_map(|(target_variant_id, target_info)| {
                (target_info.enum_id == target_enum_id && target_info.name == source_info.name)
                    .then(|| target_variant_id.clone())
            })
        {
            return Some(variant_id);
        }

        let variant_id = next_id(&self.enum_variant_info);
        let signature =
            self.convert_function_signature_from(source_tube, &source_info.signature)?;
        self.enum_variant_info.insert(
            variant_id.clone(),
            EnumVariantInfo {
                enum_id: target_enum_id,
                name: source_info.name.clone(),
                signature,
            },
        );
        Some(variant_id)
    }

    pub fn convert_record_field_id_from(
        &mut self,
        source_tube: &TubeModel,
        record_field_id: &BigUint,
    ) -> Option<BigUint> {
        let source_info = source_tube.record_field_info.get(record_field_id)?;
        let (target_owner, target_owner_id) = match &source_info.owner {
            RecordFieldOwner::Record => (
                RecordFieldOwner::Record,
                self.convert_record_id_from(source_tube, &source_info.owner_id)?,
            ),
            RecordFieldOwner::EnumVariant => (
                RecordFieldOwner::EnumVariant,
                self.convert_enum_variant_id_from(source_tube, &source_info.owner_id)?,
            ),
        };

        if let Some(record_field_id) = self
            .record_field_info
            .iter()
            .find_map(|(target_field_id, target_info)| {
                (target_info.owner == target_owner
                    && target_info.owner_id == target_owner_id
                    && target_info.name == source_info.name)
                    .then(|| target_field_id.clone())
            })
        {
            return Some(record_field_id);
        }

        let record_field_id = next_id(&self.record_field_info);
        let field_type = self.convert_token_from(source_tube, &source_info.field_type)?;
        self.record_field_info.insert(
            record_field_id.clone(),
            RecordFieldInfo {
                owner: target_owner,
                owner_id: target_owner_id,
                name: source_info.name.clone(),
                field_type,
            },
        );
        Some(record_field_id)
    }

    pub fn convert_method_id_from(
        &mut self,
        source_tube: &TubeModel,
        method_id: &BigUint,
    ) -> Option<BigUint> {
        let source_info = source_tube.method_info.get(method_id)?;
        let target_owner = match &source_info.owner {
            MethodOwner::Trait(trait_id) => {
                MethodOwner::Trait(self.convert_trait_id_from(source_tube, trait_id)?)
            }
            MethodOwner::Instance(instance_id) => {
                MethodOwner::Instance(self.convert_instance_id_from(source_tube, instance_id)?)
            }
        };
        let target_erased_signature =
            self.convert_erased_signature_from(source_tube, &source_info.erased_signature)?;

        if let Some(method_id) = self
            .method_info
            .iter()
            .find_map(|(target_method_id, target_info)| {
                (target_info.owner == target_owner
                    && target_info.name == source_info.name
                    && target_info.erased_signature == target_erased_signature)
                    .then(|| target_method_id.clone())
            })
        {
            return Some(method_id);
        }

        let method_id = next_id(&self.method_info);
        let signature = self.convert_function_signature_from(source_tube, &source_info.signature)?;
        let parent_import_specifier =
            self.convert_import_specifier_from(source_tube, &source_info.parent_import_specifier)?;
        self.method_info.insert(
            method_id.clone(),
            MethodInfo {
                owner: target_owner,
                parent_import_specifier,
                name: source_info.name.clone(),
                erased_signature: target_erased_signature,
                signature,
                definition: None,
            },
        );
        Some(method_id)
    }

    fn convert_token_args_from(
        &mut self,
        source_tube: &TubeModel,
        args: &[Box<vf::Token>],
    ) -> Option<Vec<Box<vf::Token>>> {
        args.iter()
            .map(|arg| self.convert_token_from(source_tube, arg).map(Box::new))
            .collect()
    }

    fn convert_record_field_literals_from(
        &mut self,
        source_tube: &TubeModel,
        fields: &[Box<vf::RecordFieldLiteral>],
    ) -> Option<Vec<Box<vf::RecordFieldLiteral>>> {
        fields
            .iter()
            .map(|field| {
                Some(Box::new(vf::RecordFieldLiteral {
                    field_id: self.convert_record_field_id_from(source_tube, &field.field_id)?,
                    value: field.value.clone(),
                }))
            })
            .collect()
    }

    fn convert_field_extractor_from(
        &mut self,
        source_tube: &TubeModel,
        extractor: &vf::FieldExtractor,
    ) -> Option<vf::FieldExtractor> {
        Some(vf::FieldExtractor {
            r: extractor.r.clone(),
            field_id: self.convert_record_field_id_from(source_tube, &extractor.field_id)?,
        })
    }
}

pub struct ModuleModel {
    pub path: vf::ModulePath,
    pub exports: Vec<ModuleExportEntry>,
}

pub enum ModuleExportEntry {
    FunctionDefinition(vf::FunctionDefinition),
    RecordDefinition(vf::RecordDefinition),
    EnumDefinition(vf::EnumDefinition),
    TraitDefinition(vf::TraitDefinition),
    InstanceDefinition(vf::InstanceDefinition),
}

pub struct ModuleInfo {
    pub tube_id: BigUint,
    pub path: vf::ModulePath,
}

pub struct FunctionInfo {
    pub import_specifier: vf::ImportSpecifier,
    pub signature: vf::FunctionSignature,
}

pub struct RecordInfo {
    pub import_specifier: vf::ImportSpecifier,
    pub signature: vf::FunctionSignature,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum RecordFieldOwner {
    Record,
    EnumVariant,
}

pub struct RecordFieldInfo {
    pub owner: RecordFieldOwner,
    pub owner_id: BigUint,
    pub name: vf::Identifier,
    pub field_type: vf::Token,
}

pub struct EnumInfo {
    pub import_specifier: vf::ImportSpecifier,
    pub signature: vf::FunctionSignature,
}

pub struct EnumVariantInfo {
    pub enum_id: BigUint,
    pub name: vf::Identifier,
    pub signature: vf::FunctionSignature,
}

pub struct TraitInfo {
    pub import_specifier: vf::ImportSpecifier,
    pub signature: vf::FunctionSignature,
}

#[derive(Clone, PartialEq, Eq)]
pub enum MethodOwner {
    Trait(BigUint),
    Instance(BigUint),
}

pub struct MethodInfo {
    pub owner: MethodOwner,
    pub parent_import_specifier: vf::ImportSpecifier,
    pub name: vf::Identifier,
    pub erased_signature: vf::ErasedSignature,
    pub signature: vf::FunctionSignature,
    pub definition: Option<vf::MethodDefinition>,
}

pub struct InstanceInfo {
    pub import_specifier: vf::ImportSpecifier,
    pub signature: vf::FunctionSignature,
}

trait HasImportSpecifier {
    fn import_specifier(&self) -> &vf::ImportSpecifier;
}

impl HasImportSpecifier for FunctionInfo {
    fn import_specifier(&self) -> &vf::ImportSpecifier {
        &self.import_specifier
    }
}

impl HasImportSpecifier for RecordInfo {
    fn import_specifier(&self) -> &vf::ImportSpecifier {
        &self.import_specifier
    }
}

impl HasImportSpecifier for EnumInfo {
    fn import_specifier(&self) -> &vf::ImportSpecifier {
        &self.import_specifier
    }
}

impl HasImportSpecifier for TraitInfo {
    fn import_specifier(&self) -> &vf::ImportSpecifier {
        &self.import_specifier
    }
}

impl HasImportSpecifier for InstanceInfo {
    fn import_specifier(&self) -> &vf::ImportSpecifier {
        &self.import_specifier
    }
}

#[derive(Default)]
pub struct ProgramIR<'a> {
    pub current_tube: Option<&'a mut TubeModel>,
    pub referenced_tubes: HashMap<vf::TubeName, &'a TubeModel>,
}

impl<'a> ProgramIR<'a> {
    pub fn new(
        current_tube: &'a mut TubeModel,
        referenced_tubes: impl IntoIterator<Item = &'a TubeModel>,
    ) -> Self {
        let referenced_tubes = referenced_tubes
            .into_iter()
            .map(|tube| ((*tube.metadata.name).clone(), tube))
            .collect::<HashMap<_, _>>();

        Self {
            current_tube: Some(current_tube),
            referenced_tubes,
        }
    }

    pub fn current_tube(&self) -> Option<&TubeModel> {
        self.current_tube.as_deref()
    }

    pub fn current_tube_mut(&mut self) -> Option<&mut TubeModel> {
        self.current_tube.as_deref_mut()
    }

    pub fn current_and_referenced_tubes_mut(
        &mut self,
    ) -> (Option<&mut TubeModel>, &HashMap<vf::TubeName, &'a TubeModel>) {
        (self.current_tube.as_deref_mut(), &self.referenced_tubes)
    }

    pub fn tube(&self, name: &vf::TubeName) -> Option<&TubeModel> {
        if self
            .current_tube()
            .is_some_and(|tube| tube.metadata.name.as_ref() == name)
        {
            self.current_tube()
        } else {
            self.referenced_tubes.get(name).copied()
        }
    }

    pub fn referenced_tube(&self, name: &vf::TubeName) -> Option<&'a TubeModel> {
        self.referenced_tubes.get(name).copied()
    }
}

fn import_module_id(import: &vf::ImportSpecifier) -> Option<&BigUint> {
    match import {
        vf::ImportSpecifier::Global { module_id, .. } => Some(module_id),
        vf::ImportSpecifier::Local { parent, .. } => import_module_id(parent),
    }
}

fn find_id_by_import<V>(map: &HashMap<BigUint, V>, import: &vf::ImportSpecifier) -> Option<BigUint>
where
    V: HasImportSpecifier,
{
    map.iter()
        .find_map(|(id, info)| (info.import_specifier() == import).then(|| id.clone()))
}

fn substitute_token_arguments(
    token: &vf::Token,
    token_args: &[Box<vf::Token>],
) -> Option<vf::Token> {
    match token {
        vf::Token::Builtin { b } => Some(vf::Token::Builtin {
            b: Box::new(substitute_builtin_type_token_arguments(b, token_args)?),
        }),
        vf::Token::Function { input, output } => Some(vf::Token::Function {
            input: Box::new(substitute_token_arguments(input, token_args)?),
            output: Box::new(substitute_token_arguments(output, token_args)?),
        }),
        vf::Token::FunctionErased { output } => Some(vf::Token::FunctionErased {
            output: Box::new(substitute_token_arguments(output, token_args)?),
        }),
        vf::Token::FunctionToken { token_kind, output } => Some(vf::Token::FunctionToken {
            token_kind: Box::new(substitute_token_arguments(token_kind, token_args)?),
            output: Box::new(substitute_token_arguments(output, token_args)?),
        }),
        vf::Token::InstanceValue { instance_id, args } => Some(vf::Token::InstanceValue {
            instance_id: instance_id.clone(),
            args: substitute_token_argument_list(args, token_args)?,
        }),
        vf::Token::Record { record_id, args } => Some(vf::Token::Record {
            record_id: record_id.clone(),
            args: substitute_token_argument_list(args, token_args)?,
        }),
        vf::Token::Enum { enum_id, args } => Some(vf::Token::Enum {
            enum_id: enum_id.clone(),
            args: substitute_token_argument_list(args, token_args)?,
        }),
        vf::Token::ParentTokenParameter { index } => Some(vf::Token::ParentTokenParameter {
            index: index.clone(),
        }),
        vf::Token::RefCell { inner } => Some(vf::Token::RefCell {
            inner: Box::new(substitute_token_arguments(inner, token_args)?),
        }),
        vf::Token::Trait { trait_id, args } => Some(vf::Token::Trait {
            trait_id: trait_id.clone(),
            args: substitute_token_argument_list(args, token_args)?,
        }),
        vf::Token::InstanceType { instance_id, args } => Some(vf::Token::InstanceType {
            instance_id: instance_id.clone(),
            args: substitute_token_argument_list(args, token_args)?,
        }),
        vf::Token::Tuple { elements } => Some(vf::Token::Tuple {
            elements: substitute_token_argument_list(elements, token_args)?,
        }),
        vf::Token::TokenParameter { index } => {
            let index = usize::try_from(index.clone()).ok()?;
            token_args.get(index).map(|token| token.as_ref().clone())
        }
        vf::Token::TypeInfo {} => Some(vf::Token::TypeInfo {}),
        vf::Token::Boxed {} => Some(vf::Token::Boxed {}),
    }
}

fn substitute_builtin_type_token_arguments(
    builtin_type: &vf::BuiltinType,
    token_args: &[Box<vf::Token>],
) -> Option<vf::BuiltinType> {
    match builtin_type {
        vf::BuiltinType::Int {} => Some(vf::BuiltinType::Int {}),
        vf::BuiltinType::Bool {} => Some(vf::BuiltinType::Bool {}),
        vf::BuiltinType::String {} => Some(vf::BuiltinType::String {}),
        vf::BuiltinType::Never {} => Some(vf::BuiltinType::Never {}),
        vf::BuiltinType::Array { element_type } => Some(vf::BuiltinType::Array {
            element_type: Box::new(substitute_token_arguments(element_type, token_args)?),
        }),
        vf::BuiltinType::Conjunction { lhs, rhs } => Some(vf::BuiltinType::Conjunction {
            lhs: Box::new(substitute_token_arguments(lhs, token_args)?),
            rhs: Box::new(substitute_token_arguments(rhs, token_args)?),
        }),
        vf::BuiltinType::Disjunction { lhs, rhs } => Some(vf::BuiltinType::Disjunction {
            lhs: Box::new(substitute_token_arguments(lhs, token_args)?),
            rhs: Box::new(substitute_token_arguments(rhs, token_args)?),
        }),
    }
}

fn substitute_token_argument_list(
    args: &[Box<vf::Token>],
    token_args: &[Box<vf::Token>],
) -> Option<Vec<Box<vf::Token>>> {
    args.iter()
        .map(|arg| substitute_token_arguments(arg, token_args).map(Box::new))
        .collect()
}

fn prepare_region_for_inline(
    region: &vf::Region,
    token_args: &[Box<vf::Token>],
    register_replacements: &HashMap<BigUint, BigUint>,
    block_replacements: &HashMap<BigUint, BigUint>,
    return_register: Option<&vf::RegisterId>,
) -> Option<vf::Region> {
    match region {
        vf::Region::BasicBlock { instructions } => Some(vf::Region::BasicBlock {
            instructions: instructions
                .iter()
                .map(|instruction| {
                    prepare_instruction_for_inline(
                        instruction,
                        token_args,
                        register_replacements,
                        block_replacements,
                        return_register,
                    )
                    .map(Box::new)
                })
                .collect::<Option<Vec<_>>>()?,
        }),
        vf::Region::Sequence { regions } => Some(vf::Region::Sequence {
            regions: regions
                .iter()
                .map(|region| {
                    prepare_region_for_inline(
                        region,
                        token_args,
                        register_replacements,
                        block_replacements,
                        return_register,
                    )
                    .map(Box::new)
                })
                .collect::<Option<Vec<_>>>()?,
        }),
        vf::Region::Block {
            block_id,
            flags,
            region,
        } => Some(vf::Region::Block {
            block_id: Box::new(remap_block_id(block_id, block_replacements)?),
            flags: flags.clone(),
            region: Box::new(prepare_region_for_inline(
                region,
                token_args,
                register_replacements,
                block_replacements,
                return_register,
            )?),
        }),
        vf::Region::IfElse {
            when_true_block_id,
            when_false_block_id,
            condition,
            when_true,
            when_false,
        } => Some(vf::Region::IfElse {
            when_true_block_id: Box::new(remap_block_id(
                when_true_block_id,
                block_replacements,
            )?),
            when_false_block_id: Box::new(remap_block_id(
                when_false_block_id,
                block_replacements,
            )?),
            condition: Box::new(prepare_region_for_inline(
                condition,
                token_args,
                register_replacements,
                block_replacements,
                return_register,
            )?),
            when_true: Box::new(prepare_region_for_inline(
                when_true,
                token_args,
                register_replacements,
                block_replacements,
                return_register,
            )?),
            when_false: Box::new(prepare_region_for_inline(
                when_false,
                token_args,
                register_replacements,
                block_replacements,
                return_register,
            )?),
        }),
        vf::Region::Finally { action, ensuring } => Some(vf::Region::Finally {
            action: Box::new(prepare_region_for_inline(
                action,
                token_args,
                register_replacements,
                block_replacements,
                return_register,
            )?),
            ensuring: Box::new(prepare_region_for_inline(
                ensuring,
                token_args,
                register_replacements,
                block_replacements,
                return_register,
            )?),
        }),
    }
}

fn prepare_instruction_for_inline(
    instruction: &vf::Instruction,
    token_args: &[Box<vf::Token>],
    register_replacements: &HashMap<BigUint, BigUint>,
    block_replacements: &HashMap<BigUint, BigUint>,
    return_register: Option<&vf::RegisterId>,
) -> Option<vf::Instruction> {
    match instruction {
        vf::Instruction::BlockBreak { block_id } => Some(vf::Instruction::BlockBreak {
            block_id: Box::new(remap_block_id(block_id, block_replacements)?),
        }),
        vf::Instruction::BlockBreakIf {
            block_id,
            condition,
        } => Some(vf::Instruction::BlockBreakIf {
            block_id: Box::new(remap_block_id(block_id, block_replacements)?),
            condition: Box::new(remap_register(condition, register_replacements)?),
        }),
        vf::Instruction::BlockBreakUnless {
            block_id,
            condition,
        } => Some(vf::Instruction::BlockBreakUnless {
            block_id: Box::new(remap_block_id(block_id, block_replacements)?),
            condition: Box::new(remap_register(condition, register_replacements)?),
        }),
        vf::Instruction::BlockRetry { block_id } => Some(vf::Instruction::BlockRetry {
            block_id: Box::new(remap_block_id(block_id, block_replacements)?),
        }),
        vf::Instruction::Box { dest, value } => Some(vf::Instruction::Box {
            dest: Box::new(remap_register(dest, register_replacements)?),
            value: Box::new(remap_register(value, register_replacements)?),
        }),
        vf::Instruction::Builtin { op } => Some(vf::Instruction::Builtin {
            op: Box::new(prepare_builtin_op_for_inline(
                op,
                token_args,
                register_replacements,
            )?),
        }),
        vf::Instruction::ConstBool { dest, value } => Some(vf::Instruction::ConstBool {
            dest: Box::new(remap_register(dest, register_replacements)?),
            value: *value,
        }),
        vf::Instruction::ConstInt { dest, value } => Some(vf::Instruction::ConstInt {
            dest: Box::new(remap_register(dest, register_replacements)?),
            value: value.clone(),
        }),
        vf::Instruction::ConstString { dest, value } => Some(vf::Instruction::ConstString {
            dest: Box::new(remap_register(dest, register_replacements)?),
            value: value.clone(),
        }),
        vf::Instruction::EnumVariantLiteral {
            dest,
            enum_type,
            variant_id,
            token_args: instruction_token_args,
            args,
            fields,
        } => Some(vf::Instruction::EnumVariantLiteral {
            dest: Box::new(remap_register(dest, register_replacements)?),
            enum_type: Box::new(substitute_token_arguments(enum_type, token_args)?),
            variant_id: variant_id.clone(),
            token_args: substitute_token_argument_list(instruction_token_args, token_args)?,
            args: remap_register_list(args, register_replacements)?,
            fields: prepare_record_field_literals_for_inline(fields, register_replacements)?,
        }),
        vf::Instruction::FunctionCall {
            function_id,
            dest,
            token_args: instruction_token_args,
            args,
        } => Some(vf::Instruction::FunctionCall {
            function_id: function_id.clone(),
            dest: Box::new(prepare_function_result_for_inline(
                dest,
                register_replacements,
                return_register,
            )?),
            token_args: substitute_token_argument_list(instruction_token_args, token_args)?,
            args: remap_register_list(args, register_replacements)?,
        }),
        vf::Instruction::FunctionObjectCall {
            dest,
            function,
            arg,
        } => Some(vf::Instruction::FunctionObjectCall {
            dest: Box::new(prepare_function_result_for_inline(
                dest,
                register_replacements,
                return_register,
            )?),
            function: Box::new(remap_register(function, register_replacements)?),
            arg: Box::new(remap_register(arg, register_replacements)?),
        }),
        vf::Instruction::FunctionObjectTokenCall {
            dest,
            function,
            arg,
        } => Some(vf::Instruction::FunctionObjectTokenCall {
            dest: Box::new(prepare_function_result_for_inline(
                dest,
                register_replacements,
                return_register,
            )?),
            function: Box::new(remap_register(function, register_replacements)?),
            arg: Box::new(substitute_token_arguments(arg, token_args)?),
        }),
        vf::Instruction::FunctionObjectErasedCall { dest, function } => {
            Some(vf::Instruction::FunctionObjectErasedCall {
                dest: Box::new(prepare_function_result_for_inline(
                    dest,
                    register_replacements,
                    return_register,
                )?),
                function: Box::new(remap_register(function, register_replacements)?),
            })
        }
        vf::Instruction::InstanceMethodCall {
            dest,
            method_id,
            instance_type,
            instance_object,
            token_args: instruction_token_args,
            args,
        } => Some(vf::Instruction::InstanceMethodCall {
            dest: Box::new(prepare_function_result_for_inline(
                dest,
                register_replacements,
                return_register,
            )?),
            method_id: method_id.clone(),
            instance_type: Box::new(substitute_token_arguments(instance_type, token_args)?),
            instance_object: Box::new(remap_register(instance_object, register_replacements)?),
            token_args: substitute_token_argument_list(instruction_token_args, token_args)?,
            args: remap_register_list(args, register_replacements)?,
        }),
        vf::Instruction::IsEnumVariantOrBreak {
            not_variant_block_id,
            enum_type,
            variant_id,
            value,
            args,
            field_extractors,
        } => Some(vf::Instruction::IsEnumVariantOrBreak {
            not_variant_block_id: Box::new(remap_block_id(
                not_variant_block_id,
                block_replacements,
            )?),
            enum_type: Box::new(substitute_token_arguments(enum_type, token_args)?),
            variant_id: variant_id.clone(),
            value: Box::new(remap_register(value, register_replacements)?),
            args: remap_register_list(args, register_replacements)?,
            field_extractors: prepare_field_extractors_for_inline(
                field_extractors,
                register_replacements,
            )?,
        }),
        vf::Instruction::LoadReference { dest, r#ref } => Some(vf::Instruction::LoadReference {
            dest: Box::new(remap_register(dest, register_replacements)?),
            r#ref: Box::new(remap_register(r#ref, register_replacements)?),
        }),
        vf::Instruction::LoadToken { dest, token } => Some(vf::Instruction::LoadToken {
            dest: Box::new(remap_register(dest, register_replacements)?),
            token: Box::new(substitute_token_arguments(token, token_args)?),
        }),
        vf::Instruction::LoadInstanceField {
            dest,
            instance_object,
            instance_type,
            parameter_index,
        } => Some(vf::Instruction::LoadInstanceField {
            dest: Box::new(remap_register(dest, register_replacements)?),
            instance_object: Box::new(remap_register(instance_object, register_replacements)?),
            instance_type: Box::new(substitute_token_arguments(instance_type, token_args)?),
            parameter_index: parameter_index.clone(),
        }),
        vf::Instruction::Move { dest, src } => Some(vf::Instruction::Move {
            dest: Box::new(remap_register(dest, register_replacements)?),
            src: Box::new(remap_register(src, register_replacements)?),
        }),
        vf::Instruction::NewInstance {
            instance_id,
            dest,
            token_args: instruction_token_args,
            args,
        } => Some(vf::Instruction::NewInstance {
            instance_id: instance_id.clone(),
            dest: Box::new(remap_register(dest, register_replacements)?),
            token_args: substitute_token_argument_list(instruction_token_args, token_args)?,
            args: remap_register_list(args, register_replacements)?,
        }),
        vf::Instruction::NewReference { dest, value } => Some(vf::Instruction::NewReference {
            dest: Box::new(remap_register(dest, register_replacements)?),
            value: Box::new(remap_register(value, register_replacements)?),
        }),
        vf::Instruction::PartiallyAppliedFunction {
            function_id,
            dest,
            token_args: instruction_token_args,
            args,
        } => Some(vf::Instruction::PartiallyAppliedFunction {
            function_id: function_id.clone(),
            dest: Box::new(remap_register(dest, register_replacements)?),
            token_args: substitute_token_argument_list(instruction_token_args, token_args)?,
            args: remap_register_list(args, register_replacements)?,
        }),
        vf::Instruction::PartiallyAppliedTokenFunction {
            function_id,
            dest,
            token_args: instruction_token_args,
            args,
        } => Some(vf::Instruction::PartiallyAppliedTokenFunction {
            function_id: function_id.clone(),
            dest: Box::new(remap_register(dest, register_replacements)?),
            token_args: substitute_token_argument_list(instruction_token_args, token_args)?,
            args: remap_register_list(args, register_replacements)?,
        }),
        vf::Instruction::PartiallyAppliedFunctionErased {
            function_id,
            dest,
            token_args: instruction_token_args,
            args,
        } => Some(vf::Instruction::PartiallyAppliedFunctionErased {
            function_id: function_id.clone(),
            dest: Box::new(remap_register(dest, register_replacements)?),
            token_args: substitute_token_argument_list(instruction_token_args, token_args)?,
            args: remap_register_list(args, register_replacements)?,
        }),
        vf::Instruction::Raise { exception } => Some(vf::Instruction::Raise {
            exception: Box::new(remap_register(exception, register_replacements)?),
        }),
        vf::Instruction::RecordFieldLoad {
            field_id,
            dest,
            record_value,
        } => Some(vf::Instruction::RecordFieldLoad {
            field_id: field_id.clone(),
            dest: Box::new(remap_register(dest, register_replacements)?),
            record_value: Box::new(remap_register(record_value, register_replacements)?),
        }),
        vf::Instruction::RecordFieldStore {
            field_id,
            record_value,
            field_value,
        } => Some(vf::Instruction::RecordFieldStore {
            field_id: field_id.clone(),
            record_value: Box::new(remap_register(record_value, register_replacements)?),
            field_value: Box::new(remap_register(field_value, register_replacements)?),
        }),
        vf::Instruction::RecordLiteral {
            dest,
            record_type,
            fields,
        } => Some(vf::Instruction::RecordLiteral {
            dest: Box::new(remap_register(dest, register_replacements)?),
            record_type: Box::new(substitute_token_arguments(record_type, token_args)?),
            fields: prepare_record_field_literals_for_inline(fields, register_replacements)?,
        }),
        vf::Instruction::Return { src } => {
            if let Some(return_register) = return_register {
                Some(vf::Instruction::Move {
                    dest: Box::new(return_register.clone()),
                    src: Box::new(remap_register(src, register_replacements)?),
                })
            } else {
                Some(vf::Instruction::Return {
                    src: Box::new(remap_register(src, register_replacements)?),
                })
            }
        }
        vf::Instruction::Tuple { dest, values } => Some(vf::Instruction::Tuple {
            dest: Box::new(remap_register(dest, register_replacements)?),
            values: remap_register_list(values, register_replacements)?,
        }),
        vf::Instruction::TupleElement {
            element_index,
            dest,
            src,
        } => Some(vf::Instruction::TupleElement {
            element_index: element_index.clone(),
            dest: Box::new(remap_register(dest, register_replacements)?),
            src: Box::new(remap_register(src, register_replacements)?),
        }),
        vf::Instruction::Unbox {
            dest,
            r#type,
            value,
        } => Some(vf::Instruction::Unbox {
            dest: Box::new(remap_register(dest, register_replacements)?),
            r#type: Box::new(substitute_token_arguments(r#type, token_args)?),
            value: Box::new(remap_register(value, register_replacements)?),
        }),
        vf::Instruction::Unreachable {} => Some(vf::Instruction::Unreachable {}),
        vf::Instruction::UpdateReference { r#ref, value } => {
            Some(vf::Instruction::UpdateReference {
                r#ref: Box::new(remap_register(r#ref, register_replacements)?),
                value: Box::new(remap_register(value, register_replacements)?),
            })
        }
    }
}

fn prepare_builtin_op_for_inline(
    op: &vf::BuiltinOp,
    token_args: &[Box<vf::Token>],
    register_replacements: &HashMap<BigUint, BigUint>,
) -> Option<vf::BuiltinOp> {
    match op {
        vf::BuiltinOp::IntNegate { dest, value } => Some(vf::BuiltinOp::IntNegate {
            dest: Box::new(remap_register(dest, register_replacements)?),
            value: Box::new(remap_register(value, register_replacements)?),
        }),
        vf::BuiltinOp::IntBitNot { dest, value } => Some(vf::BuiltinOp::IntBitNot {
            dest: Box::new(remap_register(dest, register_replacements)?),
            value: Box::new(remap_register(value, register_replacements)?),
        }),
        vf::BuiltinOp::IntAdd { dest, lhs, rhs } => Some(vf::BuiltinOp::IntAdd {
            dest: Box::new(remap_register(dest, register_replacements)?),
            lhs: Box::new(remap_register(lhs, register_replacements)?),
            rhs: Box::new(remap_register(rhs, register_replacements)?),
        }),
        vf::BuiltinOp::IntSub { dest, lhs, rhs } => Some(vf::BuiltinOp::IntSub {
            dest: Box::new(remap_register(dest, register_replacements)?),
            lhs: Box::new(remap_register(lhs, register_replacements)?),
            rhs: Box::new(remap_register(rhs, register_replacements)?),
        }),
        vf::BuiltinOp::IntMul { dest, lhs, rhs } => Some(vf::BuiltinOp::IntMul {
            dest: Box::new(remap_register(dest, register_replacements)?),
            lhs: Box::new(remap_register(lhs, register_replacements)?),
            rhs: Box::new(remap_register(rhs, register_replacements)?),
        }),
        vf::BuiltinOp::IntBitAnd { dest, lhs, rhs } => Some(vf::BuiltinOp::IntBitAnd {
            dest: Box::new(remap_register(dest, register_replacements)?),
            lhs: Box::new(remap_register(lhs, register_replacements)?),
            rhs: Box::new(remap_register(rhs, register_replacements)?),
        }),
        vf::BuiltinOp::IntBitOr { dest, lhs, rhs } => Some(vf::BuiltinOp::IntBitOr {
            dest: Box::new(remap_register(dest, register_replacements)?),
            lhs: Box::new(remap_register(lhs, register_replacements)?),
            rhs: Box::new(remap_register(rhs, register_replacements)?),
        }),
        vf::BuiltinOp::IntBitXor { dest, lhs, rhs } => Some(vf::BuiltinOp::IntBitXor {
            dest: Box::new(remap_register(dest, register_replacements)?),
            lhs: Box::new(remap_register(lhs, register_replacements)?),
            rhs: Box::new(remap_register(rhs, register_replacements)?),
        }),
        vf::BuiltinOp::IntBitShiftLeft { dest, lhs, rhs } => Some(vf::BuiltinOp::IntBitShiftLeft {
            dest: Box::new(remap_register(dest, register_replacements)?),
            lhs: Box::new(remap_register(lhs, register_replacements)?),
            rhs: Box::new(remap_register(rhs, register_replacements)?),
        }),
        vf::BuiltinOp::IntBitShiftRight { dest, lhs, rhs } => {
            Some(vf::BuiltinOp::IntBitShiftRight {
                dest: Box::new(remap_register(dest, register_replacements)?),
                lhs: Box::new(remap_register(lhs, register_replacements)?),
                rhs: Box::new(remap_register(rhs, register_replacements)?),
            })
        }
        vf::BuiltinOp::IntEq { dest, lhs, rhs } => Some(vf::BuiltinOp::IntEq {
            dest: Box::new(remap_register(dest, register_replacements)?),
            lhs: Box::new(remap_register(lhs, register_replacements)?),
            rhs: Box::new(remap_register(rhs, register_replacements)?),
        }),
        vf::BuiltinOp::IntLt { dest, lhs, rhs } => Some(vf::BuiltinOp::IntLt {
            dest: Box::new(remap_register(dest, register_replacements)?),
            lhs: Box::new(remap_register(lhs, register_replacements)?),
            rhs: Box::new(remap_register(rhs, register_replacements)?),
        }),
        vf::BuiltinOp::IntLe { dest, lhs, rhs } => Some(vf::BuiltinOp::IntLe {
            dest: Box::new(remap_register(dest, register_replacements)?),
            lhs: Box::new(remap_register(lhs, register_replacements)?),
            rhs: Box::new(remap_register(rhs, register_replacements)?),
        }),
        vf::BuiltinOp::IntGt { dest, lhs, rhs } => Some(vf::BuiltinOp::IntGt {
            dest: Box::new(remap_register(dest, register_replacements)?),
            lhs: Box::new(remap_register(lhs, register_replacements)?),
            rhs: Box::new(remap_register(rhs, register_replacements)?),
        }),
        vf::BuiltinOp::IntGe { dest, lhs, rhs } => Some(vf::BuiltinOp::IntGe {
            dest: Box::new(remap_register(dest, register_replacements)?),
            lhs: Box::new(remap_register(lhs, register_replacements)?),
            rhs: Box::new(remap_register(rhs, register_replacements)?),
        }),
        vf::BuiltinOp::StringConcat { dest, args } => Some(vf::BuiltinOp::StringConcat {
            dest: Box::new(remap_register(dest, register_replacements)?),
            args: remap_register_list(args, register_replacements)?,
        }),
        vf::BuiltinOp::StringEq { dest, lhs, rhs } => Some(vf::BuiltinOp::StringEq {
            dest: Box::new(remap_register(dest, register_replacements)?),
            lhs: Box::new(remap_register(lhs, register_replacements)?),
            rhs: Box::new(remap_register(rhs, register_replacements)?),
        }),
        vf::BuiltinOp::BoolNot { dest, value } => Some(vf::BuiltinOp::BoolNot {
            dest: Box::new(remap_register(dest, register_replacements)?),
            value: Box::new(remap_register(value, register_replacements)?),
        }),
        vf::BuiltinOp::BoolEq { dest, lhs, rhs } => Some(vf::BuiltinOp::BoolEq {
            dest: Box::new(remap_register(dest, register_replacements)?),
            lhs: Box::new(remap_register(lhs, register_replacements)?),
            rhs: Box::new(remap_register(rhs, register_replacements)?),
        }),
        vf::BuiltinOp::ArrayCreateUnsafeUninitialized {
            element_type,
            dest,
            length,
        } => Some(vf::BuiltinOp::ArrayCreateUnsafeUninitialized {
            element_type: Box::new(substitute_token_arguments(element_type, token_args)?),
            dest: Box::new(remap_register(dest, register_replacements)?),
            length: Box::new(remap_register(length, register_replacements)?),
        }),
        vf::BuiltinOp::ArrayLength {
            element_type,
            dest,
            array,
        } => Some(vf::BuiltinOp::ArrayLength {
            element_type: Box::new(substitute_token_arguments(element_type, token_args)?),
            dest: Box::new(remap_register(dest, register_replacements)?),
            array: Box::new(remap_register(array, register_replacements)?),
        }),
        vf::BuiltinOp::ArrayGet {
            element_type,
            dest,
            array,
            index,
        } => Some(vf::BuiltinOp::ArrayGet {
            element_type: Box::new(substitute_token_arguments(element_type, token_args)?),
            dest: Box::new(remap_register(dest, register_replacements)?),
            array: Box::new(remap_register(array, register_replacements)?),
            index: Box::new(remap_register(index, register_replacements)?),
        }),
        vf::BuiltinOp::ArraySet {
            element_type,
            array,
            index,
            value,
        } => Some(vf::BuiltinOp::ArraySet {
            element_type: Box::new(substitute_token_arguments(element_type, token_args)?),
            array: Box::new(remap_register(array, register_replacements)?),
            index: Box::new(remap_register(index, register_replacements)?),
            value: Box::new(remap_register(value, register_replacements)?),
        }),
    }
}

fn prepare_function_result_for_inline(
    result: &vf::FunctionResult,
    register_replacements: &HashMap<BigUint, BigUint>,
    return_register: Option<&vf::RegisterId>,
) -> Option<vf::FunctionResult> {
    match result {
        vf::FunctionResult::Register { id } => Some(vf::FunctionResult::Register {
            id: Box::new(remap_register(id, register_replacements)?),
        }),
        vf::FunctionResult::Discard {} => Some(vf::FunctionResult::Discard {}),
        vf::FunctionResult::ReturnValue {} => {
            if let Some(return_register) = return_register {
                Some(vf::FunctionResult::Register {
                    id: Box::new(return_register.clone()),
                })
            } else {
                Some(vf::FunctionResult::ReturnValue {})
            }
        }
    }
}

fn block_replacements_for_inline(
    region: &vf::Region,
    first_block_id: BigUint,
) -> (HashMap<BigUint, BigUint>, BigUint) {
    let mut original_block_ids = HashSet::new();
    collect_block_ids(region, &mut |block_id| {
        original_block_ids.insert(block_id.id.clone());
    });

    let mut original_block_ids = original_block_ids.into_iter().collect::<Vec<_>>();
    original_block_ids.sort();

    let mut next_block_id = first_block_id;
    let mut block_replacements = HashMap::new();
    for original_block_id in original_block_ids {
        block_replacements.insert(original_block_id, next_block_id.clone());
        next_block_id += 1u32;
    }

    (block_replacements, next_block_id)
}

fn collect_block_ids<'a>(region: &'a vf::Region, callback: &mut impl FnMut(&'a vf::BlockId)) {
    match region {
        vf::Region::BasicBlock { instructions } => {
            for instruction in instructions {
                collect_instruction_block_ids(instruction, callback);
            }
        }
        vf::Region::Sequence { regions } => {
            for region in regions {
                collect_block_ids(region, callback);
            }
        }
        vf::Region::Block {
            block_id, region, ..
        } => {
            callback(block_id);
            collect_block_ids(region, callback);
        }
        vf::Region::IfElse {
            when_true_block_id,
            when_false_block_id,
            condition,
            when_true,
            when_false,
        } => {
            callback(when_true_block_id);
            callback(when_false_block_id);
            collect_block_ids(condition, callback);
            collect_block_ids(when_true, callback);
            collect_block_ids(when_false, callback);
        }
        vf::Region::Finally { action, ensuring } => {
            collect_block_ids(action, callback);
            collect_block_ids(ensuring, callback);
        }
    }
}

fn collect_instruction_block_ids<'a>(
    instruction: &'a vf::Instruction,
    callback: &mut impl FnMut(&'a vf::BlockId),
) {
    match instruction {
        vf::Instruction::BlockBreak { block_id }
        | vf::Instruction::BlockBreakIf { block_id, .. }
        | vf::Instruction::BlockBreakUnless { block_id, .. }
        | vf::Instruction::BlockRetry { block_id } => callback(block_id),
        vf::Instruction::IsEnumVariantOrBreak {
            not_variant_block_id,
            ..
        } => callback(not_variant_block_id),
        _ => {}
    }
}

fn remap_register(
    register: &vf::RegisterId,
    register_replacements: &HashMap<BigUint, BigUint>,
) -> Option<vf::RegisterId> {
    Some(vf::RegisterId {
        id: register_replacements.get(&register.id)?.clone(),
    })
}

fn remap_block_id(
    block_id: &vf::BlockId,
    block_replacements: &HashMap<BigUint, BigUint>,
) -> Option<vf::BlockId> {
    Some(vf::BlockId {
        id: block_replacements.get(&block_id.id)?.clone(),
    })
}

fn remap_register_list(
    registers: &[Box<vf::RegisterId>],
    register_replacements: &HashMap<BigUint, BigUint>,
) -> Option<Vec<Box<vf::RegisterId>>> {
    registers
        .iter()
        .map(|register| remap_register(register, register_replacements).map(Box::new))
        .collect()
}

fn prepare_record_field_literals_for_inline(
    fields: &[Box<vf::RecordFieldLiteral>],
    register_replacements: &HashMap<BigUint, BigUint>,
) -> Option<Vec<Box<vf::RecordFieldLiteral>>> {
    fields
        .iter()
        .map(|field| {
            Some(Box::new(vf::RecordFieldLiteral {
                field_id: field.field_id.clone(),
                value: Box::new(remap_register(&field.value, register_replacements)?),
            }))
        })
        .collect()
}

fn prepare_field_extractors_for_inline(
    field_extractors: &[Box<vf::FieldExtractor>],
    register_replacements: &HashMap<BigUint, BigUint>,
) -> Option<Vec<Box<vf::FieldExtractor>>> {
    field_extractors
        .iter()
        .map(|field_extractor| {
            Some(Box::new(vf::FieldExtractor {
                r: Box::new(remap_register(&field_extractor.r, register_replacements)?),
                field_id: field_extractor.field_id.clone(),
            }))
        })
        .collect()
}

fn definition_methods(methods: &[Box<vf::MethodDefinition>]) -> impl Iterator<Item = BigUint> + '_ {
    methods.iter().map(|method| method.method_id.clone())
}

fn sorted_entries<V>(map: &HashMap<BigUint, V>) -> Vec<(&BigUint, &V)> {
    let mut entries = map.iter().collect::<Vec<_>>();
    entries.sort_by(|(id1, _), (id2, _)| id1.cmp(id2));
    entries
}

fn next_id<V>(map: &HashMap<BigUint, V>) -> BigUint {
    map.keys()
        .max()
        .map(|id| id + 1u32)
        .unwrap_or_else(|| BigUint::from(0u32))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn prepares_function_body_for_inline() {
        let body = vf::FunctionBody {
            variables: Box::new(vf::VariableDeclarations {
                variables: vec![Box::new(vf::VariableDeclaration {
                    r#type: Box::new(vf::Token::TokenParameter {
                        index: BigUint::from(0u32),
                    }),
                })],
            }),
            region: Box::new(vf::Region::BasicBlock {
                instructions: vec![
                    Box::new(vf::Instruction::Move {
                        dest: register(1),
                        src: register(0),
                    }),
                    Box::new(vf::Instruction::Return { src: register(1) }),
                ],
            }),
        };

        let prepared = prepare_function_body_for_inline(
            &body,
            &[vf::Token::TokenParameter {
                index: BigUint::from(0u32),
            }],
            &[Box::new(vf::Token::Boxed {})],
            BigUint::from(10u32),
            BigUint::from(30u32),
            Some(&vf::RegisterId {
                id: BigUint::from(20u32),
            }),
        )
        .expect("function body should prepare");

        assert_eq!(prepared.argument_registers[0].id, BigUint::from(10u32));
        assert_eq!(prepared.next_register, BigUint::from(12u32));
        assert_eq!(prepared.next_block_id, BigUint::from(30u32));
        assert_eq!(prepared.body.variables.variables.len(), 2);
        assert!(matches!(
            prepared.body.variables.variables[0].r#type.as_ref(),
            vf::Token::Boxed {}
        ));
        assert!(matches!(
            prepared.body.variables.variables[1].r#type.as_ref(),
            vf::Token::Boxed {}
        ));

        let vf::Region::BasicBlock { instructions } = prepared.body.region.as_ref() else {
            panic!("prepared body should preserve the basic block");
        };
        let vf::Instruction::Move { dest, src } = instructions[0].as_ref() else {
            panic!("first instruction should stay a move");
        };
        assert_eq!(dest.id, BigUint::from(11u32));
        assert_eq!(src.id, BigUint::from(10u32));

        let vf::Instruction::Move { dest, src } = instructions[1].as_ref() else {
            panic!("return should become a move");
        };
        assert_eq!(dest.id, BigUint::from(20u32));
        assert_eq!(src.id, BigUint::from(11u32));
    }

    #[test]
    fn prepares_function_body_for_inline_with_fresh_block_ids() {
        let body = vf::FunctionBody {
            variables: Box::new(vf::VariableDeclarations {
                variables: Vec::new(),
            }),
            region: Box::new(vf::Region::Block {
                block_id: block_id(0),
                flags: vf::BlockFlags {
                    has_break: true,
                    has_retry: true,
                    is_loop: true,
                },
                region: Box::new(vf::Region::BasicBlock {
                    instructions: vec![
                        Box::new(vf::Instruction::BlockRetry {
                            block_id: block_id(0),
                        }),
                        Box::new(vf::Instruction::BlockBreak {
                            block_id: block_id(0),
                        }),
                    ],
                }),
            }),
        };

        let prepared = prepare_function_body_for_inline(
            &body,
            &[],
            &[],
            BigUint::from(10u32),
            BigUint::from(30u32),
            None,
        )
        .expect("function body should prepare");

        assert_eq!(prepared.next_block_id, BigUint::from(31u32));
        let vf::Region::Block {
            block_id, region, ..
        } = prepared.body.region.as_ref()
        else {
            panic!("prepared body should preserve the block");
        };
        assert_eq!(block_id.id, BigUint::from(30u32));

        let vf::Region::BasicBlock { instructions } = region.as_ref() else {
            panic!("block body should preserve the basic block");
        };
        let vf::Instruction::BlockRetry { block_id } = instructions[0].as_ref() else {
            panic!("first instruction should stay a retry");
        };
        assert_eq!(block_id.id, BigUint::from(30u32));

        let vf::Instruction::BlockBreak { block_id } = instructions[1].as_ref() else {
            panic!("second instruction should stay a break");
        };
        assert_eq!(block_id.id, BigUint::from(30u32));
    }

    fn register(id: u32) -> Box<vf::RegisterId> {
        Box::new(vf::RegisterId {
            id: BigUint::from(id),
        })
    }

    fn block_id(id: u32) -> Box<vf::BlockId> {
        Box::new(vf::BlockId {
            id: BigUint::from(id),
        })
    }
}
