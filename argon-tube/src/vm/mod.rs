use crate::ids::TubeIdProvider;
use alloc::borrow::ToOwned;
use alloc::collections::VecDeque;
use alloc::format;
use alloc::vec;
use alloc::{boxed::Box, sync::Arc, vec::Vec};
use argon_compiler::access::AccessModifier;
use argon_compiler::erased_sig::{ErasedSignature, ErasedSignatureType, ImportSpecifier};
use argon_compiler::expr_type::get_expr_type;
use argon_compiler::scanner::{CaptureScanner, FreeVariableScanner};
use argon_compiler::vtable::VTableTarget;
use argon_compiler::{
    AccessModifierGlobal, BinaryOperatorIdentifier, Builtin, Context, DefaultExprContext,
    DefaultExprNormalizer, Enum, EnumVariant, Function, FunctionImplementation, FunctionSignature,
    Identifier, Instance, Method, MethodEntry, MethodOwner, Module, ModuleExportBinding,
    ModuleExportEntry, ModulePath, Record, RecordField, RecordFieldOwner, StaticMethod,
    StaticMethodOwner, Trait, Tube, TubeName, TypeDeclaration, UnaryOperatorIdentifier,
};
use argon_expr::{
    BlockLabel, EnumType, ErasureMode, Expr, ExprLocationExt, ExprScanner, ExpressionOwner,
    InstanceParameterVariable, IntegerType, LocatedExpr, NormalizerScanner, RecordType, TraitType,
    Variable,
};
use argon_format_vm::vm as vf;
use argon_util::{InternalCompilerError, TubeFormatError, UniqueIdentifier};
use argon_vm::analysis::BlockJumpScan;
use core::mem;
use embedded_io_async::Write;
use esexpr::{ESExprCodec, ESExprStatic};
use esexpr_binary::{ExprGeneratorAsync, GeneratorError};
use hashbrown::{HashMap, HashSet};
use num_bigint::{BigInt, BigUint};
use num_traits::ToPrimitive;

mod condition;
mod pattern;

use crate::vm::condition::ConditionEmitter;

fn encode_access_modifier_global(access: AccessModifierGlobal) -> vf::AccessModifierGlobal {
    match access {
        AccessModifierGlobal::Public => vf::AccessModifierGlobal::Public {},
        AccessModifierGlobal::Internal => vf::AccessModifierGlobal::Internal {},
        AccessModifierGlobal::ModulePrivate => vf::AccessModifierGlobal::ModulePrivate {},
    }
}

fn encode_access_modifier(access: AccessModifier) -> vf::AccessModifier {
    match access {
        AccessModifier::Public => vf::AccessModifier::Public {},
        AccessModifier::Private => vf::AccessModifier::Private {},
        AccessModifier::Protected => vf::AccessModifier::Protected {},
        AccessModifier::Internal => vf::AccessModifier::Internal {},
        AccessModifier::ProtectedOrInternal => vf::AccessModifier::ProtectedOrInternal {},
        AccessModifier::ProtectedAndInternal => vf::AccessModifier::ProtectedAndInternal {},
        AccessModifier::ModulePrivate => vf::AccessModifier::ModulePrivate {},
    }
}

pub async fn encode_vm_tube<W>(
    out: &mut W,
    context: Context,
    tube: Arc<Tube>,
    platform_id: impl Into<alloc::string::String>,
) -> Result<(), InternalCompilerError>
where
    W: Write,
    InternalCompilerError: From<W::Error>,
{
    let mut generator = esexpr_binary::ExprGenerator::new(out);
    for entry in encode_vm_tube_stream(context, tube, platform_id) {
        let entry = entry?;

        let expr = entry.encode_esexpr();

        match generator.generate(&expr).await {
            Ok(()) => {}
            Err(GeneratorError::IOError(err)) => Err(err)?,
        }
    }
    Ok(())
}

pub fn encode_vm_tube_stream(
    context: Context,
    tube: Arc<Tube>,
    platform_id: impl Into<alloc::string::String>,
) -> impl Iterator<Item = Result<vf::TubeFileEntry, InternalCompilerError>> {
    VmEncoder::new(context, tube, platform_id.into())
}

pub struct VmEncoder {
    context: Context,
    tube: Arc<Tube>,
    platform_id: alloc::string::String,

    // Used to ensure that modules are emitted in a consistent order.
    // Will be empty once the metadata is emitted.
    modules: Vec<Arc<Module>>,

    ids: TubeIdProvider,
    entry_emitters: VecDeque<EntryEmitter>,
    function_access: HashMap<usize, AccessModifierGlobal>,
    record_access: HashMap<usize, AccessModifierGlobal>,
    enum_access: HashMap<usize, AccessModifierGlobal>,
    trait_access: HashMap<usize, AccessModifierGlobal>,
    instance_access: HashMap<usize, AccessModifierGlobal>,
}

impl VmEncoder {
    pub fn new(context: Context, tube: Arc<Tube>, platform_id: alloc::string::String) -> Self {
        let mut modules = tube
            .modules()
            .iter()
            .map(|(_, module)| module.clone())
            .collect::<Vec<_>>();

        modules.sort_by(|m1, m2| m1.path().cmp(m2.path()));

        let mut encoder = Self {
            context,
            tube,
            platform_id,
            modules,
            ids: TubeIdProvider::default(),
            entry_emitters: VecDeque::new(),
            function_access: HashMap::new(),
            record_access: HashMap::new(),
            enum_access: HashMap::new(),
            trait_access: HashMap::new(),
            instance_access: HashMap::new(),
        };

        encoder.ids.tube_ids.get(encoder.tube.name().clone());
        for ref_tube in encoder.tube.referenced_tubes() {
            encoder.ids.tube_ids.get(ref_tube.clone());
        }

        for module in &encoder.modules {
            encoder
                .ids
                .module_ids
                .get((encoder.tube.name().clone(), module.path().clone()));
        }

        encoder.entry_emitters.push_back(EntryEmitter::Header);
        encoder.entry_emitters.push_back(EntryEmitter::Metadata);

        encoder
    }

    fn get_module_id(&mut self, tube: &TubeName, module: &ModulePath) -> usize {
        let (id, is_new) = self
            .ids
            .module_ids
            .get_with_new((tube.clone(), module.clone()));

        if is_new {
            self.entry_emitters
                .push_back(EntryEmitter::ModuleReference(tube.clone(), module.clone()));
        }

        id
    }

    fn get_function_id(&mut self, function: Arc<dyn Function>) -> usize {
        let (id, is_new) = self.ids.function_ids.get_with_new(function.clone());

        if is_new {
            self.entry_emitters
                .push_back(EntryEmitter::Function(function));
        }

        id
    }

    fn new_synthetic_function_id(&mut self) -> usize {
        self.ids.function_ids.claim_id()
    }

    fn get_record_id(&mut self, record: Arc<dyn Record>) -> usize {
        let (id, is_new) = self.ids.record_ids.get_with_new(record.clone());

        if is_new {
            self.entry_emitters.push_back(EntryEmitter::Record(record));
        }

        id
    }

    fn get_record_field_id(&mut self, field: Arc<dyn RecordField>) -> usize {
        let (id, is_new) = self.ids.record_field_ids.get_with_new(field.clone());

        if is_new {
            self.entry_emitters
                .push_back(EntryEmitter::RecordField(field));
        }

        id
    }

    fn get_enum_id(&mut self, enum_: Arc<dyn Enum>) -> usize {
        let (id, is_new) = self.ids.enum_ids.get_with_new(enum_.clone());

        if is_new {
            self.entry_emitters.push_back(EntryEmitter::Enum(enum_));
        }

        id
    }

    fn get_enum_variant_id(&mut self, variant: Arc<dyn EnumVariant>) -> usize {
        let (id, is_new) = self.ids.enum_variant_ids.get_with_new(variant.clone());

        if is_new {
            self.entry_emitters
                .push_back(EntryEmitter::EnumVariant(variant));
        }

        id
    }

    fn get_trait_id(&mut self, trait_: Arc<dyn Trait>) -> usize {
        let (id, is_new) = self.ids.trait_ids.get_with_new(trait_.clone());

        if is_new {
            self.entry_emitters.push_back(EntryEmitter::Trait(trait_));
        }

        id
    }

    fn get_instance_id(&mut self, instance: Arc<dyn Instance>) -> usize {
        let (id, is_new) = self.ids.instance_ids.get_with_new(instance.clone());

        if is_new {
            self.entry_emitters
                .push_back(EntryEmitter::Instance(instance));
        }

        id
    }

    fn get_method_id(&mut self, method: Arc<dyn Method>) -> usize {
        let (id, is_new) = self.ids.method_ids.get_with_new(method.clone());

        if is_new {
            self.entry_emitters.push_back(EntryEmitter::Method(method));
        }

        id
    }

    fn get_static_method_id(&mut self, method: Arc<dyn StaticMethod>) -> usize {
        let (id, is_new) = self.ids.static_method_ids.get_with_new(method.clone());
        if is_new {
            self.entry_emitters
                .push_back(EntryEmitter::StaticMethod(method));
        }
        id
    }

    fn encode_entry(
        &mut self,
        emitter: EntryEmitter,
    ) -> Result<Option<vf::TubeFileEntry>, InternalCompilerError> {
        let entry = 'entry: {
            match emitter {
                EntryEmitter::Header => vf::TubeFileEntry::Header {
                    header: Box::new(vf::TubeHeader {
                        format_version_major: BigInt::ZERO,
                        format_version_minor: BigInt::ZERO,
                    }),
                },

                EntryEmitter::Metadata => {
                    let modules = mem::take(&mut self.modules)
                        .into_iter()
                        .map(|module| self.emit_module(module).map(Box::new))
                        .collect::<Result<Vec<_>, _>>()?;

                    vf::TubeFileEntry::Metadata {
                        metadata: Box::new(vf::TubeMetadata {
                            name: Box::new(encode_tube_name(self.tube.name())),
                            referenced_tubes: self
                                .tube
                                .referenced_tubes()
                                .iter()
                                .map(|tube| {
                                    Box::new(vf::TubeReference {
                                        name: Box::new(encode_tube_name(tube)),
                                        metadata: None,
                                    })
                                })
                                .collect(),
                            platform_metadata: self
                                .tube
                                .metadata()
                                .platform
                                .get(&self.platform_id)
                                .cloned()
                                .map(ESExprStatic::new),
                            modules,
                        }),
                    }
                }

                EntryEmitter::ModuleReference(tube, module) => {
                    let module_id =
                        BigUint::from(self.ids.module_ids.get((tube.clone(), module.clone())));
                    let tube_id = BigUint::from(self.ids.tube_ids.get(tube.clone()));
                    let path = encode_module_path(&module);

                    vf::TubeFileEntry::ModuleReference {
                        module_id,
                        tube_id,
                        path: Box::new(path),
                    }
                }

                EntryEmitter::Function(function) => {
                    if function.metadata().erasure_mode == ErasureMode::Erased {
                        return Ok(None);
                    }

                    let function_id = BigUint::from(self.ids.function_ids.get(function.clone()));
                    let import_specifier = function.clone().import_specifier();
                    let import = self.encode_import_specifier(&import_specifier)?;

                    if import_specifier_tube(&import_specifier) != self.tube.name() {
                        let signature = self.emit_function_signature(
                            &ExpressionOwner::Function(function.clone()),
                            &function.clone().signature(),
                        )?;
                        break 'entry vf::TubeFileEntry::FunctionReference {
                            function_id,
                            import: Box::new(import),
                            signature: Box::new(signature.sig),
                        };
                    }

                    let mut signature = self.emit_function_signature(
                        &ExpressionOwner::Function(function.clone()),
                        &function.clone().signature(),
                    )?;
                    let implementation = function
                        .clone()
                        .implementation()
                        .map(|implementation| {
                            self.emit_function_implementation(
                                &implementation,
                                &mut signature,
                                import_specifier,
                            )
                        })
                        .transpose()?
                        .map(Box::new);
                    let access = encode_access_modifier_global(
                        *self
                            .function_access
                            .get(&function_id.to_usize().unwrap())
                            .unwrap_or(&AccessModifierGlobal::ModulePrivate),
                    );

                    vf::TubeFileEntry::FunctionDefinition {
                        definition: Box::new(vf::FunctionDefinition {
                            function_id,
                            import: Box::new(import),
                            flags: vf::FunctionFlags {
                                inline: function.metadata().is_inline,
                            },
                            access: Box::new(access),
                            signature: Box::new(signature.sig),
                            implementation,
                        }),
                    }
                }

                EntryEmitter::Record(record) => {
                    let record_id = BigUint::from(self.ids.record_ids.get(record.clone()));
                    let import_specifier = record.clone().import_specifier();
                    let import = self.encode_import_specifier(&import_specifier)?;

                    if import_specifier_tube(&import_specifier) != self.tube.name() {
                        let signature = self.emit_function_signature(
                            &ExpressionOwner::Record(record.clone()),
                            &record.clone().signature(),
                        )?;
                        break 'entry vf::TubeFileEntry::RecordReference {
                            record_id,
                            import: Box::new(import),
                            signature: Box::new(signature.sig),
                        };
                    }

                    let signature = record.clone().signature();
                    let owner = ExpressionOwner::Record(record.clone());
                    let mut builder = FunctionSignatureBuilder::new();

                    for (index, param) in signature.parameters.iter().enumerate() {
                        builder.add_parameter(
                            self,
                            Variable::Parameter(Box::new(
                                param.clone().to_parameter_var(owner.clone(), index),
                            )),
                            false,
                        )?;
                    }

                    let fields = record
                        .clone()
                        .fields()
                        .iter()
                        .map(|field| {
                            let field_id = self.get_record_field_id(field.clone());
                            let metadata = field.metadata();
                            let field_type = self.emit_field_type(field.clone())?;

                            Ok(Box::new(vf::RecordFieldDefinition {
                                field_id: BigUint::from(field_id),
                                name: Box::new(encode_identifier(&metadata.name)),
                                field_type: Box::new(field_type),
                                access: Box::new(encode_access_modifier(metadata.access)),
                                mutable: metadata.is_mutable,
                            }))
                        })
                        .collect::<Result<Vec<_>, InternalCompilerError>>()?;

                    let signature = builder.finish(self, &signature.return_type)?;
                    let methods = record.clone().methods();
                    let method_definitions = methods
                        .iter()
                        .map(|entry| {
                            self.emit_method_definition(entry.method.clone())
                                .map(Box::new)
                        })
                        .collect::<Result<Vec<_>, _>>()?;
                    let static_methods = record
                        .clone()
                        .static_methods()
                        .iter()
                        .map(|entry| {
                            self.emit_static_method_definition(entry.method.clone())
                                .map(Box::new)
                        })
                        .collect::<Result<Vec<_>, _>>()?;
                    let vtable =
                        self.emit_vtable(MethodOwner::Record(record.clone()), methods.as_ref())?;
                    let access = encode_access_modifier_global(
                        *self
                            .record_access
                            .get(&record_id.to_usize().unwrap())
                            .unwrap_or(&AccessModifierGlobal::ModulePrivate),
                    );

                    vf::TubeFileEntry::RecordDefinition {
                        definition: Box::new(vf::RecordDefinition {
                            record_id,
                            import: Box::new(import),
                            access: Box::new(access),
                            signature: Box::new(signature.sig),
                            vtable: Box::new(vtable),
                            methods: method_definitions,
                            static_methods,
                            fields,
                        }),
                    }
                }

                EntryEmitter::RecordField(record_field) => {
                    if import_specifier_tube(&record_field.owning_record().import_specifier())
                        == self.tube.name()
                    {
                        return Ok(None);
                    }

                    let record_field_id =
                        BigUint::from(self.get_record_field_id(record_field.clone()));
                    let field_type = Box::new(self.emit_field_type(record_field.clone())?);

                    match record_field.owning_record() {
                        RecordFieldOwner::Record(r) => {
                            let record_id = BigUint::from(self.get_record_id(r.clone()));

                            vf::TubeFileEntry::RecordFieldReference {
                                name: Box::new(encode_identifier(&record_field.metadata().name)),
                                record_id,
                                record_field_id,
                                field_type,
                            }
                        }
                        RecordFieldOwner::EnumVariant(variant) => {
                            let record_field_id =
                                BigUint::from(self.get_record_field_id(record_field.clone()));
                            let variant_id =
                                BigUint::from(self.get_enum_variant_id(variant.clone()));

                            vf::TubeFileEntry::EnumVariantRecordFieldReference {
                                name: Box::new(encode_identifier(&record_field.metadata().name)),
                                variant_id,
                                record_field_id,
                                field_type,
                            }
                        }
                    }
                }

                EntryEmitter::Enum(enum_) => {
                    let enum_id = BigUint::from(self.ids.enum_ids.get(enum_.clone()));
                    let import_specifier = enum_.clone().import_specifier();
                    let import = self.encode_import_specifier(&import_specifier)?;

                    let signature = self.emit_function_signature(
                        &ExpressionOwner::Enum(enum_.clone()),
                        &enum_.clone().signature(),
                    )?;

                    if import_specifier_tube(&import_specifier) != self.tube.name() {
                        break 'entry vf::TubeFileEntry::EnumReference {
                            enum_id,
                            import: Box::new(import),
                            signature: Box::new(signature.sig),
                        };
                    }

                    let signature = self.emit_function_signature(
                        &ExpressionOwner::Enum(enum_.clone()),
                        &enum_.clone().signature(),
                    )?;
                    let enum_methods = enum_.clone().methods();
                    let enum_method_definitions = enum_methods
                        .iter()
                        .map(|entry| {
                            self.emit_method_definition(entry.method.clone())
                                .map(Box::new)
                        })
                        .collect::<Result<Vec<_>, _>>()?;
                    let static_methods = enum_
                        .clone()
                        .static_methods()
                        .iter()
                        .map(|entry| {
                            self.emit_static_method_definition(entry.method.clone())
                                .map(Box::new)
                        })
                        .collect::<Result<Vec<_>, _>>()?;
                    let enum_vtable =
                        self.emit_vtable(MethodOwner::Enum(enum_.clone()), enum_methods.as_ref())?;
                    let variants = enum_
                        .clone()
                        .variants()
                        .iter()
                        .map(|variant| {
                            let variant_id = self.get_enum_variant_id(variant.clone());
                            let variant_fields = variant.clone().fields();
                            for field in variant_fields.iter() {
                                self.get_record_field_id(field.clone());
                            }

                            let variant_signature = variant.clone().signature();
                            let mut builder = FunctionSignatureBuilder::new();
                            for (index, param) in variant_signature.parameters.iter().enumerate() {
                                builder.add_parameter(
                                    self,
                                    Variable::Parameter(Box::new(param.clone().to_parameter_var(
                                        ExpressionOwner::EnumVariant(variant.clone()),
                                        index,
                                    ))),
                                    false,
                                )?;
                            }

                            let fields = variant_fields
                                .iter()
                                .map(|field| {
                                    let field_id = self.get_record_field_id(field.clone());
                                    let metadata = field.metadata();
                                    let field_type = builder
                                        .token_emitter(self)
                                        .token_expr(field.clone().field_type().as_ref())?;

                                    Ok(Box::new(vf::RecordFieldDefinition {
                                        field_id: BigUint::from(field_id),
                                        name: Box::new(encode_identifier(&metadata.name)),
                                        field_type: Box::new(field_type),
                                        access: Box::new(encode_access_modifier(metadata.access)),
                                        mutable: metadata.is_mutable,
                                    }))
                                })
                                .collect::<Result<Vec<_>, InternalCompilerError>>()?;

                            let variant_signature =
                                builder.finish(self, &variant_signature.return_type)?;
                            let methods = variant.clone().methods();
                            let method_definitions = methods
                                .iter()
                                .map(|entry| {
                                    self.emit_method_definition(entry.method.clone())
                                        .map(Box::new)
                                })
                                .collect::<Result<Vec<_>, _>>()?;
                            let vtable = self.emit_vtable(
                                MethodOwner::EnumVariant(variant.clone()),
                                methods.as_ref(),
                            )?;

                            Ok(Box::new(vf::EnumVariantDefinition {
                                variant_id: BigUint::from(variant_id),
                                name: Box::new(encode_identifier(&variant.metadata().name)),
                                signature: Box::new(variant_signature.sig),
                                vtable: Box::new(vtable),
                                methods: method_definitions,
                                fields,
                            }))
                        })
                        .collect::<Result<Vec<_>, InternalCompilerError>>()?;
                    let access = encode_access_modifier_global(
                        *self
                            .enum_access
                            .get(&enum_id.to_usize().unwrap())
                            .unwrap_or(&AccessModifierGlobal::ModulePrivate),
                    );

                    vf::TubeFileEntry::EnumDefinition {
                        definition: Box::new(vf::EnumDefinition {
                            enum_id,
                            import: Box::new(import),
                            access: Box::new(access),
                            signature: Box::new(signature.sig),
                            vtable: Box::new(enum_vtable),
                            methods: enum_method_definitions,
                            static_methods,
                            variants,
                        }),
                    }
                }

                EntryEmitter::EnumVariant(variant) => {
                    let owning_enum = variant.owning_enum();
                    if import_specifier_tube(&owning_enum.clone().import_specifier())
                        == self.tube.name()
                    {
                        return Ok(None);
                    }

                    let variant_id = BigUint::from(self.ids.enum_variant_ids.get(variant.clone()));
                    let enum_id = BigUint::from(self.get_enum_id(owning_enum));

                    let signature = self.emit_function_signature(
                        &ExpressionOwner::EnumVariant(variant.clone()),
                        &variant.clone().signature(),
                    )?;

                    vf::TubeFileEntry::EnumVariantReference {
                        variant_id,
                        enum_id,
                        name: Box::new(encode_identifier(&variant.metadata().name)),
                        signature: Box::new(signature.sig),
                    }
                }

                EntryEmitter::Method(method) => {
                    if import_specifier_tube(&method.clone().owner().import_specifier())
                        == self.tube.name()
                    {
                        return Ok(None);
                    }

                    let method_id = BigUint::from(self.ids.method_ids.get(method.clone()));
                    let name = encode_identifier(&method.metadata().name);
                    let signature = self.encode_erased_signature(
                        &argon_compiler::erased_sig::erase_signature(
                            self.context.clone(),
                            method.clone().signature().as_ref(),
                        ),
                    )?;

                    match method.clone().owner() {
                        MethodOwner::Record(record) => vf::TubeFileEntry::RecordMethodReference {
                            method_id,
                            record_id: BigUint::from(self.get_record_id(record)),
                            name: Box::new(name),
                            erased_signature: Box::new(signature),
                            signature: Box::new(self.emit_method_signature(method.clone())?.sig),
                        },
                        MethodOwner::Enum(enum_) => vf::TubeFileEntry::EnumMethodReference {
                            method_id,
                            enum_id: BigUint::from(self.get_enum_id(enum_)),
                            name: Box::new(name),
                            erased_signature: Box::new(signature),
                            signature: Box::new(self.emit_method_signature(method.clone())?.sig),
                        },
                        MethodOwner::EnumVariant(variant) => {
                            vf::TubeFileEntry::EnumVariantMethodReference {
                                method_id,
                                variant_id: BigUint::from(self.get_enum_variant_id(variant)),
                                name: Box::new(name),
                                erased_signature: Box::new(signature),
                                signature: Box::new(
                                    self.emit_method_signature(method.clone())?.sig,
                                ),
                            }
                        }
                        MethodOwner::Trait(trait_) => {
                            let trait_id = BigUint::from(self.get_trait_id(trait_));

                            vf::TubeFileEntry::TraitMethodReference {
                                method_id,
                                trait_id,
                                name: Box::new(name),
                                erased_signature: Box::new(signature),
                                signature: Box::new(
                                    self.emit_method_signature(method.clone())?.sig,
                                ),
                            }
                        }

                        MethodOwner::Instance(instance) => {
                            let instance_id = BigUint::from(self.get_instance_id(instance));

                            vf::TubeFileEntry::InstanceMethodReference {
                                method_id,
                                instance_id,
                                name: Box::new(name),
                                erased_signature: Box::new(signature),
                                signature: Box::new(
                                    self.emit_method_signature(method.clone())?.sig,
                                ),
                            }
                        }
                    }
                }

                EntryEmitter::StaticMethod(method) => {
                    if import_specifier_tube(&method.clone().owner().import_specifier())
                        == self.tube.name()
                    {
                        return Ok(None);
                    }
                    let static_method_id =
                        BigUint::from(self.ids.static_method_ids.get(method.clone()));
                    let name = Box::new(encode_identifier(&method.metadata().name));
                    let erased_signature = Box::new(self.encode_erased_signature(
                        &argon_compiler::erased_sig::erase_signature(
                            self.context.clone(),
                            method.clone().signature().as_ref(),
                        ),
                    )?);
                    let signature =
                        Box::new(self.emit_static_method_signature(method.clone())?.sig);
                    match method.clone().owner() {
                        StaticMethodOwner::Record(r) => {
                            vf::TubeFileEntry::RecordStaticMethodReference {
                                static_method_id,
                                record_id: BigUint::from(self.get_record_id(r)),
                                name,
                                erased_signature,
                                signature,
                            }
                        }
                        StaticMethodOwner::Enum(e) => {
                            vf::TubeFileEntry::EnumStaticMethodReference {
                                static_method_id,
                                enum_id: BigUint::from(self.get_enum_id(e)),
                                name,
                                erased_signature,
                                signature,
                            }
                        }
                        StaticMethodOwner::Trait(t) => {
                            vf::TubeFileEntry::TraitStaticMethodReference {
                                static_method_id,
                                trait_id: BigUint::from(self.get_trait_id(t)),
                                name,
                                erased_signature,
                                signature,
                            }
                        }
                    }
                }

                EntryEmitter::Trait(trait_) => {
                    let trait_id = BigUint::from(self.ids.trait_ids.get(trait_.clone()));
                    let import_specifier = trait_.clone().import_specifier();
                    let import = self.encode_import_specifier(&import_specifier)?;

                    if import_specifier_tube(&import_specifier) != self.tube.name() {
                        let signature = self.emit_function_signature(
                            &ExpressionOwner::Trait(trait_.clone()),
                            &trait_.clone().signature(),
                        )?;
                        break 'entry vf::TubeFileEntry::TraitReference {
                            trait_id,
                            import: Box::new(import),
                            signature: Box::new(signature.sig),
                        };
                    }

                    let signature = self.emit_function_signature(
                        &ExpressionOwner::Trait(trait_.clone()),
                        &trait_.clone().signature(),
                    )?;
                    let methods = trait_.clone().methods();
                    let method_definitions = methods
                        .iter()
                        .map(|method| {
                            self.emit_method_definition(method.method.clone())
                                .map(Box::new)
                        })
                        .collect::<Result<Vec<_>, _>>()?;
                    let static_methods = trait_
                        .clone()
                        .static_methods()
                        .iter()
                        .map(|entry| {
                            self.emit_static_method_definition(entry.method.clone())
                                .map(Box::new)
                        })
                        .collect::<Result<Vec<_>, _>>()?;
                    let vtable =
                        self.emit_vtable(MethodOwner::Trait(trait_.clone()), methods.as_ref())?;
                    let access = encode_access_modifier_global(
                        *self
                            .trait_access
                            .get(&trait_id.to_usize().unwrap())
                            .unwrap_or(&AccessModifierGlobal::ModulePrivate),
                    );

                    vf::TubeFileEntry::TraitDefinition {
                        definition: Box::new(vf::TraitDefinition {
                            trait_id,
                            import: Box::new(import),
                            access: Box::new(access),
                            signature: Box::new(signature.sig),
                            vtable: Box::new(vtable),
                            methods: method_definitions,
                            static_methods,
                        }),
                    }
                }

                EntryEmitter::Instance(instance) => {
                    let instance_id = BigUint::from(self.ids.instance_ids.get(instance.clone()));
                    let import_specifier = instance.clone().import_specifier();
                    let import = self.encode_import_specifier(&import_specifier)?;

                    if import_specifier_tube(&import_specifier) != self.tube.name() {
                        let signature = self.emit_function_signature(
                            &ExpressionOwner::Instance(instance.clone()),
                            &instance.clone().signature(),
                        )?;
                        break 'entry vf::TubeFileEntry::InstanceReference {
                            instance_id,
                            import: Box::new(import),
                            signature: Box::new(signature.sig),
                        };
                    }

                    let signature = self.emit_function_signature(
                        &ExpressionOwner::Instance(instance.clone()),
                        &instance.clone().signature(),
                    )?;
                    let methods = instance.clone().methods();
                    let method_definitions = methods
                        .iter()
                        .map(|method| {
                            self.emit_method_definition(method.method.clone())
                                .map(Box::new)
                        })
                        .collect::<Result<Vec<_>, _>>()?;
                    let vtable = self
                        .emit_vtable(MethodOwner::Instance(instance.clone()), methods.as_ref())?;
                    let access = encode_access_modifier_global(
                        *self
                            .instance_access
                            .get(&instance_id.to_usize().unwrap())
                            .unwrap_or(&AccessModifierGlobal::ModulePrivate),
                    );

                    vf::TubeFileEntry::InstanceDefinition {
                        definition: Box::new(vf::InstanceDefinition {
                            instance_id,
                            import: Box::new(import),
                            access: Box::new(access),
                            signature: Box::new(signature.sig),
                            vtable: Box::new(vtable),
                            methods: method_definitions,
                        }),
                    }
                }

                EntryEmitter::SyntheticFunction {
                    mut sig,
                    syn_id,
                    syn_import_spec,
                    body,
                } => {
                    let import_spec = self.encode_import_specifier(&syn_import_spec)?;
                    let block = self.emit_function_body(&body, &mut sig, syn_import_spec)?;

                    vf::TubeFileEntry::FunctionDefinition {
                        definition: Box::new(vf::FunctionDefinition {
                            function_id: BigUint::from(syn_id),
                            import: Box::new(import_spec),
                            flags: vf::FunctionFlags { inline: false },
                            access: Box::new(vf::AccessModifierGlobal::ModulePrivate {}),
                            signature: Box::new(sig.sig),
                            implementation: Some(Box::new(vf::FunctionImplementation::VmIr {
                                body: Box::new(block),
                            })),
                        }),
                    }
                }
            }
        };

        Ok(Some(entry))
    }

    fn emit_module(&mut self, module: Arc<Module>) -> Result<vf::Module, InternalCompilerError> {
        let mut export_groups = module
            .export_groups()
            .iter()
            .map(|(group_name, items)| (group_name.clone(), items.clone()))
            .collect::<Vec<_>>();
        export_groups.sort_by(|(name1, _), (name2, _)| name1.cmp(name2));

        for (_, exports) in export_groups {
            for exp in exports {
                self.emit_module_export(exp);
            }
        }

        Ok(vf::Module {
            path: Box::new(encode_module_path(module.path())),
        })
    }

    fn emit_module_export(&mut self, exp: ModuleExportEntry) {
        let preserve_access = !exp.is_reexport;
        let access = exp.access;
        match exp.binding {
            ModuleExportBinding::Function(function) => {
                let id = self.get_function_id(function);
                if preserve_access {
                    self.function_access.insert(id, access);
                }
            }
            ModuleExportBinding::Record(record) => {
                let id = self.get_record_id(record);
                if preserve_access {
                    self.record_access.insert(id, access);
                }
            }
            ModuleExportBinding::Enum(enum_) => {
                let id = self.get_enum_id(enum_);
                if preserve_access {
                    self.enum_access.insert(id, access);
                }
            }
            ModuleExportBinding::Trait(trait_) => {
                let id = self.get_trait_id(trait_);
                if preserve_access {
                    self.trait_access.insert(id, access);
                }
            }
            ModuleExportBinding::Instance(instance) => {
                let id = self.get_instance_id(instance);
                if preserve_access {
                    self.instance_access.insert(id, access);
                }
            }
        }
    }

    fn encode_import_specifier(
        &mut self,
        import: &ImportSpecifier,
    ) -> Result<vf::ImportSpecifier, InternalCompilerError> {
        Ok(match import {
            ImportSpecifier::Global {
                tube,
                module,
                name,
                signature,
            } => vf::ImportSpecifier::Global {
                module_id: self.get_module_id(tube, module).into(),
                name: Box::new(encode_identifier(name)),
                sig: Box::new(self.encode_erased_signature(signature)?),
            },

            ImportSpecifier::Local { parent, id } => vf::ImportSpecifier::Local {
                parent: Box::new(self.encode_import_specifier(parent)?),
                index: self.ids.local_import_ids.get(id.clone()).into(),
            },
        })
    }

    fn encode_erased_signature(
        &mut self,
        sig: &ErasedSignature,
    ) -> Result<vf::ErasedSignature, InternalCompilerError> {
        Ok(vf::ErasedSignature {
            params: sig
                .parameters
                .iter()
                .map(|param| self.encode_erased_signature_type(param).map(Box::new))
                .collect::<Result<Vec<_>, _>>()?,
            result: Box::new(self.encode_erased_signature_type(&sig.result)?),
        })
    }

    fn encode_erased_signature_type(
        &mut self,
        t: &ErasedSignatureType,
    ) -> Result<vf::ErasedSignatureType, InternalCompilerError> {
        Ok(match t {
            ErasedSignatureType::Int => vf::ErasedSignatureType::Int {},
            ErasedSignatureType::I8 => vf::ErasedSignatureType::I8 {},
            ErasedSignatureType::U8 => vf::ErasedSignatureType::U8 {},
            ErasedSignatureType::I16 => vf::ErasedSignatureType::I16 {},
            ErasedSignatureType::U16 => vf::ErasedSignatureType::U16 {},
            ErasedSignatureType::I32 => vf::ErasedSignatureType::I32 {},
            ErasedSignatureType::U32 => vf::ErasedSignatureType::U32 {},
            ErasedSignatureType::I64 => vf::ErasedSignatureType::I64 {},
            ErasedSignatureType::U64 => vf::ErasedSignatureType::U64 {},
            ErasedSignatureType::Bool => vf::ErasedSignatureType::Bool {},
            ErasedSignatureType::String => vf::ErasedSignatureType::String {},
            ErasedSignatureType::Never => vf::ErasedSignatureType::Never {},
            ErasedSignatureType::Array(element_type) => vf::ErasedSignatureType::Array {
                element_type: Box::new(self.encode_erased_signature_type(element_type)?),
            },

            ErasedSignatureType::Function(input, output) => vf::ErasedSignatureType::Function {
                input: Box::new(self.encode_erased_signature_type(input)?),
                output: Box::new(self.encode_erased_signature_type(output)?),
            },

            ErasedSignatureType::Declared(record_import, args) => vf::ErasedSignatureType::Record {
                record_import: Box::new(self.encode_import_specifier(record_import)?),
                args: args
                    .iter()
                    .map(|arg| self.encode_erased_signature_type(arg).map(Box::new))
                    .collect::<Result<Vec<_>, _>>()?,
            },

            ErasedSignatureType::Tuple(elements) => vf::ErasedSignatureType::Tuple {
                elements: elements
                    .iter()
                    .map(|element| self.encode_erased_signature_type(element).map(Box::new))
                    .collect::<Result<Vec<_>, _>>()?,
            },

            ErasedSignatureType::Erased => vf::ErasedSignatureType::Erased {},
        })
    }

    fn emit_function_signature(
        &mut self,
        owner: &ExpressionOwner<argon_compiler::DefaultExprContext>,
        sig: &FunctionSignature<argon_compiler::DefaultExprContext>,
    ) -> Result<FunctionSignatureWithMapping, InternalCompilerError> {
        let mut builder = FunctionSignatureBuilder::new();

        for (index, param) in sig.parameters.iter().enumerate() {
            builder.add_parameter(
                self,
                Variable::Parameter(Box::new(
                    param.clone().to_parameter_var(owner.clone(), index),
                )),
                false,
            )?;
        }

        builder.finish(self, &sig.return_type)
    }

    fn emit_function_implementation(
        &mut self,
        implementation: &FunctionImplementation,
        signature: &mut FunctionSignatureWithMapping,
        import_specifier: ImportSpecifier,
    ) -> Result<vf::FunctionImplementation, InternalCompilerError> {
        Ok(match implementation {
            FunctionImplementation::Expr(expr) => vf::FunctionImplementation::VmIr {
                body: Box::new(self.emit_function_body(expr, signature, import_specifier)?),
            },

            FunctionImplementation::Extern(externs) => vf::FunctionImplementation::Extern {
                r#extern: externs
                    .externs
                    .get(&self.platform_id)
                    .cloned()
                    .map(ESExprStatic::new)
                    .unwrap_or_else(|| todo!("report missing extern implementation for platform")),
            },
        })
    }

    fn emit_method_definition(
        &mut self,
        method: Arc<dyn Method>,
    ) -> Result<vf::MethodDefinition, InternalCompilerError> {
        let metadata = method.metadata();
        let sig = method.clone().signature();
        let erased_sig = self.encode_erased_signature(
            &argon_compiler::erased_sig::erase_signature(self.context.clone(), sig.as_ref()),
        )?;

        let mut signature = self.emit_method_signature(method.clone())?;
        let implementation = method
            .clone()
            .implementation()
            .map(|implementation| {
                let import_specifier = match method.clone().owner() {
                    MethodOwner::Record(record) => record.import_specifier(),
                    MethodOwner::Enum(enum_) => enum_.import_specifier(),
                    MethodOwner::EnumVariant(variant) => variant.owning_enum().import_specifier(),
                    MethodOwner::Trait(trait_) => trait_.import_specifier(),
                    MethodOwner::Instance(instance) => instance.import_specifier(),
                };

                self.emit_function_implementation(&implementation, &mut signature, import_specifier)
            })
            .transpose()?
            .map(Box::new);

        Ok(vf::MethodDefinition {
            method_id: BigUint::from(self.ids.method_ids.get(method.clone())),
            name: Box::new(encode_identifier(&metadata.name)),
            erased_signature: Box::new(erased_sig),
            flags: vf::MethodFlags {
                r#abstract: metadata.is_abstract,
                inline: metadata.is_inline,
            },
            access: Box::new(encode_access_modifier(metadata.access)),
            signature: Box::new(signature.sig),
            implementation,
        })
    }

    fn emit_static_method_definition(
        &mut self,
        method: Arc<dyn StaticMethod>,
    ) -> Result<vf::StaticMethodDefinition, InternalCompilerError> {
        let metadata = method.metadata();
        let sig = method.clone().signature();
        let erased_signature = self.encode_erased_signature(
            &argon_compiler::erased_sig::erase_signature(self.context.clone(), sig.as_ref()),
        )?;
        let mut signature = self.emit_static_method_signature(method.clone())?;
        let implementation = method
            .clone()
            .implementation()
            .map(|implementation| {
                self.emit_function_implementation(
                    &implementation,
                    &mut signature,
                    method.clone().owner().import_specifier(),
                )
            })
            .transpose()?
            .map(Box::new);
        Ok(vf::StaticMethodDefinition {
            static_method_id: BigUint::from(self.ids.static_method_ids.get(method.clone())),
            name: Box::new(encode_identifier(&metadata.name)),
            erased_signature: Box::new(erased_signature),
            access: Box::new(encode_access_modifier(metadata.access)),
            signature: Box::new(signature.sig),
            implementation,
        })
    }

    fn emit_static_method_signature(
        &mut self,
        method: Arc<dyn StaticMethod>,
    ) -> Result<FunctionSignatureWithMapping, InternalCompilerError> {
        let owner = ExpressionOwner::StaticMethod(method.clone());
        let mut builder = FunctionSignatureBuilder::new();
        let owner_decl = match method.clone().owner() {
            StaticMethodOwner::Record(r) => TypeDeclaration::Record(r),
            StaticMethodOwner::Enum(e) => TypeDeclaration::Enum(e),
            StaticMethodOwner::Trait(t) => TypeDeclaration::Trait(t),
        };
        let owner_signature = owner_decl.clone().signature();
        let owner_expr = owner_decl.into_expression_owner();
        for (index, param) in owner_signature.parameters.iter().enumerate() {
            builder.add_parameter(
                self,
                Variable::Parameter(Box::new(
                    param.clone().to_parameter_var(owner_expr.clone(), index),
                )),
                false,
            )?;
        }
        let sig = method.clone().signature();
        for (index, param) in sig.parameters.iter().enumerate() {
            builder.add_parameter(
                self,
                Variable::Parameter(Box::new(
                    param.clone().to_parameter_var(owner.clone(), index),
                )),
                false,
            )?;
        }
        builder.finish(self, &sig.return_type)
    }

    fn emit_vtable(
        &mut self,
        owner: MethodOwner,
        methods: &[MethodEntry],
    ) -> Result<vf::Vtable, InternalCompilerError> {
        let method_indexes = methods
            .iter()
            .enumerate()
            .map(|(index, method)| (method.method.clone(), index))
            .collect::<HashMap<_, _>>();

        let vtable = match owner {
            MethodOwner::Record(record) => record.vtable(),
            MethodOwner::Enum(enum_) => enum_.vtable(),
            MethodOwner::EnumVariant(variant) => variant.vtable(),
            MethodOwner::Trait(trait_) => trait_.vtable(),
            MethodOwner::Instance(instance) => instance.vtable(),
        };
        let entries = vtable
            .entries()
            .iter()
            .map(|(slot, slot_value)| {
                let slot_method = slot.method().clone();
                let slot_method_id = BigUint::from(self.get_method_id(slot_method.clone()));
                let mut builder = FunctionSignatureBuilder::new();
                builder.add_owner_parameters(
                    self,
                    TypeDeclaration::from(slot_method.clone().owner()),
                )?;
                let receiver_type = method_receiver_type(slot_method.clone());
                let slot_instance_type = builder.token_emitter(self).token_expr(&receiver_type)?;

                let target = match slot_value.target() {
                    VTableTarget::Abstract => vf::VtableTarget::Abstract {},
                    VTableTarget::Ambiguous(_) => vf::VtableTarget::Ambiguous {},
                    VTableTarget::Implementation(method) => {
                        let index = *method_indexes
                            .get(method)
                            .expect("vtable implementation is not in method list");
                        vf::VtableTarget::Implementation {
                            method_index: BigUint::from(index),
                        }
                    }
                };

                Ok(Box::new(vf::VtableEntry {
                    slot_method_id,
                    slot_instance_type: Box::new(slot_instance_type),
                    target: Box::new(target),
                }))
            })
            .collect::<Result<Vec<_>, InternalCompilerError>>()?;

        Ok(vf::Vtable { entries })
    }

    fn emit_method_signature(
        &mut self,
        method: Arc<dyn Method>,
    ) -> Result<FunctionSignatureWithMapping, InternalCompilerError> {
        let owner = ExpressionOwner::Method(method.clone());
        let mut builder = FunctionSignatureBuilder::new();

        builder.add_owner_parameters(self, TypeDeclaration::from(method.clone().owner()))?;
        let receiver_type = method_receiver_type(method.clone());
        let receiver_name = method.metadata().instance_parameter.name.clone();
        builder.add_instance_parameter(owner.clone(), receiver_type, receiver_name)?;

        let sig = method.clone().signature();

        for (index, param) in sig.parameters.iter().enumerate() {
            builder.add_parameter(
                self,
                Variable::Parameter(Box::new(
                    param.clone().to_parameter_var(owner.clone(), index),
                )),
                false,
            )?;
        }

        builder.finish(self, &sig.return_type)
    }

    fn emit_field_type(
        &mut self,
        field: Arc<dyn RecordField>,
    ) -> Result<vf::Token, InternalCompilerError> {
        let mut builder = FunctionSignatureBuilder::new();

        builder.add_owner_parameters(self, TypeDeclaration::from(field.owning_record()))?;

        builder
            .token_emitter(self)
            .token_expr(field.field_type().as_ref())
    }

    fn emit_function_body(
        &mut self,
        expr: &LocatedExpr<DefaultExprContext>,
        signature: &mut FunctionSignatureWithMapping,
        import_specifier: ImportSpecifier,
    ) -> Result<vf::FunctionBody, InternalCompilerError> {
        let var_offset =
            (if signature.has_instance_param { 1 } else { 0 }) + signature.sig.parameters.len();

        let mut emitter = ExprEmitter {
            encoder: self,
            allow_tail_call: true,
            var_offset,
            known_vars: mem::take(&mut signature.known_vars),
            declared_vars: Vec::new(),
            regions: Vec::new(),
            instructions: Vec::new(),
            parent_import_specifier: import_specifier,
            captured_vars: CaptureScanner::scan_captures(expr),
            block_labels: HashMap::new(),
            next_block_id: 0,
        };

        match emitter.expr_return(expr) {
            Ok(_) | Err(EmitStop::Branch) => {}
            Err(EmitStop::Error(err)) => return Err(err),
        }

        Ok(emitter.into_function_body())
    }
}

impl Iterator for VmEncoder {
    type Item = Result<vf::TubeFileEntry, InternalCompilerError>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let entry_emitter = self.entry_emitters.pop_front()?;
            match self.encode_entry(entry_emitter) {
                Ok(Some(entry)) => return Some(Ok(entry)),
                Ok(None) => continue,
                Err(err) => return Some(Err(err)),
            }
        }
    }
}

enum EntryEmitter {
    Header,
    Metadata,
    ModuleReference(TubeName, ModulePath),
    Function(Arc<dyn Function>),
    Record(Arc<dyn Record>),
    RecordField(Arc<dyn RecordField>),
    Enum(Arc<dyn Enum>),
    EnumVariant(Arc<dyn EnumVariant>),
    Method(Arc<dyn Method>),
    StaticMethod(Arc<dyn StaticMethod>),
    Trait(Arc<dyn Trait>),
    Instance(Arc<dyn Instance>),
    SyntheticFunction {
        sig: FunctionSignatureWithMapping,
        syn_id: usize,
        syn_import_spec: ImportSpecifier,
        body: LocatedExpr<DefaultExprContext>,
    },
}

struct FunctionSignatureWithMapping {
    sig: vf::FunctionSignature,
    arg_consumers: Vec<ArgConsumer>,
    has_instance_param: bool,
    known_vars: HashMap<Variable<DefaultExprContext>, VariableRealization>,
}

#[derive(Clone, Copy)]
enum ArgConsumer {
    Erased,
    Token,
    Arg,
}

struct FunctionSignatureBuilder {
    token_parameters: Vec<Box<vf::SignatureTokenParameter>>,
    parameters: Vec<Box<vf::SignatureParameter>>,
    arg_consumers: Vec<ArgConsumer>,

    instance_type_params: HashMap<Variable<DefaultExprContext>, VariableRealization>,
    instance_param: Option<Variable<DefaultExprContext>>,
    type_param_mapping: HashMap<Variable<DefaultExprContext>, vf::Token>,
    param_var_mapping: HashMap<Variable<DefaultExprContext>, MappedParamVar>,
}

impl FunctionSignatureBuilder {
    fn new() -> Self {
        Self {
            token_parameters: Vec::new(),
            parameters: Vec::new(),
            arg_consumers: Vec::new(),

            instance_type_params: HashMap::new(),
            instance_param: None,
            type_param_mapping: HashMap::new(),
            param_var_mapping: HashMap::new(),
        }
    }

    fn token_emitter<'b>(&mut self, encoder: &'b mut VmEncoder) -> TokenEmitter<'b> {
        let mut token_params = HashMap::new();
        token_params.extend(
            self.instance_type_params
                .iter()
                .filter_map(|(v, realization)| match realization {
                    VariableRealization::Tok(t) => Some((v.clone(), t.clone())),
                    _ => None,
                }),
        );

        token_params.extend(
            self.type_param_mapping
                .iter()
                .map(|(var, index)| (var.clone(), index.clone())),
        );

        TokenEmitter {
            encoder,
            token_params,
        }
    }

    fn add_parameter(
        &mut self,
        encoder: &mut VmEncoder,
        param: Variable<DefaultExprContext>,
        captured_ref: bool,
    ) -> Result<(), InternalCompilerError> {
        match param.erasure_mode() {
            ErasureMode::Erased => {
                self.arg_consumers.push(ArgConsumer::Erased);
            }
            ErasureMode::Token => {
                let token_kind = self.token_emitter(encoder).token_expr(param.var_type())?;
                let tp = vf::SignatureTokenParameter {
                    name: param.name().map(encode_identifier).map(Box::new),
                    kind: Box::new(token_kind),
                };

                let index = self.token_parameters.len();
                self.token_parameters.push(Box::new(tp));
                self.type_param_mapping.insert(
                    param,
                    vf::Token::TokenParameter {
                        index: BigUint::from(index),
                    },
                );
                self.arg_consumers.push(ArgConsumer::Token);
            }
            ErasureMode::Concrete => {
                let t = self.token_emitter(encoder).token_expr(param.var_type())?;
                let mut sig_param = vf::SignatureParameter {
                    name: param.name().map(encode_identifier).map(Box::new),
                    param_type: Box::new(t),
                };

                if captured_ref {
                    sig_param.param_type = Box::new(vf::Token::RefCell {
                        inner: sig_param.param_type,
                    });
                }

                let index = self.parameters.len();
                self.parameters.push(Box::new(sig_param));
                self.param_var_mapping.insert(
                    param,
                    MappedParamVar {
                        index,
                        captured_ref,
                    },
                );
                self.arg_consumers.push(ArgConsumer::Arg);
            }
        }

        Ok(())
    }

    fn add_instance_parameter(
        &mut self,
        owner: ExpressionOwner<DefaultExprContext>,
        var_type: LocatedExpr<DefaultExprContext>,
        name: Option<Identifier>,
    ) -> Result<(), InternalCompilerError> {
        let param = InstanceParameterVariable {
            owner,
            var_type,
            name,
        };

        self.instance_param = Some(Variable::InstanceParameter(Box::new(param)));
        Ok(())
    }

    fn add_owner_parameters(
        &mut self,
        encoder: &mut VmEncoder,
        owner: TypeDeclaration,
    ) -> Result<(), InternalCompilerError> {
        let signature = owner.clone().signature();
        let owner = owner.into_expression_owner();

        let mut token_params = Vec::new();
        let mut concrete_param_vars = Vec::new();

        for (index, param) in signature.parameters.iter().enumerate() {
            match param.erasure_mode {
                ErasureMode::Token => {
                    let token = vf::Token::ParentTokenParameter {
                        index: BigUint::from(self.instance_type_params.len()),
                    };

                    token_params.push(Box::new(token.clone()));

                    self.instance_type_params.insert(
                        Variable::Parameter(Box::new(
                            param.clone().to_parameter_var(owner.clone(), index),
                        )),
                        VariableRealization::Tok(token),
                    );
                }

                ErasureMode::Concrete => {
                    concrete_param_vars.push(Variable::Parameter(Box::new(
                        param.clone().to_parameter_var(owner.clone(), index),
                    )));
                }
                ErasureMode::Erased => {}
            }
        }

        if !concrete_param_vars.is_empty() {
            let instance_type = match owner {
                ExpressionOwner::Instance(i) => {
                    let id = encoder.get_instance_id(i.clone());
                    vf::Token::InstanceType {
                        instance_id: BigUint::from(id),
                        args: token_params,
                    }
                }
                _ => {
                    todo!("Report invalid concrete parameter owner")
                }
            };

            for (i, v) in concrete_param_vars.into_iter().enumerate() {
                self.instance_type_params.insert(
                    v,
                    VariableRealization::InstanceField {
                        instance_type: instance_type.clone(),
                        instance: vf::RegisterId { id: BigUint::ZERO },
                        parameter_index: i,
                    },
                );
            }
        }

        Ok(())
    }

    fn finish(
        self,
        encoder: &mut VmEncoder,
        return_type: &LocatedExpr<DefaultExprContext>,
    ) -> Result<FunctionSignatureWithMapping, InternalCompilerError> {
        let reg_offset = if self.instance_param.is_some() {
            BigUint::from(1u32)
        } else {
            BigUint::ZERO
        };

        let mut known_vars = HashMap::new();

        known_vars.extend(self.instance_type_params);

        known_vars.extend(self.instance_param.as_ref().map(|inst| {
            (
                inst.clone(),
                VariableRealization::Reg(vf::RegisterId { id: BigUint::ZERO }),
            )
        }));

        known_vars.extend(
            self.type_param_mapping
                .into_iter()
                .map(|(v, t)| (v, VariableRealization::Tok(t))),
        );

        known_vars.extend(
            self.param_var_mapping
                .into_iter()
                .map(|(param, mapped_var)| {
                    let r = vf::RegisterId {
                        id: &reg_offset + mapped_var.index,
                    };

                    let realization = if mapped_var.captured_ref {
                        VariableRealization::RegRefCell(r)
                    } else {
                        VariableRealization::Reg(r)
                    };

                    (param, realization)
                }),
        );

        let mut token_emitter = TokenEmitter {
            encoder,
            token_params: known_vars
                .iter()
                .filter_map(|(v, realization)| match realization {
                    VariableRealization::Tok(t) => Some((v.clone(), t.clone())),
                    _ => None,
                })
                .collect(),
        };

        Ok(FunctionSignatureWithMapping {
            sig: vf::FunctionSignature {
                token_parameters: self.token_parameters,
                parameters: self.parameters,
                return_type: Box::new(token_emitter.token_expr(return_type)?),
            },
            arg_consumers: self.arg_consumers,
            has_instance_param: self.instance_param.is_some(),
            known_vars,
        })
    }
}

struct MappedParamVar {
    index: usize,
    captured_ref: bool,
}

enum EmitStop {
    Error(InternalCompilerError),
    Branch,
}

impl From<InternalCompilerError> for EmitStop {
    fn from(value: InternalCompilerError) -> Self {
        EmitStop::Error(value)
    }
}

type EmitResult<A> = Result<A, EmitStop>;

trait TokenEmitterCommon {
    fn context(&self) -> &Context;

    fn get_parameter_as_token(
        &mut self,
        v: &Variable<DefaultExprContext>,
    ) -> Result<Option<vf::Token>, InternalCompilerError>;

    fn vm_encoder(&mut self) -> &mut VmEncoder;

    fn fallback_token_expr(
        &mut self,
        t: &LocatedExpr<DefaultExprContext>,
    ) -> Result<vf::Token, InternalCompilerError> {
        todo!("emit fallback token expression: {t:?}")
    }

    fn token_expr(
        &mut self,
        t: &LocatedExpr<argon_compiler::DefaultExprContext>,
    ) -> Result<vf::Token, InternalCompilerError> {
        let mut t = t.clone();
        NormalizerScanner::new(self.context().normalize_fuel(), DefaultExprNormalizer)
            .normalize(&mut t);

        match &t.value {
            Expr::Borrow { value } => self.token_expr(value),
            Expr::BorrowMut { value } => self.token_expr(value),

            Expr::BoxedType(_) => Ok(vf::Token::Boxed {}),

            Expr::Builtin(builtin) => {
                let builtin = match self.token_builtin_type(builtin)? {
                    Some(builtin) => builtin,
                    None => return self.fallback_token_expr(&t),
                };

                Ok(vf::Token::Builtin {
                    b: Box::new(builtin),
                })
            }

            Expr::ConjunctionType { lhs, rhs } => Ok(vf::Token::Builtin {
                b: Box::new(vf::BuiltinType::Conjunction {
                    lhs: Box::new(self.token_expr(lhs)?),
                    rhs: Box::new(self.token_expr(rhs)?),
                }),
            }),

            Expr::DisjunctionType { lhs, rhs } => Ok(vf::Token::Builtin {
                b: Box::new(vf::BuiltinType::Disjunction {
                    lhs: Box::new(self.token_expr(lhs)?),
                    rhs: Box::new(self.token_expr(rhs)?),
                }),
            }),

            Expr::FunctionType { a, r } => Ok(vf::Token::Function {
                input: Box::new(self.token_expr(&a.var_type)?),
                output: Box::new(self.token_expr(r)?),
            }),

            Expr::RecordType(record_type) => Ok(vf::Token::Record {
                record_id: BigUint::from(
                    self.vm_encoder().get_record_id(record_type.record.clone()),
                ),
                args: self.emit_token_arguments(
                    ExpressionOwner::Record(record_type.record.clone()),
                    &record_type.record.clone().signature(),
                    &record_type.arguments,
                )?,
            }),

            Expr::EnumType(enum_type) => Ok(vf::Token::Enum {
                enum_id: BigUint::from(self.vm_encoder().get_enum_id(enum_type.enum_.clone())),
                args: self.emit_token_arguments(
                    ExpressionOwner::Enum(enum_type.enum_.clone()),
                    &enum_type.enum_.clone().signature(),
                    &enum_type.arguments,
                )?,
            }),

            Expr::NewInstance {
                instance,
                arguments,
            } => Ok(vf::Token::InstanceValue {
                instance_id: BigUint::from(self.vm_encoder().get_instance_id(instance.clone())),
                args: self.emit_token_arguments(
                    ExpressionOwner::Instance(instance.clone()),
                    &instance.clone().signature(),
                    arguments,
                )?,
            }),

            Expr::Share { value } => self.token_expr(value),
            Expr::Shared { inner } => self.token_expr(inner),

            Expr::TraitType(trait_type) => Ok(vf::Token::Trait {
                trait_id: BigUint::from(self.vm_encoder().get_trait_id(trait_type.trait_.clone())),
                args: self.emit_token_arguments(
                    ExpressionOwner::Trait(trait_type.trait_.clone()),
                    &trait_type.trait_.clone().signature(),
                    &trait_type.arguments,
                )?,
            }),

            Expr::Tuple { items } => Ok(vf::Token::Tuple {
                elements: self.token_located_exprs(items)?,
            }),

            Expr::Type(_) | Expr::BigType(_) => Ok(vf::Token::TypeInfo {}),

            Expr::Use {
                inner,
                is_mutable: _,
            } => self.token_expr(inner),

            Expr::Variable(variable) => match variable_erasure_mode(variable) {
                ErasureMode::Token => match self.get_parameter_as_token(variable)? {
                    Some(token) => Ok(token),
                    None => self.fallback_token_expr(&t),
                },
                ErasureMode::Erased | ErasureMode::Concrete => self.fallback_token_expr(&t),
            },

            _ => self.fallback_token_expr(&t),
        }
    }

    fn token_located_exprs<'a>(
        &mut self,
        exprs: impl IntoIterator<Item = &'a LocatedExpr<argon_compiler::DefaultExprContext>>,
    ) -> Result<Vec<Box<vf::Token>>, InternalCompilerError> {
        exprs
            .into_iter()
            .map(|expr| self.token_expr(expr).map(Box::new))
            .collect()
    }

    fn token_builtin_type(
        &mut self,
        builtin: &Builtin<DefaultExprContext>,
    ) -> Result<Option<vf::BuiltinType>, InternalCompilerError> {
        Ok(Some(match builtin {
            Builtin::IntType { integer_type } => vf::BuiltinType::Int {
                integer_type: encode_vm_integer_type(*integer_type),
            },
            Builtin::BoolType => vf::BuiltinType::Bool {},
            Builtin::StringType => vf::BuiltinType::String {},
            Builtin::NeverType => vf::BuiltinType::Never {},
            Builtin::ArrayType { element_type } => vf::BuiltinType::Array {
                element_type: Box::new(self.token_expr(element_type)?),
            },
            _ => return Ok(None),
        }))
    }

    fn emit_token_arguments(
        &mut self,
        owner: ExpressionOwner<DefaultExprContext>,
        sig: &FunctionSignature<DefaultExprContext>,
        args: &[LocatedExpr<DefaultExprContext>],
    ) -> Result<Vec<Box<vf::Token>>, InternalCompilerError> {
        let sig_with_mapping = self.vm_encoder().emit_function_signature(&owner, sig)?;

        let mut tokens = Vec::new();
        for (consumer, arg) in sig_with_mapping.arg_consumers.iter().zip(args) {
            match consumer {
                ArgConsumer::Erased => {}
                ArgConsumer::Token => {
                    tokens.push(Box::new(self.token_expr(arg)?));
                }
                ArgConsumer::Arg => {
                    panic!("Emitting concrete argument in token context")
                }
            }
        }

        Ok(tokens)
    }
}

struct TokenEmitter<'a> {
    encoder: &'a mut VmEncoder,
    token_params: HashMap<Variable<argon_compiler::DefaultExprContext>, vf::Token>,
}

impl TokenEmitterCommon for TokenEmitter<'_> {
    fn context(&self) -> &Context {
        &self.encoder.context
    }

    fn get_parameter_as_token(
        &mut self,
        v: &Variable<DefaultExprContext>,
    ) -> Result<Option<vf::Token>, InternalCompilerError> {
        Ok(self.token_params.get(v).cloned())
    }

    fn vm_encoder(&mut self) -> &mut VmEncoder {
        self.encoder
    }
}

struct ExprEmitter<'a> {
    encoder: &'a mut VmEncoder,
    allow_tail_call: bool,
    var_offset: usize,
    known_vars: HashMap<Variable<DefaultExprContext>, VariableRealization>,
    declared_vars: Vec<Box<vf::VariableDeclaration>>,
    regions: Vec<Box<vf::Region>>,
    instructions: Vec<Box<vf::Instruction>>,
    parent_import_specifier: ImportSpecifier,
    captured_vars: HashSet<Variable<DefaultExprContext>>,
    block_labels: HashMap<BlockLabel<DefaultExprContext>, (vf::BlockId, ExprOutputKnown)>,
    next_block_id: usize,
}

impl TokenEmitterCommon for ExprEmitter<'_> {
    fn context(&self) -> &Context {
        &self.encoder.context
    }

    fn get_parameter_as_token(
        &mut self,
        v: &Variable<DefaultExprContext>,
    ) -> Result<Option<vf::Token>, InternalCompilerError> {
        Ok(self.known_vars.get(v).and_then(|real| match real {
            VariableRealization::Tok(tk) => Some(tk.clone()),
            _ => None,
        }))
    }

    fn vm_encoder(&mut self) -> &mut VmEncoder {
        self.encoder
    }
}

impl<'a> ExprEmitter<'a> {
    fn flush_instructions(&mut self) {
        if !self.instructions.is_empty() {
            self.regions.push(Box::new(vf::Region::BasicBlock {
                instructions: mem::take(&mut self.instructions),
            }));
        }
    }

    fn with_nested_block<A>(
        &mut self,
        f: impl FnOnce(&mut Self) -> EmitResult<A>,
    ) -> Result<(Box<vf::Region>, EmitResult<A>), InternalCompilerError> {
        self.flush_instructions();

        let mut regions = mem::take(&mut self.regions);
        let result = match f(self) {
            Ok(result) => Ok(result),
            Err(EmitStop::Error(err)) => return Err(err),
            Err(EmitStop::Branch) => Err(EmitStop::Branch),
        };

        self.flush_instructions();

        mem::swap(&mut self.regions, &mut regions);
        let region = if regions.len() == 1
            && let Some(region) = regions.pop()
        {
            region
        } else {
            Box::new(vf::Region::Sequence { regions })
        };

        Ok((region, result))
    }

    fn prohibit_tail_call<A>(
        &mut self,
        f: impl FnOnce(&mut Self) -> EmitResult<A>,
    ) -> EmitResult<A> {
        let allow_tail_call = self.allow_tail_call;
        self.allow_tail_call = false;
        let result = f(self);
        self.allow_tail_call = allow_tail_call;
        result
    }

    fn declare_var(
        &mut self,
        v: Variable<DefaultExprContext>,
    ) -> Result<vf::RegisterId, InternalCompilerError> {
        let t = self.token_expr(v.var_type())?;

        if self.captured_vars.contains(&v) && v.is_mutable() {
            let id = self.add_var(vf::Token::RefCell { inner: Box::new(t) });
            self.known_vars
                .insert(v, VariableRealization::RegRefCell(id.clone()));
            Ok(id)
        } else {
            let id = self.add_var(t);
            self.known_vars
                .insert(v, VariableRealization::Reg(id.clone()));
            Ok(id)
        }
    }

    fn add_var(&mut self, t: vf::Token) -> vf::RegisterId {
        let id = BigUint::from(self.var_offset) + self.declared_vars.len();
        self.declared_vars.push(Box::new(vf::VariableDeclaration {
            r#type: Box::new(t),
        }));
        vf::RegisterId { id }
    }

    fn declare_block(
        &mut self,
        label: BlockLabel<DefaultExprContext>,
        output: ExprOutputKnown,
    ) -> Result<vf::BlockId, InternalCompilerError> {
        let id = self.claim_block_id();
        self.block_labels.insert(label, (id.clone(), output));
        Ok(id)
    }

    fn claim_block_id(&mut self) -> vf::BlockId {
        let n = self.next_block_id;
        self.next_block_id += 1;
        vf::BlockId {
            id: BigUint::from(n),
        }
    }

    fn get_block_id(
        &mut self,
        label: &BlockLabel<DefaultExprContext>,
    ) -> Result<(&vf::BlockId, &ExprOutputKnown), InternalCompilerError> {
        self.block_labels
            .get(label)
            .map(|(id, output)| (id, output))
            .ok_or_else(|| {
                InternalCompilerError::TubeFormatError(TubeFormatError::InvalidTube(format!(
                    "Block label not found: {:?}",
                    label
                )))
            })
    }

    fn into_function_body(mut self) -> vf::FunctionBody {
        self.flush_instructions();

        let region = if self.regions.len() == 1
            && let Some(region) = self.regions.pop()
        {
            region
        } else {
            Box::new(vf::Region::Sequence {
                regions: self.regions,
            })
        };

        vf::FunctionBody {
            variables: Box::new(vf::VariableDeclarations {
                variables: self.declared_vars,
            }),
            region,
        }
    }

    fn emit(&mut self, insn: vf::Instruction) {
        let is_branch = is_branch_instruction(&insn);
        self.instructions.push(Box::new(insn));

        if is_branch {
            self.flush_instructions();
        }
    }

    fn emit_region(&mut self, region: Box<vf::Region>) {
        self.flush_instructions();

        match *region {
            vf::Region::Sequence { regions } => {
                self.regions.extend(regions);
            }
            _ => {
                self.regions.push(region);
            }
        }
    }

    fn expr<O: ExprOutput>(
        &mut self,
        e: &LocatedExpr<DefaultExprContext>,
        output: O,
    ) -> EmitResult<O::ResultType> {
        Ok(match &e.value {
            Expr::And(_, _) | Expr::Or(_, _) | Expr::Not(_) | Expr::Is { .. } => {
                let rb = output.output_register(self, e)?;

                let when_true_label = self.claim_block_id();
                let when_false_label = self.claim_block_id();

                let (block, _) = self.with_nested_block(|emitter| {
                    let mut cond_emitter = ConditionEmitter {
                        expr_emitter: emitter,
                        when_true_label: &when_true_label,
                        when_false_label: &when_false_label,
                    };

                    cond_emitter.emit_condition(e)
                })?;

                self.emit_region(Box::new(vf::Region::IfElse {
                    condition: block,
                    when_true_block_id: Box::new(when_true_label),
                    when_false_block_id: Box::new(when_false_label),
                    when_true: Box::new(vf::Region::BasicBlock {
                        instructions: vec![Box::new(vf::Instruction::ConstBool {
                            dest: Box::new(rb.register().clone()),
                            value: true,
                        })],
                    }),
                    when_false: Box::new(vf::Region::BasicBlock {
                        instructions: vec![Box::new(vf::Instruction::ConstBool {
                            dest: Box::new(rb.register().clone()),
                            value: false,
                        })],
                    }),
                }));

                rb.into_result(self)?
            }

            Expr::BoolLiteral(b) => {
                let rb = output.output_register(self, e)?;
                self.emit(vf::Instruction::ConstBool {
                    dest: Box::new(rb.register().clone()),
                    value: *b,
                });

                rb.into_result(self)?
            }

            Expr::Closure {
                v,
                return_type,
                body,
            } => {
                let rb = output.output_register(self, e)?;

                let func_id = UniqueIdentifier::new();
                let syn_import_spec = ImportSpecifier::Local {
                    parent: Box::new(self.parent_import_specifier.clone()),
                    id: func_id,
                };

                let syn_id = self.encoder.new_synthetic_function_id();

                let mut free_vars = FreeVariableScanner::new();
                free_vars.scan(body);
                let mut free_vars = free_vars.into_free_variables();
                free_vars.remove(&Variable::ClosureParameter(v.clone()));

                let (token_args, args, sig) = self.capture_variables(
                    &free_vars,
                    |emitter, sb| {
                        sb.add_parameter(
                            emitter.encoder,
                            Variable::ClosureParameter(v.clone()),
                            false,
                        )
                    },
                    |emitter, sb| sb.finish(emitter.encoder, return_type),
                )?;

                self.encoder
                    .entry_emitters
                    .push_back(EntryEmitter::SyntheticFunction {
                        sig,
                        syn_id,
                        syn_import_spec,
                        body: body.as_ref().clone(),
                    });

                match v.erasure_mode {
                    ErasureMode::Erased => {
                        self.emit(vf::Instruction::PartiallyAppliedFunctionErased {
                            function_id: BigUint::from(syn_id),
                            dest: Box::new(rb.register().clone()),
                            token_args,
                            args,
                        });
                    }
                    ErasureMode::Token => {
                        self.emit(vf::Instruction::PartiallyAppliedTokenFunction {
                            function_id: BigUint::from(syn_id),
                            dest: Box::new(rb.register().clone()),
                            token_args,
                            args,
                        });
                    }
                    ErasureMode::Concrete => {
                        self.emit(vf::Instruction::PartiallyAppliedFunction {
                            function_id: BigUint::from(syn_id),
                            dest: Box::new(rb.register().clone()),
                            token_args,
                            args,
                        });
                    }
                }

                rb.into_result(self)?
            }

            Expr::Condition { value, .. } => self.expr(value, output)?,

            Expr::Borrow { value } => self.expr(value, output)?,
            Expr::BorrowMut { value } => self.expr(value, output)?,

            Expr::Box { t: _, value } => {
                let rb = output.output_register(self, e)?;
                let unboxed_value = self.expr(value, AnyRegister)?;

                self.emit(vf::Instruction::Box {
                    dest: Box::new(rb.register().clone()),
                    value: Box::new(unboxed_value),
                });

                rb.into_result(self)?
            }

            Expr::Block { label, body } => {
                let (output_result, output) = output.into_known_location(self, e)?;

                let block_id = self.declare_block((**label).clone(), output)?;

                let (body, body_result) =
                    self.with_nested_block(|emitter| emitter.expr(body, ExprOutputKnown::Discard))?;

                match body_result {
                    Ok(_) => Err(InternalCompilerError::TubeFormatError(
                        TubeFormatError::InvalidTube(
                            "Block body must exit with a branch".to_owned(),
                        ),
                    ))?,
                    Err(EmitStop::Branch) => {}
                    Err(EmitStop::Error(err)) => Err(err)?,
                }

                self.emit_block(block_id, body);

                output_result
            }

            Expr::Break { label, value } => {
                let (block_id, break_output) = self.get_block_id(label)?;
                let block_id = block_id.clone();
                let break_output = break_output.clone();

                self.expr(value, break_output)?;

                self.emit(vf::Instruction::BlockBreak {
                    block_id: Box::new(block_id),
                });

                Err(EmitStop::Branch)?
            }

            Expr::ConjunctionType { .. } | Expr::DisjunctionType { .. } => {
                let t = self.token_expr(e)?;
                let rb = output.output_register(self, e)?;
                self.emit(vf::Instruction::LoadToken {
                    dest: Box::new(rb.register().clone()),
                    token: Box::new(t),
                });
                rb.into_result(self)?
            }

            Expr::Builtin(builtin) => {
                fn emit_value_op<O: ExprOutput>(
                    emitter: &mut ExprEmitter<'_>,
                    e: &LocatedExpr<DefaultExprContext>,
                    output: O,
                    op: impl FnOnce(vf::RegisterId) -> vf::BuiltinOp,
                ) -> EmitResult<O::ResultType> {
                    let rb = output.output_register(emitter, e)?;
                    let dest = rb.register().clone();
                    emitter.emit(vf::Instruction::Builtin {
                        op: Box::new(op(dest)),
                    });
                    rb.into_result(emitter)
                }

                fn emit_void_op<O: ExprOutput>(
                    emitter: &mut ExprEmitter<'_>,
                    output: O,
                    op: vf::BuiltinOp,
                ) -> EmitResult<O::ResultType> {
                    emitter.emit(vf::Instruction::Builtin { op: Box::new(op) });
                    output.output_unit_result(emitter)
                }

                macro_rules! integer_unary_op {
                    ($variant:ident, $integer_type:expr, $value:expr) => {{
                        let value = self.expr($value, AnyRegister)?;
                        emit_value_op(self, e, output, |dest| vf::BuiltinOp::$variant {
                            integer_type: encode_vm_integer_type(*$integer_type),
                            dest: Box::new(dest),
                            value: Box::new(value),
                        })?
                    }};
                }

                macro_rules! integer_binary_op {
                    ($variant:ident, $integer_type:expr, $lhs:expr, $rhs:expr) => {{
                        let lhs = self.expr($lhs, AnyRegister)?;
                        let rhs = self.expr($rhs, AnyRegister)?;
                        emit_value_op(self, e, output, |dest| vf::BuiltinOp::$variant {
                            integer_type: encode_vm_integer_type(*$integer_type),
                            dest: Box::new(dest),
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        })?
                    }};
                }

                macro_rules! binary_op {
                    ($variant:ident, $lhs:expr, $rhs:expr) => {{
                        let lhs = self.expr($lhs, AnyRegister)?;
                        let rhs = self.expr($rhs, AnyRegister)?;
                        emit_value_op(self, e, output, |dest| vf::BuiltinOp::$variant {
                            dest: Box::new(dest),
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        })?
                    }};
                }

                match builtin {
                    Builtin::IntType { .. }
                    | Builtin::BoolType
                    | Builtin::StringType
                    | Builtin::NeverType
                    | Builtin::ArrayType { .. } => {
                        let t = self.token_expr(e)?;
                        let rb = output.output_register(self, e)?;
                        self.emit(vf::Instruction::LoadToken {
                            dest: Box::new(rb.register().clone()),
                            token: Box::new(t),
                        });
                        rb.into_result(self)?
                    }

                    Builtin::IntNegate {
                        integer_type,
                        value,
                    } => integer_unary_op!(IntNegate, integer_type, value),
                    Builtin::IntBitNot {
                        integer_type,
                        value,
                    } => integer_unary_op!(IntBitNot, integer_type, value),
                    Builtin::IntConvert {
                        source_type,
                        dest_type,
                        value,
                    } => {
                        let value = self.expr(value, AnyRegister)?;
                        emit_value_op(self, e, output, |dest| vf::BuiltinOp::IntConvert {
                            source_type: encode_vm_integer_type(*source_type),
                            dest_type: encode_vm_integer_type(*dest_type),
                            dest: Box::new(dest),
                            value: Box::new(value),
                        })?
                    }
                    Builtin::IntAdd {
                        integer_type,
                        lhs,
                        rhs,
                    } => integer_binary_op!(IntAdd, integer_type, lhs, rhs),
                    Builtin::IntSub {
                        integer_type,
                        lhs,
                        rhs,
                    } => integer_binary_op!(IntSub, integer_type, lhs, rhs),
                    Builtin::IntMul {
                        integer_type,
                        lhs,
                        rhs,
                    } => integer_binary_op!(IntMul, integer_type, lhs, rhs),
                    Builtin::IntBitAnd {
                        integer_type,
                        lhs,
                        rhs,
                    } => integer_binary_op!(IntBitAnd, integer_type, lhs, rhs),
                    Builtin::IntBitOr {
                        integer_type,
                        lhs,
                        rhs,
                    } => integer_binary_op!(IntBitOr, integer_type, lhs, rhs),
                    Builtin::IntBitXor {
                        integer_type,
                        lhs,
                        rhs,
                    } => integer_binary_op!(IntBitXor, integer_type, lhs, rhs),
                    Builtin::IntBitShiftLeft {
                        integer_type,
                        lhs,
                        rhs,
                    } => {
                        integer_binary_op!(IntBitShiftLeft, integer_type, lhs, rhs)
                    }
                    Builtin::IntBitShiftRight {
                        integer_type,
                        lhs,
                        rhs,
                    } => {
                        integer_binary_op!(IntBitShiftRight, integer_type, lhs, rhs)
                    }
                    Builtin::IntEq {
                        integer_type,
                        lhs,
                        rhs,
                    } => integer_binary_op!(IntEq, integer_type, lhs, rhs),
                    Builtin::IntLt {
                        integer_type,
                        lhs,
                        rhs,
                    } => integer_binary_op!(IntLt, integer_type, lhs, rhs),
                    Builtin::IntLe {
                        integer_type,
                        lhs,
                        rhs,
                    } => integer_binary_op!(IntLe, integer_type, lhs, rhs),
                    Builtin::IntGt {
                        integer_type,
                        lhs,
                        rhs,
                    } => integer_binary_op!(IntGt, integer_type, lhs, rhs),
                    Builtin::IntGe {
                        integer_type,
                        lhs,
                        rhs,
                    } => integer_binary_op!(IntGe, integer_type, lhs, rhs),
                    Builtin::StringConcat { values } => match values.as_slice() {
                        [] => self.expr(
                            &Expr::StringLiteral(Box::from("")).with_location(e.location.clone()),
                            output,
                        )?,
                        [value] => self.expr(value, output)?,
                        values => {
                            let rb = output.output_register(self, e)?;
                            let args = values
                                .iter()
                                .map(|value| self.expr(value, AnyRegister).map(Box::new))
                                .collect::<EmitResult<Vec<_>>>()?;

                            self.emit(vf::Instruction::Builtin {
                                op: Box::new(vf::BuiltinOp::StringConcat {
                                    dest: Box::new(rb.register().clone()),
                                    args,
                                }),
                            });

                            rb.into_result(self)?
                        }
                    },
                    Builtin::StringEq { lhs, rhs } => binary_op!(StringEq, lhs, rhs),
                    Builtin::BoolEq { lhs, rhs } => binary_op!(BoolEq, lhs, rhs),
                    Builtin::ArrayCreateUnsafeUninitialized {
                        element_type,
                        length,
                    } => {
                        let element_type = self.token_expr(element_type)?;
                        let length = self.expr(length, AnyRegister)?;
                        emit_value_op(self, e, output, |dest| {
                            vf::BuiltinOp::ArrayCreateUnsafeUninitialized {
                                element_type: Box::new(element_type),
                                dest: Box::new(dest),
                                length: Box::new(length),
                            }
                        })?
                    }
                    Builtin::ArrayLength {
                        element_type,
                        array,
                    } => {
                        let element_type = self.token_expr(element_type)?;
                        let array = self.expr(array, AnyRegister)?;
                        emit_value_op(self, e, output, |dest| vf::BuiltinOp::ArrayLength {
                            element_type: Box::new(element_type),
                            dest: Box::new(dest),
                            array: Box::new(array),
                        })?
                    }
                    Builtin::ArrayGet {
                        element_type,
                        array,
                        index,
                    } => {
                        let element_type = self.token_expr(element_type)?;
                        let array = self.expr(array, AnyRegister)?;
                        let index = self.expr(index, AnyRegister)?;
                        emit_value_op(self, e, output, |dest| vf::BuiltinOp::ArrayGet {
                            element_type: Box::new(element_type),
                            dest: Box::new(dest),
                            array: Box::new(array),
                            index: Box::new(index),
                        })?
                    }
                    Builtin::ArraySet {
                        element_type,
                        array,
                        index,
                        value,
                    } => {
                        let element_type = self.token_expr(element_type)?;
                        let array = self.expr(array, AnyRegister)?;
                        let index = self.expr(index, AnyRegister)?;
                        let value = self.expr(value, AnyRegister)?;
                        emit_void_op(
                            self,
                            output,
                            vf::BuiltinOp::ArraySet {
                                element_type: Box::new(element_type),
                                array: Box::new(array),
                                index: Box::new(index),
                                value: Box::new(value),
                            },
                        )?
                    }
                    Builtin::EqualToRefl { .. } | Builtin::UnsafeAssumeErased { .. } => Err(
                        InternalCompilerError::TubeFormatError(TubeFormatError::InvalidTube(
                            "Erased builtins must not be emitted to VM IR".to_owned(),
                        )),
                    )?,
                }
            }

            Expr::EnumVariantLiteral {
                enum_type,
                variant,
                arguments,
                fields,
            } => {
                let rb = output.output_register(self, e)?;

                let mut field_values = HashMap::with_capacity(fields.len());
                for field in fields {
                    let value = self.expr(&field.value, AnyRegister)?;
                    field_values.insert(field.field.clone(), value);
                }

                let mut vm_fields = Vec::with_capacity(fields.len());
                for field in variant.clone().fields().iter() {
                    let field_id = BigUint::from(self.encoder.get_record_field_id(field.clone()));
                    let value = field_values
                        .remove(field)
                        .expect("Enum variant literal is missing a field after type checking");
                    vm_fields.push(Box::new(vf::RecordFieldLiteral {
                        field_id,
                        value: Box::new(value),
                    }));
                }

                let enum_type_token = self.token_expr(
                    &Expr::EnumType(enum_type.clone()).with_location(e.location.clone()),
                )?;
                let args = self.emit_arguments(
                    ExpressionOwner::EnumVariant(variant.clone()),
                    &variant.clone().signature(),
                    arguments,
                )?;
                let variant_id = BigUint::from(self.encoder.get_enum_variant_id(variant.clone()));

                self.emit(vf::Instruction::EnumVariantLiteral {
                    dest: Box::new(rb.register().clone()),
                    enum_type: Box::new(enum_type_token),
                    variant_id,
                    token_args: args.token_arguments,
                    args: args.arguments,
                    fields: vm_fields,
                });

                rb.into_result(self)?
            }

            Expr::Finally {
                block_body,
                finally_body,
            } => {
                let (block_body, block_result) = self.with_nested_block(|emitter| {
                    emitter.prohibit_tail_call(|emitter| emitter.expr(block_body, output))
                })?;

                let (finally_body, finally_result) = self.with_nested_block(|emitter| {
                    emitter.prohibit_tail_call(|emitter| {
                        emitter.expr(finally_body, ExprOutputKnown::Discard)
                    })
                })?;

                self.emit_region(Box::new(vf::Region::Finally {
                    action: block_body,
                    ensuring: finally_body,
                }));

                let value = block_result?;
                finally_result?;
                value
            }

            Expr::FunctionCall {
                function,
                arguments,
            } => {
                let frb = output.output_function_result(self, e)?;

                let id = self.encoder.get_function_id(function.clone());

                let sig = function.clone().signature();
                let func_args = self.emit_arguments(
                    ExpressionOwner::Function(function.clone()),
                    &sig,
                    arguments,
                )?;
                self.emit(vf::Instruction::FunctionCall {
                    function_id: BigUint::from(id),
                    dest: Box::new(frb.function_result()),
                    token_args: func_args.token_arguments,
                    args: func_args.arguments,
                });

                frb.into_result(self)?
            }

            Expr::FunctionObjectCall { function, argument } => {
                let frb = output.output_function_result(self, e)?;

                let Expr::FunctionType { a, .. } = get_expr_type(function).value else {
                    todo!("return a proper error")
                };

                let func = self.expr(function, AnyRegister)?;

                match a.erasure_mode {
                    ErasureMode::Erased => {
                        self.emit(vf::Instruction::FunctionObjectErasedCall {
                            dest: Box::new(frb.function_result()),
                            function: Box::new(func),
                        });
                    }
                    ErasureMode::Token => {
                        let arg = self.token_expr(argument)?;
                        self.emit(vf::Instruction::FunctionObjectTokenCall {
                            dest: Box::new(frb.function_result()),
                            function: Box::new(func),
                            arg: Box::new(arg),
                        });
                    }
                    ErasureMode::Concrete => {
                        let arg = self.expr(argument, AnyRegister)?;
                        self.emit(vf::Instruction::FunctionObjectCall {
                            dest: Box::new(frb.function_result()),
                            function: Box::new(func),
                            arg: Box::new(arg),
                        });
                    }
                }

                frb.into_result(self)?
            }

            Expr::IfElse {
                condition,
                when_true,
                when_false,
                ..
            } => {
                let (result, output) = output.into_known_location(self, e)?;

                let when_true_label = self.claim_block_id();
                let when_false_label = self.claim_block_id();

                let (cond, _) = self.with_nested_block(|emitter| {
                    let mut cond_emitter = ConditionEmitter {
                        expr_emitter: emitter,
                        when_true_label: &when_true_label,
                        when_false_label: &when_false_label,
                    };

                    cond_emitter.emit_condition(condition)
                })?;

                let (when_true, true_result) =
                    self.with_nested_block(|emitter| emitter.expr(when_true, output.clone()))?;

                let (when_false, false_result) =
                    self.with_nested_block(|emitter| emitter.expr(when_false, output))?;

                self.emit_region(Box::new(vf::Region::IfElse {
                    condition: cond,
                    when_true_block_id: Box::new(when_true_label),
                    when_false_block_id: Box::new(when_false_label),
                    when_true,
                    when_false,
                }));

                match (true_result, false_result) {
                    (Err(EmitStop::Branch), Err(EmitStop::Branch)) => Err(EmitStop::Branch)?,
                    _ => result,
                }
            }

            Expr::IntLiteral(i) => {
                let rb = output.output_register(self, e)?;
                self.emit(vf::Instruction::ConstInt {
                    dest: Box::new(rb.register().clone()),
                    value: i.clone(),
                });

                rb.into_result(self)?
            }

            Expr::I8Literal(value) => {
                let rb = output.output_register(self, e)?;
                self.emit(vf::Instruction::ConstI8 {
                    dest: Box::new(rb.register().clone()),
                    value: *value,
                });

                rb.into_result(self)?
            }

            Expr::U8Literal(value) => {
                let rb = output.output_register(self, e)?;
                self.emit(vf::Instruction::ConstU8 {
                    dest: Box::new(rb.register().clone()),
                    value: *value,
                });

                rb.into_result(self)?
            }

            Expr::I16Literal(value) => {
                let rb = output.output_register(self, e)?;
                self.emit(vf::Instruction::ConstI16 {
                    dest: Box::new(rb.register().clone()),
                    value: *value,
                });

                rb.into_result(self)?
            }

            Expr::U16Literal(value) => {
                let rb = output.output_register(self, e)?;
                self.emit(vf::Instruction::ConstU16 {
                    dest: Box::new(rb.register().clone()),
                    value: *value,
                });

                rb.into_result(self)?
            }

            Expr::I32Literal(value) => {
                let rb = output.output_register(self, e)?;
                self.emit(vf::Instruction::ConstI32 {
                    dest: Box::new(rb.register().clone()),
                    value: *value,
                });

                rb.into_result(self)?
            }

            Expr::U32Literal(value) => {
                let rb = output.output_register(self, e)?;
                self.emit(vf::Instruction::ConstU32 {
                    dest: Box::new(rb.register().clone()),
                    value: *value,
                });

                rb.into_result(self)?
            }

            Expr::I64Literal(value) => {
                let rb = output.output_register(self, e)?;
                self.emit(vf::Instruction::ConstI64 {
                    dest: Box::new(rb.register().clone()),
                    value: *value,
                });

                rb.into_result(self)?
            }

            Expr::U64Literal(value) => {
                let rb = output.output_register(self, e)?;
                self.emit(vf::Instruction::ConstU64 {
                    dest: Box::new(rb.register().clone()),
                    value: *value,
                });

                rb.into_result(self)?
            }

            Expr::Match { value, cases } => {
                let (result, output) = output.into_known_location(self, e)?;
                let value_reg = self.expr(value, AnyRegister)?;
                let match_block = self.claim_block_id();

                let (block, _) = self.with_nested_block(|emitter| {
                    for case in cases {
                        let when_true_label = emitter.claim_block_id();
                        let when_false_label = emitter.claim_block_id();
                        let value_reg = value_reg.clone();
                        let output = output.clone();
                        let match_block = match_block.clone();

                        let (condition, _) = emitter.with_nested_block(|emitter| {
                            pattern::emit_pattern(
                                emitter,
                                &when_false_label,
                                value_reg,
                                &case.pattern,
                            )
                        })?;

                        let (when_true, _) = emitter.with_nested_block(|emitter| {
                            emitter.expr(&case.body, output)?;
                            emitter.emit(vf::Instruction::BlockBreak {
                                block_id: Box::new(match_block),
                            });
                            Err::<(), _>(EmitStop::Branch)
                        })?;

                        emitter.emit_region(Box::new(vf::Region::IfElse {
                            condition,
                            when_true_block_id: Box::new(when_true_label),
                            when_false_block_id: Box::new(when_false_label),
                            when_true,
                            when_false: Box::new(vf::Region::Sequence { regions: vec![] }),
                        }));
                    }

                    emitter.emit(vf::Instruction::Unreachable {});
                    Ok(())
                })?;

                self.emit_block(match_block, block);

                result
            }

            Expr::MethodCall {
                method,
                instance_type,
                receiver,
                arguments,
            } => {
                let frb = output.output_function_result(self, e)?;

                let instance_type_expr = instance_type
                    .clone()
                    .into_expr()
                    .with_location(e.location.clone());
                let instance_type_token = self.token_expr(&instance_type_expr)?;

                let receiver_reg = self.expr(receiver, AnyRegister)?;

                let method_id = self.encoder.get_method_id(method.clone());
                let mut sig = method.clone().signature().as_ref().clone();
                sig.substitute_method_instance_type_parameters(instance_type);

                let args =
                    self.emit_arguments(ExpressionOwner::Method(method.clone()), &sig, arguments)?;

                self.emit(vf::Instruction::InstanceMethodCall {
                    dest: Box::new(frb.function_result()),

                    method_id: BigUint::from(method_id),
                    instance_type: Box::new(instance_type_token),

                    instance_object: Box::new(receiver_reg),
                    token_args: args.token_arguments,
                    args: args.arguments,
                });

                frb.into_result(self)?
            }

            Expr::StaticMethodCall {
                method,
                owner_type,
                arguments,
            } => {
                let frb = output.output_function_result(self, e)?;
                let owner_type_expr = owner_type
                    .clone()
                    .into_expr()
                    .with_location(e.location.clone());
                let owner_type_token = self.token_expr(&owner_type_expr)?;
                let (owner_signature, owner_arguments) = match owner_type {
                    argon_expr::MethodInstanceType::Record(t) => {
                        (t.record.clone().signature(), &t.arguments)
                    }
                    argon_expr::MethodInstanceType::Enum(t) => {
                        (t.enum_.clone().signature(), &t.arguments)
                    }
                    argon_expr::MethodInstanceType::Trait(t) => {
                        (t.trait_.clone().signature(), &t.arguments)
                    }
                };
                let mut args = Vec::new();
                for (param, argument) in owner_signature
                    .parameters
                    .iter()
                    .zip(owner_arguments.iter())
                {
                    match param.erasure_mode {
                        ErasureMode::Token => {}
                        ErasureMode::Concrete => {
                            args.push(Box::new(self.expr(argument, AnyRegister)?))
                        }
                        ErasureMode::Erased => {}
                    }
                }
                let mut sig = method.clone().signature().as_ref().clone();
                sig.substitute_method_instance_type_parameters(owner_type);
                let method_args = self.emit_arguments(
                    ExpressionOwner::StaticMethod(method.clone()),
                    &sig,
                    arguments,
                )?;
                args.extend(method_args.arguments);
                let static_method_id =
                    BigUint::from(self.encoder.get_static_method_id(method.clone()));
                self.emit(vf::Instruction::StaticMethodCall {
                    static_method_id,
                    owner_type: Box::new(owner_type_token),
                    dest: Box::new(frb.function_result()),
                    token_args: method_args.token_arguments,
                    args,
                });
                frb.into_result(self)?
            }

            Expr::NewInstance {
                instance,
                arguments,
            } => {
                let rb = output.output_register(self, e)?;

                let instance_id = self.encoder.get_instance_id(instance.clone());
                let sig = instance.clone().signature();
                let args = self.emit_arguments(
                    ExpressionOwner::Instance(instance.clone()),
                    &sig,
                    arguments,
                )?;

                self.emit(vf::Instruction::NewInstance {
                    dest: Box::new(rb.register().clone()),
                    instance_id: BigUint::from(instance_id),
                    token_args: args.token_arguments,
                    args: args.arguments,
                });

                rb.into_result(self)?
            }

            Expr::Raise { ex } => {
                let exception = self.expr(ex, AnyRegister)?;
                self.emit(vf::Instruction::Raise {
                    exception: Box::new(exception),
                });

                Err(EmitStop::Branch)?
            }

            // RecordType
            Expr::Retry { label } => {
                let (block_id, _) = self.get_block_id(label)?;
                let block_id = block_id.clone();

                self.emit(vf::Instruction::BlockRetry {
                    block_id: Box::new(block_id),
                });

                Err(EmitStop::Branch)?
            }

            // EnumType
            // RefCellType
            // TraitType
            Expr::RecordFieldLoad {
                record_type: _,
                field,
                record_value,
            } => {
                let rb = output.output_register(self, e)?;
                let field_id = BigUint::from(self.encoder.get_record_field_id(field.clone()));
                let value_reg = self.expr(record_value, AnyRegister)?;
                self.emit(vf::Instruction::RecordFieldLoad {
                    dest: Box::new(rb.register().clone()),
                    field_id,
                    record_value: Box::new(value_reg),
                });
                rb.into_result(self)?
            }

            Expr::RecordFieldStore {
                record_type: _,
                field,
                record_value,
                new_value,
            } => {
                let field_id = BigUint::from(self.encoder.get_record_field_id(field.clone()));
                let record_value_reg = self.expr(record_value, AnyRegister)?;
                let new_value_reg = self.expr(new_value, AnyRegister)?;

                self.emit(vf::Instruction::RecordFieldStore {
                    field_id,
                    record_value: Box::new(record_value_reg),
                    field_value: Box::new(new_value_reg),
                });

                output.output_unit_result(self)?
            }

            Expr::RecordLiteral {
                record_type,
                fields,
            } => {
                let rb = output.output_register(self, e)?;

                let mut field_values = HashMap::with_capacity(fields.len());
                for field in fields {
                    let value = self.expr(&field.value, AnyRegister)?;
                    field_values.insert(field.field.clone(), value);
                }

                let mut vm_fields = Vec::with_capacity(fields.len());
                for field in record_type.record.clone().fields().iter() {
                    let field_id = BigUint::from(self.encoder.get_record_field_id(field.clone()));
                    let value = field_values
                        .remove(field)
                        .expect("Record literal is missing a field after type checking");
                    vm_fields.push(Box::new(vf::RecordFieldLiteral {
                        field_id,
                        value: Box::new(value),
                    }));
                }

                let record_type_token = self.token_expr(
                    &Expr::RecordType(record_type.clone()).with_location(e.location.clone()),
                )?;

                self.emit(vf::Instruction::RecordLiteral {
                    dest: Box::new(rb.register().clone()),
                    record_type: Box::new(record_type_token),
                    fields: vm_fields,
                });

                rb.into_result(self)?
            }

            // RefCellCreate
            Expr::Sequence(items) => {
                let mut item = items.first();
                for next_item in items.iter().skip(1) {
                    self.expr(item, ExprOutputKnown::Discard)?;
                    item = next_item;
                }

                self.expr(item, output)?
            }

            Expr::Share { value } => self.expr(value, output)?,

            Expr::StringLiteral(s) => {
                let rb = output.output_register(self, e)?;

                self.emit(vf::Instruction::ConstString {
                    dest: Box::new(rb.register().clone()),
                    value: s.as_ref().to_owned(),
                });
                rb.into_result(self)?
            }

            Expr::Tuple { items } => {
                if let Some(result) = output.try_discard() {
                    for item in items {
                        self.expr(item, ExprOutputKnown::Discard)?;
                    }

                    result
                } else {
                    let rb = output.output_register(self, e)?;

                    let item_regs = items
                        .iter()
                        .map(|item| self.expr(item, AnyRegister).map(Box::new))
                        .collect::<Result<Vec<_>, _>>()?;

                    self.emit(vf::Instruction::Tuple {
                        dest: Box::new(rb.register().clone()),
                        values: item_regs,
                    });

                    rb.into_result(self)?
                }
            }

            Expr::TupleElement(tuple, index) => {
                let rb = output.output_register(self, e)?;

                let tuple_reg = self.expr(tuple, AnyRegister)?;

                self.emit(vf::Instruction::TupleElement {
                    dest: Box::new(rb.register().clone()),
                    element_index: BigUint::from(*index),
                    src: Box::new(tuple_reg),
                });

                rb.into_result(self)?
            }

            Expr::Unbox { t, value } => {
                let rb = output.output_register(self, e)?;
                let boxed_value = self.expr(value, AnyRegister)?;

                let tt = self.token_expr(t)?;

                self.emit(vf::Instruction::Unbox {
                    dest: Box::new(rb.register().clone()),
                    r#type: Box::new(tt),
                    value: Box::new(boxed_value),
                });

                rb.into_result(self)?
            }

            Expr::Variable(v) => {
                let Some(realization) = self.known_vars.get(v) else {
                    todo!("return a proper error")
                };

                match realization {
                    VariableRealization::Reg(reg) => output.copy_from(self, reg.clone())?,
                    VariableRealization::RegRefCell(reg) => {
                        let reg = reg.clone();

                        let rb = output.output_register(self, e)?;
                        self.emit(vf::Instruction::LoadReference {
                            dest: Box::new(rb.register().clone()),
                            r#ref: Box::new(reg),
                        });

                        rb.into_result(self)?
                    }
                    VariableRealization::Tok(tok) => {
                        let tok = tok.clone();

                        let rb = output.output_register(self, e)?;
                        self.emit(vf::Instruction::LoadToken {
                            dest: Box::new(rb.register().clone()),
                            token: Box::new(tok),
                        });

                        rb.into_result(self)?
                    }
                    VariableRealization::InstanceField {
                        instance,
                        instance_type,
                        parameter_index,
                    } => {
                        let instance_object = Box::new(instance.clone());
                        let instance_type = Box::new(instance_type.clone());
                        let parameter_index = BigUint::from(*parameter_index);

                        let rb = output.output_register(self, e)?;

                        self.emit(vf::Instruction::LoadInstanceField {
                            dest: Box::new(rb.register().clone()),
                            instance_object,
                            instance_type,
                            parameter_index,
                        });

                        rb.into_result(self)?
                    }
                }
            }

            Expr::VariableBinding(v, value) => {
                if v.erasure_mode != ErasureMode::Erased {
                    let v = Variable::Local(v.clone());
                    let r = self.declare_var(v.clone())?;
                    if self.captured_vars.contains(&v) && v.is_mutable() {
                        let value_reg = self.expr(value, AnyRegister)?;
                        self.emit(vf::Instruction::NewReference {
                            dest: Box::new(r),
                            value: Box::new(value_reg),
                        });
                    } else {
                        self.expr(value, ExprOutputKnown::Register(r))?;
                    }
                }

                output.output_unit_result(self)?
            }

            Expr::BindEnsures { value, .. } | Expr::BindErasedAlias { value, .. } => {
                self.expr(value, output)?
            }

            Expr::VariableStore(v, value) => {
                let Some(realization) = self.known_vars.get(v) else {
                    todo!("return a proper error")
                };

                match realization {
                    VariableRealization::Reg(reg) => {
                        self.expr(value, ExprOutputKnown::Register(reg.clone()))?;
                    }
                    VariableRealization::RegRefCell(reg) => {
                        self.expr(value, ExprOutputKnown::RefCell(reg.clone()))?;
                    }
                    VariableRealization::Tok(_) | VariableRealization::InstanceField { .. } => {
                        todo!("return a proper error")
                    }
                }

                output.output_unit_result(self)?
            }

            _ => todo!("Unimplemented emit expression: {:?}", e),
        })
    }

    fn emit_block(&mut self, block_id: vf::BlockId, block: Box<vf::Region>) {
        let mut scan = BlockJumpScan::new(&block_id);
        scan.scan_region(&block);

        if !scan.has_break && !scan.has_retry {
            self.emit_region(block);
        } else {
            let flags = vf::BlockFlags {
                has_break: scan.has_break,
                has_retry: scan.has_retry,
                is_loop: false,
            };

            self.emit_region(Box::new(vf::Region::Block {
                block_id: Box::new(block_id),
                flags,
                region: block,
            }));
        }
    }

    fn emit_arguments(
        &mut self,
        owner: ExpressionOwner<DefaultExprContext>,
        sig: &FunctionSignature<DefaultExprContext>,
        args: &[LocatedExpr<DefaultExprContext>],
    ) -> EmitResult<FunctionArguments> {
        let sig_with_mapping = self.encoder.emit_function_signature(&owner, sig)?;
        self.emit_arguments_common(sig_with_mapping, args)
    }

    fn emit_arguments_common(
        &mut self,
        sig: FunctionSignatureWithMapping,
        args: &[LocatedExpr<DefaultExprContext>],
    ) -> EmitResult<FunctionArguments> {
        let mut arguments = Vec::with_capacity(args.len());
        let mut token_arguments = Vec::with_capacity(args.len());

        for (consumer, arg) in sig.arg_consumers.iter().zip(args) {
            match consumer {
                ArgConsumer::Erased => {}
                ArgConsumer::Token => {
                    token_arguments.push(Box::new(self.token_expr(arg)?));
                }
                ArgConsumer::Arg => {
                    arguments.push(Box::new(self.expr(arg, AnyRegister)?));
                }
            }
        }

        Ok(FunctionArguments {
            token_arguments,
            arguments,
        })
    }

    fn expr_return(&mut self, e: &LocatedExpr<DefaultExprContext>) -> EmitResult<()> {
        self.expr(e, ExprOutputKnown::Return)
    }

    fn capture_variables(
        &mut self,
        captured_vars: &HashSet<Variable<DefaultExprContext>>,
        build_rest: impl FnOnce(
            &mut Self,
            &mut FunctionSignatureBuilder,
        ) -> Result<(), InternalCompilerError>,
        finish: impl FnOnce(
            &mut Self,
            FunctionSignatureBuilder,
        ) -> Result<FunctionSignatureWithMapping, InternalCompilerError>,
    ) -> Result<
        (
            Vec<Box<vf::Token>>,
            Vec<Box<vf::RegisterId>>,
            FunctionSignatureWithMapping,
        ),
        InternalCompilerError,
    > {
        let mut sb = FunctionSignatureBuilder::new();

        for v in captured_vars {
            sb.add_parameter(self.encoder, v.clone(), v.is_mutable())?;
        }

        build_rest(self, &mut sb)?;
        let sig = finish(self, sb)?;

        let mut args = Vec::new();
        let mut token_args = Vec::new();

        for (consumer, v) in sig.arg_consumers.iter().zip(captured_vars.iter()) {
            match consumer {
                ArgConsumer::Erased => {}
                ArgConsumer::Token => match self.known_vars.get(v) {
                    Some(VariableRealization::Tok(tok)) => {
                        token_args.push(Box::new(tok.clone()));
                    }
                    _ => unreachable!("Capture type mismatch"),
                },
                ArgConsumer::Arg => match self.known_vars.get(v) {
                    Some(VariableRealization::Reg(reg)) => {
                        args.push(Box::new(reg.clone()));
                    }
                    Some(VariableRealization::RegRefCell(reg)) => {
                        args.push(Box::new(reg.clone()));
                    }
                    _ => unreachable!("Capture type mismatch"),
                },
            }
        }

        Ok((token_args, args, sig))
    }
}

#[derive(Debug, Clone)]
enum VariableRealization {
    Reg(vf::RegisterId),
    RegRefCell(vf::RegisterId),
    Tok(vf::Token),
    InstanceField {
        instance: vf::RegisterId,
        instance_type: vf::Token,
        parameter_index: usize,
    },
}

struct FunctionArguments {
    token_arguments: Vec<Box<vf::Token>>,
    arguments: Vec<Box<vf::RegisterId>>,
}

trait ExprOutput: Sized {
    type ResultType;

    fn into_known_location(
        self,
        expr_emitter: &mut ExprEmitter<'_>,
        e: &LocatedExpr<DefaultExprContext>,
    ) -> EmitResult<(Self::ResultType, ExprOutputKnown)>;
    fn copy_from(
        self,
        expr_emitter: &mut ExprEmitter<'_>,
        r: vf::RegisterId,
    ) -> EmitResult<Self::ResultType>;
    fn output_register(
        self,
        expr_emitter: &mut ExprEmitter<'_>,
        e: &LocatedExpr<DefaultExprContext>,
    ) -> EmitResult<OutputRegisterBuilder<Self>>;
    fn output_function_result(
        self,
        expr_emitter: &mut ExprEmitter<'_>,
        e: &LocatedExpr<DefaultExprContext>,
    ) -> EmitResult<OutputFunctionResultBuilder<Self::ResultType>>;
    fn output_unit_result(self, expr_emitter: &mut ExprEmitter<'_>)
    -> EmitResult<Self::ResultType>;
    fn try_discard(&self) -> Option<Self::ResultType>;
}

#[derive(Clone)]
enum ExprOutputKnown {
    Register(vf::RegisterId),
    RefCell(vf::RegisterId),
    Discard,
    Return,
}

impl ExprOutput for ExprOutputKnown {
    type ResultType = ();

    fn into_known_location(
        self,
        _expr_emitter: &mut ExprEmitter<'_>,
        _e: &LocatedExpr<DefaultExprContext>,
    ) -> EmitResult<(Self::ResultType, ExprOutputKnown)> {
        Ok(((), self))
    }

    fn copy_from(
        self,
        expr_emitter: &mut ExprEmitter<'_>,
        r: vf::RegisterId,
    ) -> EmitResult<Self::ResultType> {
        Ok(match self {
            ExprOutputKnown::Register(out_reg) => {
                if out_reg.id != r.id {
                    expr_emitter.emit(vf::Instruction::Move {
                        dest: Box::new(out_reg),
                        src: Box::new(r),
                    });
                }
            }
            ExprOutputKnown::RefCell(cell) => {
                expr_emitter.emit(vf::Instruction::UpdateReference {
                    r#ref: Box::new(cell),
                    value: Box::new(r),
                });
            }
            ExprOutputKnown::Discard => {}
            ExprOutputKnown::Return => {
                expr_emitter.emit(vf::Instruction::Return { src: Box::new(r) });
                Err(EmitStop::Branch)?
            }
        })
    }

    fn output_register(
        self,
        expr_emitter: &mut ExprEmitter<'_>,
        e: &LocatedExpr<DefaultExprContext>,
    ) -> EmitResult<OutputRegisterBuilder<Self>> {
        Ok(match &self {
            ExprOutputKnown::Register(r) => OutputRegisterBuilder {
                r: r.clone(),
                output: self,
            },
            _ => {
                let t = token_for_expr_type(expr_emitter, e)?;
                let r = expr_emitter.add_var(t);
                OutputRegisterBuilder { r, output: self }
            }
        })
    }

    fn output_function_result(
        self,
        expr_emitter: &mut ExprEmitter<'_>,
        e: &LocatedExpr<DefaultExprContext>,
    ) -> EmitResult<OutputFunctionResultBuilder<Self::ResultType>> {
        let builder_type = match self {
            ExprOutputKnown::Register(r) => OutputFunctionResultBuilderType::Register(r),
            ExprOutputKnown::RefCell(cell) => {
                let t = token_for_expr_type(expr_emitter, e)?;
                let r = expr_emitter.add_var(t);
                OutputFunctionResultBuilderType::RefCell {
                    cell,
                    function_output: r,
                }
            }
            ExprOutputKnown::Discard => OutputFunctionResultBuilderType::Discard,
            ExprOutputKnown::Return if !expr_emitter.allow_tail_call => {
                let t = token_for_expr_type(expr_emitter, e)?;
                let r = expr_emitter.add_var(t);
                OutputFunctionResultBuilderType::ReturnNonTail(r)
            }
            ExprOutputKnown::Return => OutputFunctionResultBuilderType::Return,
        };

        Ok(OutputFunctionResultBuilder {
            builder_type,
            result: (),
        })
    }

    fn output_unit_result(
        self,
        expr_emitter: &mut ExprEmitter<'_>,
    ) -> EmitResult<Self::ResultType> {
        Ok(match self {
            ExprOutputKnown::Discard => (),

            _ => {
                let r = expr_emitter.add_var(vf::Token::Tuple { elements: vec![] });
                expr_emitter.emit(vf::Instruction::Tuple {
                    dest: Box::new(r.clone()),
                    values: vec![],
                });
                self.copy_from(expr_emitter, r)?
            }
        })
    }

    fn try_discard(&self) -> Option<Self::ResultType> {
        matches!(self, ExprOutputKnown::Discard).then(|| ())
    }
}

fn token_for_expr_type(
    expr_emitter: &mut ExprEmitter<'_>,
    e: &LocatedExpr<DefaultExprContext>,
) -> Result<vf::Token, InternalCompilerError> {
    let t = get_expr_type(e);
    expr_emitter.token_expr(&t)
}

struct AnyRegister;

impl ExprOutput for AnyRegister {
    type ResultType = vf::RegisterId;

    fn into_known_location(
        self,
        expr_emitter: &mut ExprEmitter<'_>,
        e: &LocatedExpr<DefaultExprContext>,
    ) -> EmitResult<(Self::ResultType, ExprOutputKnown)> {
        let t = token_for_expr_type(expr_emitter, e)?;
        let r = expr_emitter.add_var(t);
        Ok((r.clone(), ExprOutputKnown::Register(r)))
    }

    fn copy_from(
        self,
        expr_emitter: &mut ExprEmitter<'_>,
        r: vf::RegisterId,
    ) -> EmitResult<Self::ResultType> {
        if &r.id < &BigUint::from(expr_emitter.var_offset) {
            // Skip copy because it is a parameter.
            Ok(r)
        } else {
            let v = &expr_emitter.declared_vars[(&r.id - expr_emitter.var_offset)
                .to_usize()
                .expect("register id is not a usize")];
            let t = (*v.r#type).clone();

            let dest = expr_emitter.add_var(t);
            expr_emitter.emit(vf::Instruction::Move {
                dest: Box::new(dest.clone()),
                src: Box::new(r),
            });
            Ok(dest)
        }
    }

    fn output_register(
        self,
        expr_emitter: &mut ExprEmitter<'_>,
        e: &LocatedExpr<DefaultExprContext>,
    ) -> EmitResult<OutputRegisterBuilder<Self>> {
        let t = token_for_expr_type(expr_emitter, e)?;
        let r = expr_emitter.add_var(t);
        Ok(OutputRegisterBuilder { r, output: self })
    }

    fn output_function_result(
        self,
        expr_emitter: &mut ExprEmitter<'_>,
        e: &LocatedExpr<DefaultExprContext>,
    ) -> EmitResult<OutputFunctionResultBuilder<Self::ResultType>> {
        let (result, ko) = self.into_known_location(expr_emitter, e)?;
        let builder = ko.output_function_result(expr_emitter, e)?;
        Ok(OutputFunctionResultBuilder {
            builder_type: builder.builder_type,
            result,
        })
    }

    fn output_unit_result(
        self,
        expr_emitter: &mut ExprEmitter<'_>,
    ) -> EmitResult<Self::ResultType> {
        let r = expr_emitter.add_var(vf::Token::Tuple { elements: vec![] });
        expr_emitter.emit(vf::Instruction::Tuple {
            dest: Box::new(r.clone()),
            values: vec![],
        });
        Ok(r)
    }

    fn try_discard(&self) -> Option<Self::ResultType> {
        None
    }
}

struct OutputRegisterBuilder<O> {
    r: vf::RegisterId,
    output: O,
}

impl<O: ExprOutput> OutputRegisterBuilder<O> {
    fn register(&self) -> &vf::RegisterId {
        &self.r
    }

    fn into_result(self, expr_emitter: &mut ExprEmitter<'_>) -> EmitResult<O::ResultType> {
        self.output.copy_from(expr_emitter, self.r)
    }
}

struct OutputFunctionResultBuilder<R> {
    builder_type: OutputFunctionResultBuilderType,
    result: R,
}

impl<R> OutputFunctionResultBuilder<R> {
    fn function_result(&self) -> vf::FunctionResult {
        match &self.builder_type {
            OutputFunctionResultBuilderType::Register(r)
            | OutputFunctionResultBuilderType::ReturnNonTail(r) => vf::FunctionResult::Register {
                id: Box::new(r.clone()),
            },
            OutputFunctionResultBuilderType::RefCell {
                function_output, ..
            } => vf::FunctionResult::Register {
                id: Box::new(function_output.clone()),
            },
            OutputFunctionResultBuilderType::Discard => vf::FunctionResult::Discard {},
            OutputFunctionResultBuilderType::Return => vf::FunctionResult::ReturnValue {},
        }
    }

    fn into_result(self, expr_emitter: &mut ExprEmitter<'_>) -> EmitResult<R> {
        match self.builder_type {
            OutputFunctionResultBuilderType::RefCell {
                cell,
                function_output,
            } => {
                expr_emitter.emit(vf::Instruction::UpdateReference {
                    r#ref: Box::new(cell),
                    value: Box::new(function_output),
                });
            }

            OutputFunctionResultBuilderType::ReturnNonTail(r) => {
                expr_emitter.emit(vf::Instruction::Return { src: Box::new(r) });
                return Err(EmitStop::Branch);
            }

            OutputFunctionResultBuilderType::Return => {
                return Err(EmitStop::Branch);
            }

            _ => {}
        }

        Ok(self.result)
    }
}

enum OutputFunctionResultBuilderType {
    Register(vf::RegisterId),
    RefCell {
        cell: vf::RegisterId,
        function_output: vf::RegisterId,
    },
    Discard,
    Return,
    ReturnNonTail(vf::RegisterId),
}

fn variable_erasure_mode(variable: &Variable<DefaultExprContext>) -> ErasureMode {
    match variable {
        Variable::Local(variable) => variable.erasure_mode,
        Variable::Parameter(variable) => variable.erasure_mode,
        Variable::InstanceParameter(_) => ErasureMode::Concrete,
        Variable::ClosureParameter(variable) => variable.erasure_mode,
    }
}

fn method_receiver_type(method: Arc<dyn Method>) -> LocatedExpr<DefaultExprContext> {
    match method.owner() {
        MethodOwner::Record(record) => {
            let owner = ExpressionOwner::Record(record.clone());
            let signature = record.clone().signature();
            let location = record.location();
            Expr::RecordType(RecordType {
                record,
                arguments: signature
                    .parameters
                    .iter()
                    .enumerate()
                    .map(|(index, param)| {
                        Expr::Variable(Variable::Parameter(Box::new(
                            param.clone().to_parameter_var(owner.clone(), index),
                        )))
                        .with_location(location.clone())
                    })
                    .collect(),
            })
            .with_location(location)
        }
        MethodOwner::Enum(enum_) => {
            let owner = ExpressionOwner::Enum(enum_.clone());
            let signature = enum_.clone().signature();
            let location = enum_.location();
            Expr::EnumType(EnumType {
                enum_,
                arguments: signature
                    .parameters
                    .iter()
                    .enumerate()
                    .map(|(index, param)| {
                        Expr::Variable(Variable::Parameter(Box::new(
                            param.clone().to_parameter_var(owner.clone(), index),
                        )))
                        .with_location(location.clone())
                    })
                    .collect(),
            })
            .with_location(location)
        }
        MethodOwner::EnumVariant(variant) => variant.signature().return_type.clone(),
        MethodOwner::Trait(trait_) => {
            let trait_owner = ExpressionOwner::Trait(trait_.clone());
            let signature = trait_.clone().signature();
            let location = trait_.location();

            Expr::TraitType(TraitType {
                trait_,
                arguments: signature
                    .parameters
                    .iter()
                    .enumerate()
                    .map(|(index, param)| {
                        Expr::Variable(Variable::Parameter(Box::new(
                            param.clone().to_parameter_var(trait_owner.clone(), index),
                        )))
                        .with_location(location.clone())
                    })
                    .collect(),
            })
            .with_location(location)
        }
        MethodOwner::Instance(instance) => instance.signature().return_type.clone(),
    }
}

fn encode_module_path(path: &argon_compiler::ModulePath) -> vf::ModulePath {
    vf::ModulePath {
        path: path.0.clone(),
    }
}

fn encode_tube_name(name: &TubeName) -> vf::TubeName {
    vf::TubeName {
        head: name.0.first().clone(),
        tail: name.0.iter().skip(1).cloned().collect(),
    }
}

fn encode_identifier(id: &Identifier) -> vf::Identifier {
    match id {
        Identifier::Named(s) => vf::Identifier::Named { s: s.clone() },
        Identifier::BinaryOp(op) => vf::Identifier::BinOp {
            op: encode_binary_operator(*op),
        },
        Identifier::UnaryOp(op) => vf::Identifier::UnOp {
            op: encode_unary_operator(*op),
        },
        Identifier::Index => vf::Identifier::Index {},
        Identifier::Extension(inner) => vf::Identifier::Extension {
            inner: Box::new(encode_identifier(inner)),
        },
        Identifier::Inverse(inner) => vf::Identifier::Inverse {
            inner: Box::new(encode_identifier(inner)),
        },
        Identifier::Update(inner) => vf::Identifier::Update {
            inner: Box::new(encode_identifier(inner)),
        },
    }
}

fn encode_binary_operator(op: BinaryOperatorIdentifier) -> vf::BinaryOperator {
    match op {
        BinaryOperatorIdentifier::Plus => vf::BinaryOperator::Plus,
        BinaryOperatorIdentifier::Minus => vf::BinaryOperator::Minus,
        BinaryOperatorIdentifier::Mul => vf::BinaryOperator::Mul,
        BinaryOperatorIdentifier::Div => vf::BinaryOperator::Div,
        BinaryOperatorIdentifier::Equal => vf::BinaryOperator::Equal,
        BinaryOperatorIdentifier::NotEqual => vf::BinaryOperator::NotEqual,
        BinaryOperatorIdentifier::LessThan => vf::BinaryOperator::LessThan,
        BinaryOperatorIdentifier::LessThanEq => vf::BinaryOperator::LessThanEq,
        BinaryOperatorIdentifier::GreaterThan => vf::BinaryOperator::GreaterThan,
        BinaryOperatorIdentifier::GreaterThanEq => vf::BinaryOperator::GreaterThanEq,
        BinaryOperatorIdentifier::BitOr => vf::BinaryOperator::BitOr,
        BinaryOperatorIdentifier::BitXOr => vf::BinaryOperator::BitXor,
        BinaryOperatorIdentifier::BitAnd => vf::BinaryOperator::BitAnd,
        BinaryOperatorIdentifier::ShiftLeft => vf::BinaryOperator::ShiftLeft,
        BinaryOperatorIdentifier::ShiftRight => vf::BinaryOperator::ShiftRight,
        BinaryOperatorIdentifier::Concat => vf::BinaryOperator::Concat,
    }
}

fn encode_unary_operator(op: UnaryOperatorIdentifier) -> vf::UnaryOperator {
    match op {
        UnaryOperatorIdentifier::Plus => vf::UnaryOperator::Plus,
        UnaryOperatorIdentifier::Minus => vf::UnaryOperator::Minus,
        UnaryOperatorIdentifier::BitNot => vf::UnaryOperator::BitNot,
    }
}

fn encode_vm_integer_type(integer_type: IntegerType) -> vf::IntegerType {
    match integer_type {
        IntegerType::Int => vf::IntegerType::Int {},
        IntegerType::I8 => vf::IntegerType::I8 {},
        IntegerType::U8 => vf::IntegerType::U8 {},
        IntegerType::I16 => vf::IntegerType::I16 {},
        IntegerType::U16 => vf::IntegerType::U16 {},
        IntegerType::I32 => vf::IntegerType::I32 {},
        IntegerType::U32 => vf::IntegerType::U32 {},
        IntegerType::I64 => vf::IntegerType::I64 {},
        IntegerType::U64 => vf::IntegerType::U64 {},
    }
}

fn import_specifier_tube(import: &ImportSpecifier) -> &TubeName {
    match import {
        ImportSpecifier::Global { tube, .. } => tube,
        ImportSpecifier::Local { parent, .. } => import_specifier_tube(parent),
    }
}

fn is_branch_instruction(insn: &vf::Instruction) -> bool {
    match insn {
        vf::Instruction::BlockBreak { .. }
        | vf::Instruction::BlockBreakIf { .. }
        | vf::Instruction::BlockBreakUnless { .. }
        | vf::Instruction::BlockRetry { .. }
        | vf::Instruction::IsEnumVariantOrBreak { .. }
        | vf::Instruction::Return { .. } => true,

        vf::Instruction::FunctionCall { dest, .. }
        | vf::Instruction::FunctionObjectCall { dest, .. }
        | vf::Instruction::FunctionObjectTokenCall { dest, .. }
        | vf::Instruction::FunctionObjectErasedCall { dest, .. }
        | vf::Instruction::InstanceMethodCall { dest, .. } => {
            matches!(&**dest, vf::FunctionResult::ReturnValue {})
        }

        _ => false,
    }
}
