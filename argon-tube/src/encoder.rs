use alloc::collections::{BTreeMap, VecDeque};
use alloc::{boxed::Box, string::ToString, sync::Arc, vec::Vec};
use argon_compiler::{
    AccessModifierGlobal, BinaryOperatorIdentifier, Builtin, Context, DefaultExprContext,
    EffectInfo, Enum, EnumVariant, ErasureMode, Expr, Function, FunctionImplementation,
    FunctionParameterListType, FunctionSignature, Identifier, Module, ModuleExportBinding,
    ModuleExportEntry, ModulePath, Record, RecordField, RecordFieldOwner, Tube, TubeName,
    UnaryOperatorIdentifier,
};
use core::mem;
use num_bigint::{BigInt, BigUint};

use crate::ids::TubeIdProvider;
use argon_expr::{
    BlockLabel, BlockLabelKind, ExpressionOwner, LocalVariable, Pattern, RecordFieldPattern,
    RecordType, Variable,
};
use argon_format::tube as tf;

use argon_compiler::erased_sig::{
    ErasedSignature, ErasedSignatureType, ImportSpecifier, erase_signature,
};
use argon_util::InternalCompilerError;
use esexpr::ESExprStatic;

pub fn encode_tube(
    context: Context,
    tube: Arc<Tube>,
) -> impl Iterator<Item = Result<tf::TubeFileEntry, InternalCompilerError>> {
    TubeEncoder::new(context, tube)
}

pub struct TubeEncoder {
    context: Context,
    tube: Arc<Tube>,

    // Used to ensure that modules are emitted in a consistent order.
    // Will be empty once the metadata is emitted.
    modules: Vec<Arc<Module>>,

    ids: TubeIdProvider,
    entry_emitters: VecDeque<EntryEmitter>,
}

impl TubeEncoder {
    pub fn new(context: Context, tube: Arc<Tube>) -> Self {
        let mut modules = tube
            .modules()
            .iter()
            .map(|(_, m)| m.clone())
            .collect::<Vec<_>>();

        modules.sort_by(|m1, m2| m1.path().cmp(m2.path()));

        let mut encoder = Self {
            context,
            tube,
            modules,
            ids: TubeIdProvider::default(),
            entry_emitters: VecDeque::new(),
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

    fn get_module_id(&mut self, tube_name: &TubeName, module_path: &ModulePath) -> usize {
        let (id, is_new) = self
            .ids
            .module_ids
            .get_with_new((tube_name.clone(), module_path.clone()));

        if is_new {
            self.entry_emitters.push_back(EntryEmitter::ModuleReference(
                tube_name.clone(),
                module_path.clone(),
            ));
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

    fn get_block_label_id(&mut self, label: &BlockLabel<DefaultExprContext>) -> usize {
        self.ids.block_label_ids.get(label.clone())
    }

    fn emit_block_label(
        &mut self,
        label: &BlockLabel<DefaultExprContext>,
    ) -> Result<tf::BlockLabel, InternalCompilerError> {
        Ok(tf::BlockLabel {
            id: self.get_block_label_id(label).into(),
            name: label
                .name
                .as_ref()
                .map(|name| encode_identifier(name).map(Box::new))
                .transpose()?,
            kind: encode_block_label_kind(label.kind),
            block_result_type: Box::new(self.emit_expr(&label.block_result_type)?),
        })
    }

    fn emit_block_id(&mut self, label: &BlockLabel<DefaultExprContext>) -> tf::BlockId {
        tf::BlockId {
            id: self.get_block_label_id(label).into(),
        }
    }

    fn encode_entry(
        &mut self,
        emitter: EntryEmitter,
    ) -> Result<Option<tf::TubeFileEntry>, InternalCompilerError> {
        Ok(Some(match emitter {
            EntryEmitter::Header => tf::TubeFileEntry::Header {
                header: Box::new(tf::TubeHeader {
                    format_version_major: BigInt::ZERO,
                    format_version_minor: BigInt::ZERO,
                }),
            },

            EntryEmitter::Metadata => {
                let modules = mem::take(&mut self.modules)
                    .into_iter()
                    .map(|module| self.emit_module(module).map(Box::new))
                    .collect::<Result<Vec<_>, _>>()?;

                tf::TubeFileEntry::Metadata {
                    metadata: Box::new(tf::TubeMetadata {
                        name: Box::new(encode_tube_name(self.tube.name())),

                        platforms: self
                            .tube
                            .metadata()
                            .platform
                            .keys()
                            .map(|platform| {
                                Box::new(tf::SupportedPlatform {
                                    id: platform.clone(),
                                })
                            })
                            .collect(),

                        platform_metadata: self
                            .tube
                            .metadata()
                            .platform
                            .iter()
                            .map(|(platform, expr)| {
                                (platform.clone(), ESExprStatic::new(expr.clone()))
                            })
                            .collect(),

                        referenced_tubes: self
                            .tube
                            .referenced_tubes()
                            .iter()
                            .map(|tube| Box::new(encode_tube_name(tube)))
                            .collect(),

                        modules,
                    }),
                }
            }

            EntryEmitter::ModuleReference(tube, module) => {
                let module_id =
                    BigUint::from(self.ids.module_ids.get((tube.clone(), module.clone())));
                let tube_id = BigUint::from(self.ids.tube_ids.get(tube.clone()));

                let path = encode_module_path(&module);
                tf::TubeFileEntry::ModuleReference {
                    tube_id,
                    module_id,
                    path: Box::new(path),
                }
            }

            EntryEmitter::Function(function) => {
                let function_id = BigUint::from(self.ids.function_ids.get(function.clone()));
                let import_specifier = function.clone().import_specifier();
                let import = self.encode_import_specifier(&import_specifier)?;

                if import_specifier_tube(&import_specifier) != self.tube.name() {
                    tf::TubeFileEntry::FunctionReference {
                        function_id,
                        import: Box::new(import),
                    }
                } else {
                    let metadata = function.metadata();
                    let implementation = function
                        .clone()
                        .implementation()
                        .map(|implementation| self.emit_function_implementation(&implementation))
                        .transpose()?
                        .map(Box::new);

                    tf::TubeFileEntry::FunctionDefinition {
                        definition: Box::new(tf::FunctionDefinition {
                            function_id,
                            import: Box::new(import),
                            inline: metadata.is_inline,
                            erasure: Box::new(encode_erasure_mode(metadata.erasure_mode)),
                            witness: metadata.is_witness,
                            effects: Box::new(encode_effect_info(metadata.effect_info)),
                            signature: Box::new(
                                self.emit_function_signature(&function.signature())?,
                            ),
                            implementation,
                        }),
                    }
                }
            }

            EntryEmitter::Record(record) => {
                let record_id = BigUint::from(self.ids.record_ids.get(record.clone()));
                let import_specifier = record.clone().import_specifier();
                let import = self.encode_import_specifier(&import_specifier)?;

                if import_specifier_tube(&import_specifier) != self.tube.name() {
                    tf::TubeFileEntry::RecordReference {
                        record_id,
                        import: Box::new(import),
                    }
                } else {
                    let fields = record
                        .clone()
                        .fields()
                        .iter()
                        .map(|field| {
                            self.get_record_field_id(field.clone());
                            let metadata = field.metadata();
                            Ok(Box::new(tf::RecordFieldDefinition {
                                name: Box::new(encode_identifier(&metadata.name)?),
                                field_type: Box::new(self.emit_expr(&field.clone().field_type())?),
                                mutable: metadata.is_mutable,
                            }))
                        })
                        .collect::<Result<Vec<_>, InternalCompilerError>>()?;

                    tf::TubeFileEntry::RecordDefinition {
                        definition: Box::new(tf::RecordDefinition {
                            record_id,
                            import: Box::new(import),
                            signature: Box::new(self.emit_function_signature(&record.signature())?),
                            fields,
                        }),
                    }
                }
            }

            EntryEmitter::RecordField(field) => match field.owning_record() {
                RecordFieldOwner::Record(record) => {
                    let record_field_id =
                        BigUint::from(self.ids.record_field_ids.get(field.clone()));
                    let record_id = BigUint::from(self.get_record_id(record));
                    let name = encode_identifier(&field.metadata().name)?;

                    tf::TubeFileEntry::RecordFieldReference {
                        record_field_id,
                        record_id,
                        name: Box::new(name),
                    }
                }
                RecordFieldOwner::EnumVariant(variant) => {
                    let record_field_id =
                        BigUint::from(self.ids.record_field_ids.get(field.clone()));
                    let variant_id = BigUint::from(self.get_enum_variant_id(variant));
                    let name = encode_identifier(&field.metadata().name)?;

                    tf::TubeFileEntry::EnumVariantRecordFieldReference {
                        record_field_id,
                        variant_id,
                        name: Box::new(name),
                    }
                }
            },
            EntryEmitter::Enum(enum_) => {
                let enum_id = BigUint::from(self.ids.enum_ids.get(enum_.clone()));
                let import_specifier = enum_.clone().import_specifier();
                let import = self.encode_import_specifier(&import_specifier)?;

                if import_specifier_tube(&import_specifier) != self.tube.name() {
                    tf::TubeFileEntry::EnumReference {
                        enum_id,
                        import: Box::new(import),
                    }
                } else {
                    let variants = enum_
                        .clone()
                        .variants()
                        .iter()
                        .map(|variant| {
                            self.get_enum_variant_id(variant.clone());
                            let fields = variant
                                .clone()
                                .fields()
                                .iter()
                                .map(|field| {
                                    self.get_record_field_id(field.clone());
                                    let metadata = field.metadata();
                                    Ok(Box::new(tf::RecordFieldDefinition {
                                        name: Box::new(encode_identifier(&metadata.name)?),
                                        field_type: Box::new(
                                            self.emit_expr(&field.clone().field_type())?,
                                        ),
                                        mutable: metadata.is_mutable,
                                    }))
                                })
                                .collect::<Result<Vec<_>, InternalCompilerError>>()?;

                            Ok(Box::new(tf::EnumVariantDefinition {
                                name: Box::new(encode_identifier(&variant.metadata().name)?),
                                signature: Box::new(
                                    self.emit_function_signature(&variant.clone().signature())?,
                                ),
                                fields,
                            }))
                        })
                        .collect::<Result<Vec<_>, InternalCompilerError>>()?;

                    tf::TubeFileEntry::EnumDefinition {
                        definition: Box::new(tf::EnumDefinition {
                            enum_id,
                            import: Box::new(import),
                            signature: Box::new(self.emit_function_signature(&enum_.signature())?),
                            variants,
                        }),
                    }
                }
            }
            EntryEmitter::EnumVariant(variant) => {
                let variant_id = BigUint::from(self.ids.enum_variant_ids.get(variant.clone()));
                let enum_id = BigUint::from(self.get_enum_id(variant.clone().owning_enum()));
                let name = encode_identifier(&variant.metadata().name)?;

                tf::TubeFileEntry::EnumVariantReference {
                    variant_id,
                    enum_id,
                    name: Box::new(name),
                }
            }
        }))
    }

    fn emit_module(&mut self, module: Arc<Module>) -> Result<tf::Module, InternalCompilerError> {
        let mut export_groups = module
            .export_groups()
            .iter()
            .map(|(group_name, items)| (group_name.clone(), items.clone()))
            .collect::<Vec<_>>();
        export_groups.sort_by(|(name1, _), (name2, _)| name1.cmp(name2));

        let groups = export_groups
            .into_iter()
            .map(|(name, exports)| {
                let exports = exports
                    .into_iter()
                    .map(|exp| self.emit_module_export(exp).map(Box::new))
                    .collect::<Result<Vec<_>, _>>()?;

                Ok(Box::new(tf::ExportGroup {
                    name: Box::new(encode_identifier(&name)?),
                    exports,
                }))
            })
            .collect::<Result<Vec<_>, InternalCompilerError>>()?;

        Ok(tf::Module {
            path: Box::new(encode_module_path(module.path())),
            groups,
        })
    }

    fn emit_module_export(
        &mut self,
        exp: ModuleExportEntry,
    ) -> Result<tf::ModuleExport, InternalCompilerError> {
        let module_export = match exp.binding {
            ModuleExportBinding::Function(function) => {
                let function_id = self.get_function_id(function.clone());
                let sig = function.signature();
                let erased_sig = erase_signature(self.context.clone(), sig.as_ref());
                let signature = self.encode_erased_signature(&erased_sig)?;
                tf::ModuleExport::Function {
                    function_id: BigUint::from(function_id),
                    access: Box::new(encode_access_modifier_global(exp.access)),
                    signature: Box::new(signature),
                }
            }

            ModuleExportBinding::Record(record) => {
                let record_id = self.get_record_id(record.clone());
                let sig = record.signature();
                let erased_sig = erase_signature(self.context.clone(), sig.as_ref());
                let signature = self.encode_erased_signature(&erased_sig)?;
                tf::ModuleExport::Record {
                    record_id: BigUint::from(record_id),
                    access: Box::new(encode_access_modifier_global(exp.access)),
                    signature: Box::new(signature),
                }
            }
            ModuleExportBinding::Enum(enum_) => {
                let enum_id = self.get_enum_id(enum_.clone());
                let sig = enum_.signature();
                let erased_sig = erase_signature(self.context.clone(), sig.as_ref());
                let signature = self.encode_erased_signature(&erased_sig)?;
                tf::ModuleExport::Enum {
                    enum_id: BigUint::from(enum_id),
                    access: Box::new(encode_access_modifier_global(exp.access)),
                    signature: Box::new(signature),
                }
            }
            ModuleExportBinding::Trait(_) => {
                todo!()
            }
            ModuleExportBinding::Instance(_) => {
                todo!()
            }
        };

        if exp.is_reexport {
            Ok(tf::ModuleExport::Exported {
                exp: Box::new(module_export),
            })
        } else {
            Ok(module_export)
        }
    }

    fn encode_erased_signature(
        &mut self,
        sig: &ErasedSignature,
    ) -> Result<tf::ErasedSignature, InternalCompilerError> {
        Ok(tf::ErasedSignature {
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
    ) -> Result<tf::ErasedSignatureType, InternalCompilerError> {
        Ok(match t {
            ErasedSignatureType::Builtin(builtin) => {
                let Some(builtin_type) = encode_erased_builtin_type(*builtin) else {
                    return Ok(tf::ErasedSignatureType::Erased {});
                };

                tf::ErasedSignatureType::Builtin {
                    b: Box::new(builtin_type),
                    args: Vec::new(),
                }
            }

            ErasedSignatureType::Function(input, output) => tf::ErasedSignatureType::Function {
                input: Box::new(self.encode_erased_signature_type(input)?),
                output: Box::new(self.encode_erased_signature_type(output)?),
            },

            ErasedSignatureType::Declared(record_import, args) => tf::ErasedSignatureType::Record {
                record_import: Box::new(self.encode_import_specifier(record_import)?),
                args: args
                    .iter()
                    .map(|arg| self.encode_erased_signature_type(arg).map(Box::new))
                    .collect::<Result<Vec<_>, _>>()?,
            },

            ErasedSignatureType::Tuple(elements) => tf::ErasedSignatureType::Tuple {
                elements: elements
                    .iter()
                    .map(|element| self.encode_erased_signature_type(element).map(Box::new))
                    .collect::<Result<Vec<_>, _>>()?,
            },

            ErasedSignatureType::Erased => tf::ErasedSignatureType::Erased {},
        })
    }

    fn encode_import_specifier(
        &mut self,
        import: &ImportSpecifier,
    ) -> Result<tf::ImportSpecifier, InternalCompilerError> {
        Ok(match import {
            ImportSpecifier::Global {
                tube,
                module,
                name,
                signature,
            } => {
                let module_id = self.get_module_id(tube, module);

                tf::ImportSpecifier::Global {
                    module_id: BigUint::from(module_id),
                    name: Box::new(encode_identifier(name)?),
                    sig: Box::new(self.encode_erased_signature(signature)?),
                }
            }

            ImportSpecifier::Local { parent, id } => tf::ImportSpecifier::Local {
                parent: Box::new(self.encode_import_specifier(parent)?),
                index: self.ids.local_import_ids.get(id.clone()).into(),
            },
        })
    }

    fn emit_function_signature(
        &mut self,
        sig: &FunctionSignature<argon_compiler::DefaultExprContext>,
    ) -> Result<tf::FunctionSignature, InternalCompilerError> {
        Ok(tf::FunctionSignature {
            parameters: sig
                .parameters
                .iter()
                .map(|param| {
                    Ok(Box::new(tf::SignatureParameter {
                        list_type: encode_parameter_list_type(param.list_type),
                        erasure: Box::new(encode_erasure_mode(param.erasure_mode)),
                        bindings: param
                            .bindings
                            .iter()
                            .map(|binding| {
                                Ok(Box::new(tf::ParameterBinding {
                                    name: binding
                                        .name
                                        .as_ref()
                                        .map(|name| encode_identifier(name).map(Box::new))
                                        .transpose()?,
                                    param_type: Box::new(self.emit_expr(&binding.param_type)?),
                                }))
                            })
                            .collect::<Result<Vec<_>, InternalCompilerError>>()?,
                        name: param
                            .name
                            .as_ref()
                            .map(|name| encode_identifier(name).map(Box::new))
                            .transpose()?,
                        param_type: Box::new(self.emit_expr(&param.param_type)?),
                    }))
                })
                .collect::<Result<Vec<_>, InternalCompilerError>>()?,
            return_type: Box::new(self.emit_expr(&sig.return_type)?),
            ensures_clauses: sig
                .ensures_clauses
                .iter()
                .map(|clause| self.emit_expr(clause).map(Box::new))
                .collect::<Result<Vec<_>, _>>()?,
        })
    }

    fn emit_function_implementation(
        &mut self,
        implementation: &FunctionImplementation,
    ) -> Result<tf::FunctionImplementation, InternalCompilerError> {
        Ok(match implementation {
            FunctionImplementation::Expr(expr) => tf::FunctionImplementation::Expr {
                body: Box::new(self.emit_expr(expr)?),
            },

            FunctionImplementation::Extern(externs) => tf::FunctionImplementation::Extern {
                externs: Box::new(tf::ExternMap {
                    externs: externs
                        .externs
                        .iter()
                        .map(|(platform, expr)| (platform.clone(), ESExprStatic::new(expr.clone())))
                        .collect::<BTreeMap<_, _>>()
                        .into(),
                }),
            },
        })
    }

    fn emit_expr(
        &mut self,
        expr: &Expr<argon_compiler::DefaultExprContext>,
    ) -> Result<tf::Expr, InternalCompilerError> {
        Ok(match expr {
            Expr::Error => tf::Expr::Error {},
            Expr::And(a, b) => tf::Expr::And {
                a: Box::new(self.emit_expr(a)?),
                b: Box::new(self.emit_expr(b)?),
            },
            Expr::BoolLiteral(value) => tf::Expr::BoolLiteral { value: *value },
            Expr::IntLiteral(i) => tf::Expr::IntLiteral { i: i.clone() },
            Expr::StringLiteral(s) => tf::Expr::StringLiteral { s: s.to_string() },
            Expr::Tuple { items } => tf::Expr::Tuple {
                items: items
                    .iter()
                    .map(|item| self.emit_expr(item).map(Box::new))
                    .collect::<Result<Vec<_>, _>>()?,
            },
            Expr::TupleElement(tuple, index) => tf::Expr::TupleElement {
                index: BigUint::from(*index),
                tuple: Box::new(self.emit_expr(tuple)?),
            },
            Expr::Builtin { builtin, arguments } => tf::Expr::Builtin {
                builtin: encode_builtin(*builtin)?,
                args: arguments
                    .iter()
                    .map(|arg| self.emit_expr(arg).map(Box::new))
                    .collect::<Result<Vec<_>, _>>()?,
            },
            Expr::FunctionCall {
                function,
                arguments,
            } => tf::Expr::FunctionCall {
                id: self.get_function_id(function.clone()).into(),
                args: arguments
                    .iter()
                    .map(|arg| self.emit_expr(arg).map(Box::new))
                    .collect::<Result<Vec<_>, _>>()?,
            },
            Expr::FunctionObjectCall { function, argument } => tf::Expr::FunctionObjectCall {
                f: Box::new(self.emit_expr(function)?),
                a: Box::new(self.emit_expr(argument)?),
            },
            Expr::FunctionType { a, r } => tf::Expr::FunctionType {
                a: Box::new(tf::LambdaParameterVar {
                    id: BigUint::ZERO,
                    var_type: Box::new(self.emit_expr(a)?),
                    name: None,
                    mutable: false,
                    erasure: Box::new(tf::ErasureMode::Concrete {}),
                    witness: false,
                }),
                r: Box::new(self.emit_expr(r)?),
            },
            Expr::Type(t) => tf::Expr::TypeN {
                n: Box::new(self.emit_expr(t)?),
            },
            Expr::BigType(n) => tf::Expr::TypeBigN {
                n: n.to_biguint().unwrap_or_default(),
            },
            Expr::BoxedType { t } => tf::Expr::Boxed {
                t: Box::new(self.emit_expr(t)?),
            },
            Expr::Box { t, value } => tf::Expr::Box {
                t: Box::new(self.emit_expr(t)?),
                value: Box::new(self.emit_expr(value)?),
            },
            Expr::Block { label, body } => tf::Expr::Block {
                label: Box::new(self.emit_block_label(label)?),
                body: Box::new(self.emit_expr(body)?),
            },
            Expr::Break { label, value } => tf::Expr::Break {
                block_id: Box::new(self.emit_block_id(label)),
                value: Box::new(self.emit_expr(value)?),
            },
            Expr::BreakIf {
                label,
                value,
                condition,
            } => tf::Expr::BreakIf {
                block_id: Box::new(self.emit_block_id(label)),
                value: Box::new(self.emit_expr(value)?),
                condition: Box::new(self.emit_expr(condition)?),
            },
            Expr::Unbox { t, value } => tf::Expr::Unbox {
                t: Box::new(self.emit_expr(t)?),
                value: Box::new(self.emit_expr(value)?),
            },
            Expr::Sequence(exprs) => tf::Expr::Sequence {
                head: Box::new(self.emit_expr(exprs.first())?),
                tail: exprs
                    .iter()
                    .skip(1)
                    .map(|expr| self.emit_expr(expr).map(Box::new))
                    .collect::<Result<Vec<_>, _>>()?,
            },
            Expr::Condition {
                value,
                when_true_witness,
                when_false_witness,
            } => tf::Expr::Condition {
                value: Box::new(self.emit_expr(value)?),
                when_true_witness: when_true_witness
                    .as_ref()
                    .map(|variable| self.emit_local_var_from_variable(variable).map(Box::new))
                    .transpose()?,
                when_false_witness: when_false_witness
                    .as_ref()
                    .map(|variable| self.emit_local_var_from_variable(variable).map(Box::new))
                    .transpose()?,
            },
            Expr::IfElse {
                condition,
                when_true,
                when_false,
            } => tf::Expr::IfElse {
                condition: Box::new(self.emit_expr(condition)?),
                true_body: Box::new(self.emit_expr(when_true)?),
                false_body: Box::new(self.emit_expr(when_false)?),
            },
            Expr::Is { value, pattern } => tf::Expr::Is {
                value: Box::new(self.emit_expr(value)?),
                pattern: Box::new(self.emit_pattern(pattern)?),
            },
            Expr::Or(a, b) => tf::Expr::Or {
                a: Box::new(self.emit_expr(a)?),
                b: Box::new(self.emit_expr(b)?),
            },
            Expr::Not(value) => tf::Expr::Not {
                a: Box::new(self.emit_expr(value)?),
            },
            Expr::Retry { label } => tf::Expr::Retry {
                block_id: Box::new(self.emit_block_id(label)),
            },
            Expr::RecordFieldLoad {
                record_type,
                field,
                record_value,
            } => {
                let Expr::RecordType(record_type) = &**record_type else {
                    panic!("record field load has non-record type {:?}", record_type);
                };

                tf::Expr::RecordFieldLoad {
                    record: Box::new(tf::RecordType {
                        id: BigUint::from(self.get_record_id(record_type.record.clone())),
                        args: record_type
                            .arguments
                            .iter()
                            .map(|arg| self.emit_expr(arg).map(Box::new))
                            .collect::<Result<Vec<_>, _>>()?,
                    }),
                    field_id: BigUint::from(self.get_record_field_id(field.clone())),
                    record_value: Box::new(self.emit_expr(record_value)?),
                }
            }
            Expr::RecordFieldStore {
                record_type,
                field,
                record_value,
                new_value,
            } => {
                let Expr::RecordType(record_type) = &**record_type else {
                    panic!("record field store has non-record type {:?}", record_type);
                };

                tf::Expr::RecordFieldStore {
                    record: Box::new(tf::RecordType {
                        id: BigUint::from(self.get_record_id(record_type.record.clone())),
                        args: record_type
                            .arguments
                            .iter()
                            .map(|arg| self.emit_expr(arg).map(Box::new))
                            .collect::<Result<Vec<_>, _>>()?,
                    }),
                    field_id: BigUint::from(self.get_record_field_id(field.clone())),
                    record_value: Box::new(self.emit_expr(record_value)?),
                    field_value: Box::new(self.emit_expr(new_value)?),
                }
            }
            Expr::RecordLiteral {
                record_type,
                fields,
            } => {
                let RecordType { record, arguments } = record_type;
                let declared_fields = record.clone().fields();
                tf::Expr::RecordLiteral {
                    record: Box::new(tf::RecordType {
                        id: BigUint::from(self.get_record_id(record.clone())),
                        args: arguments
                            .iter()
                            .map(|arg| self.emit_expr(arg).map(Box::new))
                            .collect::<Result<Vec<_>, _>>()?,
                    }),
                    fields: fields
                        .iter()
                        .map(|field| {
                            let declared_field = declared_fields
                                .iter()
                                .find(|declared_field| declared_field.metadata().name == field.name)
                                .unwrap_or_else(|| {
                                    panic!(
                                        "record literal references unknown field {:?}",
                                        field.name
                                    )
                                });
                            Ok(Box::new(tf::RecordFieldLiteral {
                                field_id: BigUint::from(
                                    self.get_record_field_id(declared_field.clone()),
                                ),
                                value: Box::new(self.emit_expr(&field.value)?),
                            }))
                        })
                        .collect::<Result<Vec<_>, InternalCompilerError>>()?,
                }
            }
            Expr::RecordType(record_type) => tf::Expr::RecordType {
                record_type: Box::new(tf::RecordType {
                    id: BigUint::from(self.get_record_id(record_type.record.clone())),
                    args: record_type
                        .arguments
                        .iter()
                        .map(|arg| self.emit_expr(arg).map(Box::new))
                        .collect::<Result<Vec<_>, _>>()?,
                }),
            },
            Expr::EnumType(enum_type) => tf::Expr::EnumType {
                enum_type: Box::new(tf::EnumType {
                    id: BigUint::from(self.get_enum_id(enum_type.enum_.clone())),
                    args: enum_type
                        .arguments
                        .iter()
                        .map(|arg| self.emit_expr(arg).map(Box::new))
                        .collect::<Result<Vec<_>, _>>()?,
                }),
            },
            Expr::EnumVariantLiteral {
                enum_type,
                variant,
                arguments,
                fields,
            } => {
                let declared_fields = variant.clone().fields();
                tf::Expr::EnumVariantLiteral {
                    enum_type: Box::new(tf::EnumType {
                        id: BigUint::from(self.get_enum_id(enum_type.enum_.clone())),
                        args: enum_type
                            .arguments
                            .iter()
                            .map(|arg| self.emit_expr(arg).map(Box::new))
                            .collect::<Result<Vec<_>, _>>()?,
                    }),
                    variant_id: BigUint::from(self.get_enum_variant_id(variant.clone())),
                    args: arguments
                        .iter()
                        .map(|arg| self.emit_expr(arg).map(Box::new))
                        .collect::<Result<Vec<_>, _>>()?,
                    fields: fields
                        .iter()
                        .map(|field| {
                            let declared_field = declared_fields
                                .iter()
                                .find(|declared_field| declared_field.metadata().name == field.name)
                                .unwrap_or_else(|| {
                                    panic!(
                                        "enum variant literal references unknown field {:?}",
                                        field.name
                                    )
                                });
                            Ok(Box::new(tf::RecordFieldLiteral {
                                field_id: BigUint::from(
                                    self.get_record_field_id(declared_field.clone()),
                                ),
                                value: Box::new(self.emit_expr(&field.value)?),
                            }))
                        })
                        .collect::<Result<Vec<_>, InternalCompilerError>>()?,
                }
            }
            Expr::Variable(variable) => tf::Expr::Variable {
                v: Box::new(self.emit_var(variable)?),
            },
            Expr::VariableBinding(variable, value) => tf::Expr::BindVariable {
                value: Box::new(self.emit_expr(value)?),
                v: Box::new(self.emit_local_var_from_variable(variable)?),
            },
            Expr::VariableStore(variable, value) => tf::Expr::VariableStore {
                v: Box::new(self.emit_var(variable)?),
                value: Box::new(self.emit_expr(value)?),
            },
            _ => todo!("Unimplement emit_expr for {:?}", expr),
        })
    }

    fn emit_pattern(
        &mut self,
        pattern: &Pattern<argon_compiler::DefaultExprContext>,
    ) -> Result<tf::Pattern, InternalCompilerError> {
        Ok(match pattern {
            Pattern::Error => tf::Pattern::Error {},
            Pattern::Discard { t } => tf::Pattern::Discard {
                t: Box::new(self.emit_expr(t)?),
            },
            Pattern::Tuple(items) => tf::Pattern::Tuple {
                items: items
                    .iter()
                    .map(|item| self.emit_pattern(item).map(Box::new))
                    .collect::<Result<Vec<_>, _>>()?,
            },
            Pattern::Binding(variable, pattern) => tf::Pattern::Binding {
                v: Box::new(self.emit_local_var(variable)?),
                pattern: Box::new(self.emit_pattern(pattern)?),
            },
            Pattern::EnumVariant {
                enum_type,
                variant,
                args,
                fields,
            } => tf::Pattern::EnumVariant {
                enum_type: Box::new(tf::EnumType {
                    id: BigUint::from(self.get_enum_id(enum_type.enum_.clone())),
                    args: enum_type
                        .arguments
                        .iter()
                        .map(|arg| self.emit_expr(arg).map(Box::new))
                        .collect::<Result<Vec<_>, _>>()?,
                }),
                variant_id: BigUint::from(self.get_enum_variant_id(variant.clone())),
                args: args
                    .iter()
                    .map(|arg| self.emit_pattern(arg).map(Box::new))
                    .collect::<Result<Vec<_>, _>>()?,
                fields: fields
                    .iter()
                    .map(|field| self.emit_record_field_pattern(field).map(Box::new))
                    .collect::<Result<Vec<_>, _>>()?,
            },
            Pattern::String(s) => tf::Pattern::String { s: s.clone() },
            Pattern::Int(i) => tf::Pattern::Int { i: i.clone() },
            Pattern::Bool(b) => tf::Pattern::Bool { b: *b },
        })
    }

    fn emit_record_field_pattern(
        &mut self,
        field: &RecordFieldPattern<argon_compiler::DefaultExprContext>,
    ) -> Result<tf::RecordFieldPattern, InternalCompilerError> {
        Ok(tf::RecordFieldPattern {
            field_id: BigUint::from(self.get_record_field_id(field.field.clone())),
            pattern: Box::new(self.emit_pattern(&field.pattern)?),
        })
    }

    fn emit_var(
        &mut self,
        variable: &Variable<argon_compiler::DefaultExprContext>,
    ) -> Result<tf::Var, InternalCompilerError> {
        Ok(match variable {
            Variable::Parameter(variable) => tf::Var::ParameterVar {
                owner: Box::new(self.emit_expression_owner(&variable.owner)?),
                parameter_index: BigInt::from(variable.parameter_index),
                name: variable
                    .name
                    .as_ref()
                    .map(|name| encode_identifier(name).map(Box::new))
                    .transpose()?,
                var_type: Box::new(self.emit_expr(&variable.var_type)?),
                erasure: Box::new(encode_erasure_mode(variable.erasure_mode)),
                witness: variable.is_witness,
            },
            Variable::Local(variable) => tf::Var::LocalVar {
                id: self.ids.local_variable_ids.get(variable.id.clone()).into(),
            },
            Variable::InstanceParameter(variable) => tf::Var::InstanceParameterVar {
                owner: Box::new(self.emit_expression_owner(&variable.owner)?),
                name: variable
                    .name
                    .as_ref()
                    .map(|name| encode_identifier(name).map(Box::new))
                    .transpose()?,
                var_type: Box::new(self.emit_expr(&variable.var_type)?),
            },
        })
    }

    fn emit_expression_owner(
        &mut self,
        owner: &ExpressionOwner<argon_compiler::DefaultExprContext>,
    ) -> Result<tf::ExpressionOwner, InternalCompilerError> {
        Ok(match owner {
            ExpressionOwner::Function(function) => tf::ExpressionOwner::Func {
                index: self.get_function_id(function.clone()).into(),
            },
            ExpressionOwner::Record(record) => tf::ExpressionOwner::Rec {
                index: self.get_record_id(record.clone()).into(),
            },
            ExpressionOwner::Enum(enum_) => tf::ExpressionOwner::Enum {
                index: self.get_enum_id(enum_.clone()).into(),
            },
            ExpressionOwner::Trait(_) => todo!("emit trait expression owners"),
            ExpressionOwner::EnumVariant(variant) => tf::ExpressionOwner::EnumVariant {
                index: self.get_enum_variant_id(variant.clone()).into(),
            },
            ExpressionOwner::Method(_) => todo!("emit method expression owners"),
            ExpressionOwner::Instance(_) => todo!("emit instance expression owners"),
        })
    }

    fn emit_local_var_from_variable(
        &mut self,
        variable: &Variable<argon_compiler::DefaultExprContext>,
    ) -> Result<tf::LocalVar, InternalCompilerError> {
        match variable {
            Variable::Local(variable) => self.emit_local_var(variable),
            Variable::Parameter(_) => todo!("Expected local variable"),
            Variable::InstanceParameter(_) => todo!("Expected local variable"),
        }
    }

    fn emit_local_var(
        &mut self,
        variable: &LocalVariable<argon_compiler::DefaultExprContext>,
    ) -> Result<tf::LocalVar, InternalCompilerError> {
        Ok(tf::LocalVar {
            id: self.ids.local_variable_ids.get(variable.id.clone()).into(),
            var_type: Box::new(self.emit_expr(&variable.var_type)?),
            name: variable
                .name
                .as_ref()
                .map(|name| encode_identifier(name).map(Box::new))
                .transpose()?,
            mutable: variable.is_mutable,
            erased: variable.erasure_mode == ErasureMode::Erased,
            witness: variable.is_witness,
        })
    }
}

impl Iterator for TubeEncoder {
    type Item = Result<tf::TubeFileEntry, InternalCompilerError>;

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
}

fn encode_module_path(path: &argon_compiler::ModulePath) -> tf::ModulePath {
    tf::ModulePath {
        path: path.0.clone(),
    }
}

fn encode_tube_name(name: &TubeName) -> tf::TubeName {
    tf::TubeName {
        head: name.0.first().clone(),
        tail: name.0.iter().skip(1).cloned().collect(),
    }
}

fn encode_identifier(id: &Identifier) -> Result<tf::Identifier, InternalCompilerError> {
    Ok(match id {
        Identifier::Named(s) => tf::Identifier::Named { s: s.clone() },
        Identifier::BinaryOp(op) => tf::Identifier::BinOp {
            op: encode_binary_operator(*op),
        },
        Identifier::UnaryOp(op) => tf::Identifier::UnOp {
            op: encode_unary_operator(*op),
        },
        Identifier::Extension(inner) => tf::Identifier::Extension {
            inner: Box::new(encode_identifier(inner)?),
        },
        Identifier::Inverse(inner) => tf::Identifier::Inverse {
            inner: Box::new(encode_identifier(inner)?),
        },
        Identifier::Update(inner) => tf::Identifier::Update {
            inner: Box::new(encode_identifier(inner)?),
        },
    })
}

fn encode_binary_operator(op: BinaryOperatorIdentifier) -> tf::BinaryOperator {
    match op {
        BinaryOperatorIdentifier::Plus => tf::BinaryOperator::Plus,
        BinaryOperatorIdentifier::Minus => tf::BinaryOperator::Minus,
        BinaryOperatorIdentifier::Mul => tf::BinaryOperator::Mul,
        BinaryOperatorIdentifier::Div => tf::BinaryOperator::Div,
        BinaryOperatorIdentifier::Equal => tf::BinaryOperator::Equal,
        BinaryOperatorIdentifier::NotEqual => tf::BinaryOperator::NotEqual,
        BinaryOperatorIdentifier::LessThan => tf::BinaryOperator::LessThan,
        BinaryOperatorIdentifier::LessThanEq => tf::BinaryOperator::LessThanEq,
        BinaryOperatorIdentifier::GreaterThan => tf::BinaryOperator::GreaterThan,
        BinaryOperatorIdentifier::GreaterThanEq => tf::BinaryOperator::GreaterThanEq,
        BinaryOperatorIdentifier::BitOr => tf::BinaryOperator::BitOr,
        BinaryOperatorIdentifier::BitXOr => tf::BinaryOperator::BitXor,
        BinaryOperatorIdentifier::BitAnd => tf::BinaryOperator::BitAnd,
        BinaryOperatorIdentifier::ShiftLeft => tf::BinaryOperator::ShiftLeft,
        BinaryOperatorIdentifier::ShiftRight => tf::BinaryOperator::ShiftRight,
        BinaryOperatorIdentifier::Concat => tf::BinaryOperator::Concat,
    }
}

fn encode_unary_operator(op: UnaryOperatorIdentifier) -> tf::UnaryOperator {
    match op {
        UnaryOperatorIdentifier::Plus => tf::UnaryOperator::Plus,
        UnaryOperatorIdentifier::Minus => tf::UnaryOperator::Minus,
        UnaryOperatorIdentifier::BitNot => tf::UnaryOperator::BitNot,
    }
}

fn encode_access_modifier_global(access: AccessModifierGlobal) -> tf::AccessModifierGlobal {
    match access {
        AccessModifierGlobal::Public => tf::AccessModifierGlobal::Public {},
        AccessModifierGlobal::Internal => tf::AccessModifierGlobal::Internal {},
        AccessModifierGlobal::ModulePrivate => tf::AccessModifierGlobal::ModulePrivate {},
    }
}

fn encode_block_label_kind(kind: BlockLabelKind) -> tf::BlockLabelKind {
    match kind {
        BlockLabelKind::Block => tf::BlockLabelKind::Block,
        BlockLabelKind::Loop => tf::BlockLabelKind::Loop,
        BlockLabelKind::WhileOuter => tf::BlockLabelKind::WhileOuter,
        BlockLabelKind::WhileInner => tf::BlockLabelKind::WhileInner,
    }
}

fn import_specifier_tube(import: &ImportSpecifier) -> &TubeName {
    match import {
        ImportSpecifier::Global { tube, .. } => tube,
        ImportSpecifier::Local { parent, .. } => import_specifier_tube(parent),
    }
}

fn encode_erasure_mode(mode: ErasureMode) -> tf::ErasureMode {
    match mode {
        ErasureMode::Concrete => tf::ErasureMode::Concrete {},
        ErasureMode::Erased => tf::ErasureMode::Erased {},
        ErasureMode::Token => tf::ErasureMode::Token {},
    }
}

fn encode_effect_info(effect: EffectInfo) -> tf::EffectInfo {
    match effect {
        EffectInfo::Pure => tf::EffectInfo::Pure {},
        EffectInfo::Effectful => tf::EffectInfo::Effectful {},
    }
}

fn encode_parameter_list_type(
    list_type: FunctionParameterListType,
) -> tf::FunctionParameterListType {
    match list_type {
        FunctionParameterListType::NormalList => tf::FunctionParameterListType::NormalList,
        FunctionParameterListType::InferrableList => tf::FunctionParameterListType::InferrableList,
        FunctionParameterListType::QuoteList => tf::FunctionParameterListType::QuoteList,
        FunctionParameterListType::RequiresList => tf::FunctionParameterListType::RequiresList,
    }
}

fn encode_builtin(builtin: Builtin) -> Result<tf::Builtin, InternalCompilerError> {
    Ok(match builtin {
        Builtin::IntType => tf::Builtin::IntType,
        Builtin::BoolType => tf::Builtin::BoolType,
        Builtin::StringType => tf::Builtin::StringType,
        Builtin::NeverType => tf::Builtin::NeverType,
        Builtin::ArrayType => tf::Builtin::ArrayType,
        Builtin::ConjunctionType => tf::Builtin::ConjunctionType,
        Builtin::DisjunctionType => tf::Builtin::DisjunctionType,
        Builtin::IntNegate => tf::Builtin::IntNegate,
        Builtin::IntBitNot => tf::Builtin::IntBitNot,
        Builtin::IntAdd => tf::Builtin::IntAdd,
        Builtin::IntSub => tf::Builtin::IntSub,
        Builtin::IntMul => tf::Builtin::IntMul,
        Builtin::IntBitAnd => tf::Builtin::IntBitAnd,
        Builtin::IntBitOr => tf::Builtin::IntBitOr,
        Builtin::IntBitXor => tf::Builtin::IntBitXor,
        Builtin::IntBitShiftLeft => tf::Builtin::IntBitShiftLeft,
        Builtin::IntBitShiftRight => tf::Builtin::IntBitShiftRight,
        Builtin::IntEq => tf::Builtin::IntEq,
        Builtin::IntNe => tf::Builtin::IntNe,
        Builtin::IntLt => tf::Builtin::IntLt,
        Builtin::IntLe => tf::Builtin::IntLe,
        Builtin::IntGt => tf::Builtin::IntGt,
        Builtin::IntGe => tf::Builtin::IntGe,
        Builtin::StringConcat => tf::Builtin::StringConcat,
        Builtin::StringEq => tf::Builtin::StringEq,
        Builtin::StringNe => tf::Builtin::StringNe,
        Builtin::BoolEq => tf::Builtin::BoolEq,
        Builtin::BoolNe => tf::Builtin::BoolNe,
        Builtin::ArrayCreateUnsafeUninitialized => tf::Builtin::ArrayCreateUnsafeUninitialized,
        Builtin::ArrayLength => tf::Builtin::ArrayLength,
        Builtin::ArrayGet => tf::Builtin::ArrayGet,
        Builtin::ArraySet => tf::Builtin::ArraySet,
        Builtin::EqualToType => tf::Builtin::EqualTo,
    })
}

fn encode_erased_builtin_type(builtin: Builtin) -> Option<tf::BuiltinType> {
    Some(match builtin {
        Builtin::IntType => tf::BuiltinType::Int {},
        Builtin::BoolType => tf::BuiltinType::Bool {},
        Builtin::StringType => tf::BuiltinType::String {},
        Builtin::NeverType => tf::BuiltinType::Never {},
        Builtin::ArrayType => tf::BuiltinType::Array {},
        Builtin::ConjunctionType => tf::BuiltinType::Conjunction {},
        Builtin::DisjunctionType => tf::BuiltinType::Disjunction {},
        _ => return None,
    })
}
