use alloc::collections::VecDeque;
use alloc::{boxed::Box, sync::Arc, vec::Vec};
use alloc::borrow::ToOwned;
use argon_compiler::erased_sig::{ErasedSignature, ErasedSignatureType, ImportSpecifier};
use argon_compiler::{BinaryOperatorIdentifier, Builtin, DefaultExprContext, Enum, Function, FunctionImplementation, FunctionSignature, Identifier, Instance, Module, ModuleExportBinding, ModuleExportEntry, ModulePath, Record, Trait, Tube, TubeName, UnaryOperatorIdentifier};
use argon_expr::{ErasureMode, Expr, ExprScannerMut, ExpressionOwner, Normalizer, NormalizerScanner, ParameterVariable, SubstScanner, Variable};
use argon_format::vm as vf;
use argon_util::{Fuel, InternalCompilerError, UniqueIdentifier};
use core::mem;
use alloc::vec;
use esexpr::{ESExprStatic, ESExprCodec};
use esexpr_binary::{ExprGenerator, ExprGeneratorSync, GeneratorError};
use esexpr_binary::io::Write;
use hashbrown::{HashMap, HashSet};
use num_bigint::{BigInt, BigUint};
use num_traits::ToPrimitive;
use argon_compiler::expr_type::get_expr_type;
use crate::ids::TubeIdProvider;

pub fn encode_vm_tube<W: Write<InternalCompilerError>>(
    out: &mut W,
    tube: Arc<Tube>,
    platform_id: impl Into<alloc::string::String>,
) -> Result<(), InternalCompilerError> {
    let mut generator = ExprGenerator::new(out);
    for entry in encode_vm_tube_stream(tube, platform_id) {
        let entry = entry?;

        let expr = entry.encode_esexpr();

        match generator.generate(&expr) {
            Ok(()) => {},
            Err(GeneratorError::IOError(err)) => Err(err)?,
        }
    }
    Ok(())
}

pub fn encode_vm_tube_stream(
    tube: Arc<Tube>,
    platform_id: impl Into<alloc::string::String>,
) -> impl Iterator<Item = Result<vf::TubeFileEntry, InternalCompilerError>> {
    VmEncoder::new(tube, platform_id.into())
}

pub struct VmEncoder {
    tube: Arc<Tube>,
    platform_id: alloc::string::String,

    // Used to ensure that modules are emitted in a consistent order.
    // Will be empty once the metadata is emitted.
    modules: Vec<Arc<Module>>,

    ids: TubeIdProvider,
    entry_emitters: VecDeque<EntryEmitter>,
}

impl VmEncoder {
    pub fn new(tube: Arc<Tube>, platform_id: alloc::string::String) -> Self {
        let mut modules = tube
            .modules()
            .iter()
            .map(|(_, module)| module.clone())
            .collect::<Vec<_>>();

        modules.sort_by(|m1, m2| m1.path().cmp(m2.path()));

        let mut encoder = Self {
            tube,
            platform_id,
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

    fn get_module_id(&mut self, tube: &TubeName, module: &ModulePath) -> usize {
        let (id, is_new) = self.ids.module_ids.get_with_new((tube.clone(), module.clone()));

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

    fn get_record_id(&mut self, record: Arc<dyn Record>) -> usize {
        let (id, is_new) = self.ids.record_ids.get_with_new(record.clone());

        if is_new {
            self.entry_emitters.push_back(EntryEmitter::Record(record));
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

    fn encode_entry(
        &mut self,
        emitter: EntryEmitter,
    ) -> Result<Option<vf::TubeFileEntry>, InternalCompilerError> {
        Ok(Some(match emitter {
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
            },

            EntryEmitter::ModuleReference(tube, module) => {
                let module_id = BigUint::from(self.ids.module_ids.get((tube.clone(), module.clone())));
                let tube_id = BigUint::from(self.ids.tube_ids.get(tube.clone()));
                let path = encode_module_path(&module);

                vf::TubeFileEntry::ModuleReference {
                    module_id,
                    tube_id,
                    path: Box::new(path),
                }
            },

            EntryEmitter::Function(function) => {
                let function_id = BigUint::from(self.ids.function_ids.get(function.clone()));
                let import_specifier = function.clone().import_specifier();
                let import = self.encode_import_specifier(&import_specifier)?;

                if import_specifier_tube(&import_specifier) != self.tube.name() {
                    vf::TubeFileEntry::FunctionReference {
                        function_id,
                        import: Box::new(import),
                    }
                } else {
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

                    vf::TubeFileEntry::FunctionDefinition {
                        definition: Box::new(vf::FunctionDefinition {
                            function_id,
                            import: Box::new(import),
                            signature: Box::new(signature.sig),
                            implementation,
                        }),
                    }
                }
            },

            EntryEmitter::Record(record) => {
                let record_id = BigUint::from(self.ids.record_ids.get(record.clone()));
                let import_specifier = record.clone().import_specifier();
                let import = self.encode_import_specifier(&import_specifier)?;

                if import_specifier_tube(&import_specifier) != self.tube.name() {
                    vf::TubeFileEntry::RecordReference {
                        record_id,
                        import: Box::new(import),
                    }
                } else {
                    todo!("emit VM record definitions")
                }
            },

            EntryEmitter::Enum(enum_) => {
                let enum_id = BigUint::from(self.ids.enum_ids.get(enum_.clone()));
                let import_specifier = enum_.clone().import_specifier();
                let import = self.encode_import_specifier(&import_specifier)?;

                if import_specifier_tube(&import_specifier) != self.tube.name() {
                    vf::TubeFileEntry::EnumReference {
                        enum_id,
                        import: Box::new(import),
                    }
                } else {
                    todo!("emit VM enum definitions")
                }
            },

            EntryEmitter::Trait(trait_) => {
                let trait_id = BigUint::from(self.ids.trait_ids.get(trait_.clone()));
                let import_specifier = trait_.clone().import_specifier();
                let import = self.encode_import_specifier(&import_specifier)?;

                if import_specifier_tube(&import_specifier) != self.tube.name() {
                    vf::TubeFileEntry::TraitReference {
                        trait_id,
                        import: Box::new(import),
                    }
                } else {
                    todo!("emit VM trait definitions")
                }
            },

            EntryEmitter::Instance(_instance) => {
                todo!("emit VM instance references and definitions")
            },
        }))
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
        match exp.binding {
            ModuleExportBinding::Function(function) => {
                self.get_function_id(function);
            }
            ModuleExportBinding::Record(record) => {
                self.get_record_id(record);
            }
            ModuleExportBinding::Enum(enum_) => {
                self.get_enum_id(enum_);
            }
            ModuleExportBinding::Trait(trait_) => {
                self.get_trait_id(trait_);
            }
            ModuleExportBinding::Instance(instance) => {
                self.get_instance_id(instance);
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
                module_id: self.get_module_id(tube, module)
                    .into(),
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
            ErasedSignatureType::Builtin(builtin) => {
                let builtin_type = encode_builtin_type(*builtin);

                vf::ErasedSignatureType::Builtin {
                    b: Box::new(builtin_type),
                    args: Vec::new(),
                }
            }

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
        let mut builder = FunctionSignatureBuilder::new(self);

        for (index, param) in sig.parameters.iter().enumerate() {
            builder.add_parameter(Box::new(param.clone().to_parameter_var(owner.clone(), index)), false)?;
        }

        builder.finish(&sig.return_type)
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

    fn emit_function_body(
        &mut self,
        expr: &Expr<DefaultExprContext>,
        signature: &mut FunctionSignatureWithMapping,
        import_specifier: ImportSpecifier,
    ) -> Result<vf::FunctionBody, InternalCompilerError> {
        let var_offset = (if signature.has_instance_param { 1 } else { 0 }) + signature.sig.parameters.len();

        let mut emitter = ExprEmitter {
            encoder: self,
            var_offset,
            known_vars: mem::take(&mut signature.known_vars),
            declared_vars: Vec::new(),
            instructions: Vec::new(),
            parent_import_specifier: import_specifier,
            captured_vars: HashSet::new(),
            loop_ids: HashMap::new(),
        };

        match emitter.expr_return(expr) {
            Ok(_) | Err(EmitStop::Branch) => {},
            Err(EmitStop::Error(err)) => return Err(err),
        }

        Ok(emitter.into_function_body())
    }

    fn emit_token_expr(
        &mut self,
        expr: &Expr<argon_compiler::DefaultExprContext>,
    ) -> Result<vf::Token, InternalCompilerError> {
        TokenEmitter {
            encoder: self,
            token_params: HashMap::new(),
        }
        .token_expr(expr)
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
    Enum(Arc<dyn Enum>),
    Trait(Arc<dyn Trait>),
    Instance(Arc<dyn Instance>),
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

struct FunctionSignatureBuilder<'a> {
    encoder: &'a mut VmEncoder,

    instance_type_params: HashMap<Variable<DefaultExprContext>, vf::Token>,
    token_parameters: Vec<Box<vf::SignatureTokenParameter>>,
    parameters: Vec<Box<vf::SignatureParameter>>,
    arg_consumers: Vec<ArgConsumer>,

    instance_param: Option<Variable<DefaultExprContext>>,
    type_param_mapping: HashMap<Variable<DefaultExprContext>, usize>,
    param_var_mapping: HashMap<Variable<DefaultExprContext>, MappedParamVar>,
}

impl<'a> FunctionSignatureBuilder<'a> {
    fn new(encoder: &'a mut VmEncoder) -> Self {
        Self {
            encoder,

            instance_type_params: HashMap::new(),
            token_parameters: Vec::new(),
            parameters: Vec::new(),
            arg_consumers: Vec::new(),

            instance_param: None,
            type_param_mapping: HashMap::new(),
            param_var_mapping: HashMap::new(),
        }
    }

    fn token_emitter<'b>(&'b mut self) -> TokenEmitter<'b> {
        let mut token_params = self.instance_type_params.clone();
        token_params.extend(
            self.type_param_mapping
                .iter()
                .map(|(var, index)| (var.clone(), vf::Token::TokenParameter {
                    index: BigUint::from(index.clone()),
                }))
        );

        TokenEmitter {
            encoder: self.encoder,
            token_params,
        }
    }

    fn add_parameter(
        &mut self,
        param: Box<ParameterVariable<DefaultExprContext>>,
        captured_ref: bool,
    ) -> Result<(), InternalCompilerError> {
        match param.erasure_mode {
            ErasureMode::Erased => {
                self.arg_consumers.push(ArgConsumer::Erased);
            }
            ErasureMode::Token => {
                let token_kind = self.token_emitter().token_expr(&param.var_type)?;
                let tp = vf::SignatureTokenParameter {
                    name: param.name.as_ref().map(encode_identifier).map(Box::new),
                    kind: Box::new(token_kind),
                };

                let index = self.token_parameters.len();
                self.token_parameters.push(Box::new(tp));
                self.type_param_mapping.insert(Variable::Parameter(param), index);
                self.arg_consumers.push(ArgConsumer::Token);
            }
            ErasureMode::Concrete => {
                let t = self.token_emitter().token_expr(&param.var_type)?;
                let sig_param = vf::SignatureParameter {
                    name: param.name.as_ref().map(encode_identifier).map(Box::new),
                    param_type: Box::new(t),
                };

                let index = self.parameters.len();
                self.parameters.push(Box::new(sig_param));
                self.param_var_mapping.insert(Variable::Parameter(param), MappedParamVar { index, captured_ref });
                self.arg_consumers.push(ArgConsumer::Arg);
            }
        }

        Ok(())
    }

    fn finish(
        self,
        return_type: &Expr<DefaultExprContext>,
    ) -> Result<FunctionSignatureWithMapping, InternalCompilerError> {
        let mut token_emitter = TokenEmitter {
            encoder: self.encoder,
            token_params: HashMap::new(),
        };


        let reg_offset = if self.instance_param.is_some() { BigUint::from(1u32) } else { BigUint::ZERO };


        let mut known_vars = HashMap::new();

        known_vars.extend(
            self.instance_type_params
                .into_iter()
                .map(|(v, t)| (v, VariableRealization::Tok(t)))
        );

        known_vars.extend(
            self.instance_param
                .as_ref()
                .map(|inst| (inst.clone(), VariableRealization::Reg(vf::RegisterId { id: BigUint::ZERO })))
        );

        known_vars.extend(
            self.param_var_mapping
                .into_iter()
                .map(|(param, mapped_var)| {
                    let r = vf::RegisterId {
                        id: &reg_offset + mapped_var.index
                    };

                    let realization = if mapped_var.captured_ref {
                        VariableRealization::RegRefCell(r)
                    }
                    else {
                        VariableRealization::Reg(r)
                    };

                    (param, realization)
                })
        );

        Ok(FunctionSignatureWithMapping {
            sig: vf::FunctionSignature {
                token_parameters: self.token_parameters,
                parameters: self.parameters,
                return_type: Box::new(token_emitter.token_expr(return_type)?),
            },
            arg_consumers: self.arg_consumers,
            has_instance_param: false,
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
    fn get_parameter_as_token(
        &mut self,
        v: &Variable<DefaultExprContext>,
    ) -> Result<Option<vf::Token>, InternalCompilerError>;

    fn vm_encoder(&mut self) -> &mut VmEncoder;

    fn fallback_token_expr(
        &mut self,
        _t: &Expr<DefaultExprContext>,
    ) -> Result<vf::Token, InternalCompilerError> {
        todo!("emit fallback token expression")
    }

    fn token_expr(
        &mut self,
        t: &Expr<argon_compiler::DefaultExprContext>,
    ) -> Result<vf::Token, InternalCompilerError> {
        let mut t = t.clone();
        NormalizerScanner::new(Fuel::new(5), DefaultExprNormalizer).scan(&mut t);

        match &t {
            Expr::BoxedType { .. } => Ok(vf::Token::Boxed {}),

            Expr::Builtin { builtin, arguments } => {
                let builtin = match encode_token_builtin_type(*builtin) {
                    Some(builtin) => builtin,
                    None => return self.fallback_token_expr(&t),
                };

                Ok(vf::Token::Builtin {
                    b: Box::new(builtin),
                    args: self.token_exprs(arguments)?,
                })
            }

            Expr::FunctionType { a, r } => Ok(vf::Token::Function {
                input: Box::new(self.token_expr(a)?),
                output: Box::new(self.token_expr(r)?),
            }),

            Expr::RecordType(record, arguments) => Ok(vf::Token::Record {
                record_id: BigUint::from(self.vm_encoder().get_record_id(record.clone())),
                args: self.token_exprs(arguments)?,
            }),

            Expr::EnumType(enum_, arguments) => Ok(vf::Token::Enum {
                enum_id: BigUint::from(self.vm_encoder().get_enum_id(enum_.clone())),
                args: self.token_exprs(arguments)?,
            }),

            Expr::TraitType(trait_, arguments) => Ok(vf::Token::Trait {
                trait_id: BigUint::from(self.vm_encoder().get_trait_id(trait_.clone())),
                args: self.token_exprs(arguments)?,
            }),

            Expr::Tuple { items } => Ok(vf::Token::Tuple {
                elements: self.token_exprs(items)?,
            }),

            Expr::Type(_) | Expr::BigType(_) => Ok(vf::Token::TypeInfo {}),

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

    fn token_exprs<'a>(
        &mut self,
        exprs: impl IntoIterator<Item = &'a Expr<argon_compiler::DefaultExprContext>>,
    ) -> Result<Vec<Box<vf::Token>>, InternalCompilerError> {
        exprs
            .into_iter()
            .map(|expr| self.token_expr(expr).map(Box::new))
            .collect()
    }
}

struct TokenEmitter<'a> {
    encoder: &'a mut VmEncoder,
    token_params: HashMap<Variable<argon_compiler::DefaultExprContext>, vf::Token>,
}

impl TokenEmitterCommon for TokenEmitter<'_> {
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
    var_offset: usize,
    known_vars: HashMap<Variable<DefaultExprContext>, VariableRealization>,
    declared_vars: Vec<Box<vf::VariableDeclaration>>,
    instructions: Vec<Box<vf::Instruction>>,
    parent_import_specifier: ImportSpecifier,
    captured_vars: HashSet<Variable<DefaultExprContext>>,
    loop_ids: HashMap<UniqueIdentifier, usize>,
}

impl TokenEmitterCommon for ExprEmitter<'_> {
    fn get_parameter_as_token(
        &mut self,
        _v: &Variable<DefaultExprContext>,
    ) -> Result<Option<vf::Token>, InternalCompilerError> {
        todo!("get token parameter from expression emitter")
    }

    fn vm_encoder(&mut self) -> &mut VmEncoder {
        self.encoder
    }
}

impl <'a> ExprEmitter<'a> {

    fn with_nested_block<A>(&mut self, f: impl FnOnce(&mut Self) -> EmitResult<A>) -> Result<(vf::Block, EmitResult<A>), InternalCompilerError> {
        let mut instructions = mem::take(&mut self.instructions);
        let result = match f(self) {
            Ok(result) => Ok(result),
            Err(EmitStop::Error(err)) => return Err(err),
            Err(EmitStop::Branch) => Err(EmitStop::Branch),
        };
        mem::swap(&mut self.instructions, &mut instructions);
        let block = vf::Block {
            instructions,
        };

        Ok((block, result))
    }

    fn declare_var(&mut self, v: Variable<DefaultExprContext>) -> Result<vf::RegisterId, InternalCompilerError> {
        let t = self.token_expr(v.var_type())?;

        if self.captured_vars.contains(&v) && v.is_mutable() {
            let id = self.add_var(vf::Token::RefCell { inner: Box::new(t) });
            self.known_vars.insert(v, VariableRealization::RegRefCell(id.clone()));
            Ok(id)
        }
        else {
            let id = self.add_var(t);
            self.known_vars.insert(v, VariableRealization::Reg(id.clone()));
            Ok(id)
        }
    }

    fn add_var(&mut self, t: vf::Token) -> vf::RegisterId {
        let id = BigUint::from(self.var_offset) + self.declared_vars.len();
        self.declared_vars.push(Box::new(vf::VariableDeclaration { r#type: Box::new(t) }));
        vf::RegisterId { id }
    }

    fn get_loop_id(&mut self, loop_id: UniqueIdentifier) -> BigUint {
        BigUint::from(self.loop_ids.get(&loop_id).cloned().unwrap_or_else(|| {
            let id = self.loop_ids.len();
            self.loop_ids.insert(loop_id, id);
            id
        }))
    }

    fn into_function_body(self) -> vf::FunctionBody {
        vf::FunctionBody {
            variables: Box::new(vf::VariableDeclarations {
                variables: self.declared_vars,
            }),
            block: Box::new(vf::Block {
                instructions: self.instructions,
            }),
        }
    }

    fn emit(&mut self, insn: vf::Instruction) {
        self.instructions.push(Box::new(insn));
    }

    fn expr<O: ExprOutput>(&mut self, e: &Expr<DefaultExprContext>, output: O) -> EmitResult<O::ResultType> {
        Ok(match e {
            Expr::And(a, b) => {
                let rb = output.output_register(self, e)?;
                self.expr(a, ExprOutputKnown::Register(rb.register().clone()))?;

                // ignore branching since it won't branch when a is false
                let (block, _) = self.with_nested_block(|emitter| {
                    emitter.expr(b, ExprOutputKnown::Register(rb.register().clone()))?;
                    Ok(())
                })?;

                self.emit(vf::Instruction::IfElse {
                    condition: Box::new(rb.register().clone()),
                    when_true: Box::new(block),
                    when_false: Box::new(vf::Block { instructions: vec![] }),
                });

                rb.into_result(self)?
            },

            Expr::BoolLiteral(b) => {
                let rb = output.output_register(self, e)?;
                self.emit(vf::Instruction::ConstBool {
                    dest: Box::new(rb.register().clone()),
                    value: *b,
                });

                rb.into_result(self)?
            },

            // Box
            // Break

            Expr::Builtin { builtin, arguments } => {

                fn emit_op<O: ExprOutput>(emitter: &mut ExprEmitter<'_>, e: &Expr<DefaultExprContext>, builtin: vf::BuiltinOp, arguments: &[Expr<DefaultExprContext>], output: O) -> EmitResult<O::ResultType> {
                    let rb = output.output_register(emitter, e)?;

                    let mut registers = Vec::with_capacity(arguments.len() + 1);
                    registers.push(Box::new(rb.register().clone()));

                    for e in arguments {
                        let r = emitter.expr(e, AnyRegister)?;
                        registers.push(Box::new(r));
                    }

                    emitter.emit(vf::Instruction::Builtin {
                        op: builtin,
                        tokens: vec![],
                        registers,
                    });

                    rb.into_result(emitter)
                }

                fn emit_parameterized_op<O: ExprOutput>(emitter: &mut ExprEmitter<'_>, e: &Expr<DefaultExprContext>, builtin: vf::BuiltinOp, arguments: &[Expr<DefaultExprContext>], output: O) -> EmitResult<O::ResultType> {
                    let rb = output.output_register(emitter, e)?;

                    let mut registers = Vec::with_capacity(arguments.len());
                    registers.push(Box::new(rb.register().clone()));

                    let Some((type_arg, arguments)) = arguments.split_first() else {
                        todo!("return a proper error")
                    };

                    let t = emitter.token_expr(type_arg)?;

                    for e in arguments {
                        let r = emitter.expr(e, AnyRegister)?;
                        registers.push(Box::new(r));
                    }

                    emitter.emit(vf::Instruction::Builtin {
                        op: builtin,
                        tokens: vec![Box::new(t)],
                        registers,
                    });

                    rb.into_result(emitter)
                }

                fn emit_parameterized_void_op<O: ExprOutput>(emitter: &mut ExprEmitter<'_>, builtin: vf::BuiltinOp, arguments: &[Expr<DefaultExprContext>], output: O) -> EmitResult<O::ResultType> {
                    let Some((type_arg, arguments)) = arguments.split_first() else {
                        todo!("return a proper error")
                    };

                    let mut registers = Vec::with_capacity(arguments.len());

                    let t = emitter.token_expr(type_arg)?;

                    for e in arguments {
                        let r = emitter.expr(e, AnyRegister)?;
                        registers.push(Box::new(r));
                    }

                    emitter.emit(vf::Instruction::Builtin {
                        op: builtin,
                        tokens: vec![Box::new(t)],
                        registers,
                    });

                    output.output_unit_result(emitter)
                }


                macro_rules! op {
                    ($name: ident) => {
                        emit_op(self, e, vf::BuiltinOp::$name, arguments, output)?
                    };
                }

                macro_rules! parameterized_op {
                    ($name: ident) => {
                        emit_parameterized_op(self, e, vf::BuiltinOp::$name, arguments, output)?
                    };
                }

                macro_rules! parameterized_void_op {
                    ($name: ident) => {
                        emit_parameterized_void_op(self, vf::BuiltinOp::$name, arguments, output)?
                    };
                }

                match builtin {
                    Builtin::IntType
                    | Builtin::BoolType
                    | Builtin::StringType
                    | Builtin::NeverType
                    | Builtin::ArrayType
                    | Builtin::ConjunctionType
                    | Builtin::DisjunctionType
                    | Builtin::EqualToType => {
                        let t = self.token_expr(e)?;
                        let rb = output.output_register(self, e)?;
                        self.emit(vf::Instruction::LoadToken {
                            dest: Box::new(rb.register().clone()),
                            token: Box::new(t),
                        });
                        rb.into_result(self)?
                    }

                    Builtin::IntNegate => op!(IntNegate),
                    Builtin::IntBitNot => op!(IntBitNot),
                    Builtin::IntAdd => op!(IntAdd),
                    Builtin::IntSub => op!(IntSub),
                    Builtin::IntMul => op!(IntMul),
                    Builtin::IntBitAnd => op!(IntBitAnd),
                    Builtin::IntBitOr => op!(IntBitOr),
                    Builtin::IntBitXor => op!(IntBitXor),
                    Builtin::IntBitShiftLeft => op!(IntBitShiftLeft),
                    Builtin::IntBitShiftRight => op!(IntBitShiftRight),
                    Builtin::IntEq => op!(IntEq),
                    Builtin::IntNe => op!(IntNe),
                    Builtin::IntLt => op!(IntLt),
                    Builtin::IntLe => op!(IntLe),
                    Builtin::IntGt => op!(IntGt),
                    Builtin::IntGe => op!(IntGe),
                    Builtin::StringConcat => op!(StringConcat),
                    Builtin::StringEq => op!(StringEq),
                    Builtin::StringNe => op!(StringNe),
                    Builtin::BoolNot => op!(BoolNot),
                    Builtin::BoolEq => op!(BoolEq),
                    Builtin::BoolNe => op!(BoolNe),
                    Builtin::ArrayCreateUnsafeUninitialized => parameterized_op!(ArrayCreateUnsafeUninitialized),
                    Builtin::ArrayLength => parameterized_op!(ArrayLength),
                    Builtin::ArrayGet => parameterized_op!(ArrayGet),
                    Builtin::ArraySet => parameterized_void_op!(ArraySet),
                }
            },

            // EnumVariantLiteral

            Expr::Finally { block_body, finally_body } => {
                let (block_body, block_result) = self.with_nested_block(|emitter|
                    emitter.expr(block_body, output)
                )?;

                let (finally_body, finally_result) = self.with_nested_block(|emitter|
                    emitter.expr(finally_body, ExprOutputKnown::Discard)
                )?;

                self.emit(vf::Instruction::Finally {
                    action: Box::new(block_body),
                    ensuring: Box::new(finally_body),
                });

                let value = block_result?;
                finally_result?;
                value
            },

            Expr::FunctionCall { function, arguments } => {
                let frb = output.output_function_result(self, e)?;
                let id = self.encoder.get_function_id(function.clone());

                let sig = function.clone().signature();
                let func_args = self.emit_arguments(ExpressionOwner::Function(function.clone()), sig, arguments)?;
                self.emit(vf::Instruction::FunctionCall {
                    function_id: BigUint::from(id),
                    dest: Box::new(frb.function_result()),
                    token_args: func_args.token_arguments,
                    args: func_args.arguments,
                });

                frb.into_result(self)?
            },

            // FunctionObjectCall

            Expr::IfElse { condition, when_true, when_false, .. } => {
                let (result, output) = output.into_known_location(self, e)?;

                let cond = self.expr(condition, AnyRegister)?;

                let (when_true, true_result) = self.with_nested_block(|emitter|
                    emitter.expr(when_true, output.clone())
                )?;

                let (when_false, false_result) = self.with_nested_block(|emitter|
                    emitter.expr(when_false, output)
                )?;

                self.emit(vf::Instruction::IfElse {
                    condition: Box::new(cond),
                    when_true: Box::new(when_true),
                    when_false: Box::new(when_false),
                });

                match (true_result, false_result) {
                    (Err(EmitStop::Branch), Err(EmitStop::Branch)) => Err(EmitStop::Branch)?,
                    _ => result,
                }
            },

            // InstanceMethodCall

            Expr::IntLiteral(i) => {
                let rb = output.output_register(self, e)?;
                self.emit(vf::Instruction::ConstInt {
                    dest: Box::new(rb.register().clone()),
                    value: i.clone(),
                });

                rb.into_result(self)?
            },

            // Is
            // Lambda
            // Loop
            // Match
            // NewInstance
            // Next

            Expr::Or(a, b) => {
                let rb = output.output_register(self, e)?;
                self.expr(a, ExprOutputKnown::Register(rb.register().clone()))?;

                // ignore branching since it won't branch when a is true
                let (block, _) = self.with_nested_block(|emitter| {
                    emitter.expr(b, ExprOutputKnown::Register(rb.register().clone()))?;
                    Ok(())
                })?;

                self.emit(vf::Instruction::IfElse {
                    condition: Box::new(rb.register().clone()),
                    when_true: Box::new(vf::Block { instructions: vec![] }),
                    when_false: Box::new(block),
                });

                rb.into_result(self)?
            },

            // Raise
            // RecordType
            // EnumType
            // RefCellType
            // TraitType
            // RecordFieldLoad
            // RecordFieldStore
            // RecordLiteral
            // Redo
            // RefCellCreate

            Expr::Sequence(items) => {
                let mut item = items.first();
                for next_item in items.iter().skip(1) {
                    self.expr(item, ExprOutputKnown::Discard)?;
                    item = next_item;
                }

                self.expr(item, output)?
            },

            Expr::StringLiteral(s) => {
                let rb = output.output_register(self, e)?;

                self.emit(vf::Instruction::ConstString {
                    dest: Box::new(rb.register().clone()),
                    value: s.as_ref().to_owned(),
                });
                rb.into_result(self)?
            },

            Expr::Tuple { items } => {
                if let Some(result) = output.try_discard() {
                    for item in items {
                        self.expr(item, ExprOutputKnown::Discard)?;
                    }

                    result
                }
                else {
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
            },

            Expr::TupleElement(tuple, index) => {
                let rb = output.output_register(self, e)?;

                let tuple_reg = self.expr(tuple, AnyRegister)?;

                self.emit(vf::Instruction::TupleElement {
                    dest: Box::new(rb.register().clone()),
                    element_index: BigUint::from(*index),
                    src: Box::new(tuple_reg),
                });

                rb.into_result(self)?
            },

            // Unbox

            Expr::Variable(v) => {
                let Some(realization) = self.known_vars.get(v) else {
                    todo!("return a proper error")
                };

                match realization {
                    VariableRealization::Reg(reg) => {
                        output.copy_from(self, reg.clone())?
                    },
                    VariableRealization::RegRefCell(reg) => {
                        let reg = reg.clone();

                        let rb = output.output_register(self, e)?;
                        self.emit(vf::Instruction::LoadReference {
                            dest: Box::new(rb.register().clone()),
                            r#ref: Box::new(reg),
                        });

                        rb.into_result(self)?
                    },
                    VariableRealization::Tok(tok) => {
                        let tok = tok.clone();

                        let rb = output.output_register(self, e)?;
                        self.emit(vf::Instruction::LoadToken {
                            dest: Box::new(rb.register().clone()),
                            token: Box::new(tok),
                        });

                        rb.into_result(self)?
                    },
                }
            },

            Expr::VariableBinding(v, value) => {
                let r = self.declare_var(v.clone())?;
                if self.captured_vars.contains(v) && v.is_mutable() {
                    let value_reg = self.expr(value, AnyRegister)?;
                    self.emit(vf::Instruction::NewReference {
                        dest: Box::new(r),
                        value: Box::new(value_reg),
                    });
                }
                else {
                    self.expr(value, ExprOutputKnown::Register(r))?;
                }

                output.output_unit_result(self)?
            }

            Expr::VariableStore(v, value) => {
                let Some(realization) = self.known_vars.get(v) else {
                    todo!("return a proper error")
                };

                match realization {
                    VariableRealization::Reg(reg) => {
                        self.expr(value, ExprOutputKnown::Register(reg.clone()))?;
                    },
                    VariableRealization::RegRefCell(reg) => {
                        self.expr(value, ExprOutputKnown::RefCell(reg.clone()))?;
                    },
                    VariableRealization::Tok(tok) => {
                        todo!("return a proper error")
                    },
                }

                output.output_unit_result(self)?
            },

            _ => todo!("Unimplemented emit expression: {:?}", e)
        })
    }

    fn emit_arguments(&mut self, owner: ExpressionOwner<DefaultExprContext>, sig: Arc<FunctionSignature<DefaultExprContext>>, args: &[Expr<DefaultExprContext>]) -> EmitResult<FunctionArguments> {
        let sig_with_mapping = self.encoder.emit_function_signature(&owner, &sig)?;
        self.emit_arguments_common(sig_with_mapping, args)
    }

    fn emit_arguments_common(&mut self, sig: FunctionSignatureWithMapping, args: &[Expr<DefaultExprContext>]) -> EmitResult<FunctionArguments> {
        let mut arguments = Vec::with_capacity(args.len());
        let mut token_arguments = Vec::with_capacity(args.len());

        for (consumer, arg) in sig.arg_consumers.iter().zip(args) {
            match consumer {
                ArgConsumer::Erased => {},
                ArgConsumer::Token => {
                    token_arguments.push(Box::new(self.token_expr(arg)?));
                },
                ArgConsumer::Arg => {
                    arguments.push(Box::new(self.expr(arg, AnyRegister)?));
                },
            }
        }

        Ok(FunctionArguments { token_arguments, arguments })
    }

    fn expr_return(&mut self, e: &Expr<DefaultExprContext>) -> EmitResult<()> {
        self.expr(e, ExprOutputKnown::Return)
    }
}

#[derive(Debug, Clone)]
enum VariableRealization {
    Reg(vf::RegisterId),
    RegRefCell(vf::RegisterId),
    Tok(vf::Token),
}

struct FunctionArguments {
    token_arguments: Vec<Box<vf::Token>>,
    arguments: Vec<Box<vf::RegisterId>>,
}

trait ExprOutput: Sized {
    type ResultType;

    fn into_known_location(self, expr_emitter: &mut ExprEmitter<'_>, e: &Expr<DefaultExprContext>) -> EmitResult<(Self::ResultType, ExprOutputKnown)>;
    fn copy_from(self, expr_emitter: &mut ExprEmitter<'_>, r: vf::RegisterId) -> EmitResult<Self::ResultType>;
    fn output_register(self, expr_emitter: &mut ExprEmitter<'_>, e: &Expr<DefaultExprContext>) -> EmitResult<OutputRegisterBuilder<Self>>;
    fn output_function_result(self, expr_emitter: &mut ExprEmitter<'_>, e: &Expr<DefaultExprContext>) -> EmitResult<OutputFunctionResultBuilder<Self::ResultType>>;
    fn output_unit_result(self, expr_emitter: &mut ExprEmitter<'_>) -> EmitResult<Self::ResultType>;
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

    fn into_known_location(self, _expr_emitter: &mut ExprEmitter<'_>, _e: &Expr<DefaultExprContext>) -> EmitResult<(Self::ResultType, ExprOutputKnown)> {
        Ok(((), self))
    }

    fn copy_from(self, expr_emitter: &mut ExprEmitter<'_>, r: vf::RegisterId) -> EmitResult<Self::ResultType> {
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
                expr_emitter.emit(vf::Instruction::Return {
                    src: Box::new(r),
                });
                Err(EmitStop::Branch)?
            }
        })
    }

    fn output_register(self, expr_emitter: &mut ExprEmitter<'_>, e: &Expr<DefaultExprContext>) -> EmitResult<OutputRegisterBuilder<Self>> {
        Ok(match &self {
            ExprOutputKnown::Register(r) => {
                OutputRegisterBuilder {
                    r: r.clone(),
                    output: self,
                }
            }
            _ => {
                let t = get_expr_type(e);
                let t = expr_emitter.token_expr(&t)?;
                let r = expr_emitter.add_var(t);
                OutputRegisterBuilder {
                    r,
                    output: self,
                }
            }
        })
    }

    fn output_function_result(self, expr_emitter: &mut ExprEmitter<'_>, e: &Expr<DefaultExprContext>) -> EmitResult<OutputFunctionResultBuilder<Self::ResultType>> {
        let builder_type = match self {
            ExprOutputKnown::Register(r) => OutputFunctionResultBuilderType::Register(r),
            ExprOutputKnown::RefCell(cell) => {
                let t = get_expr_type(e);
                let t = expr_emitter.token_expr(&t)?;
                let r = expr_emitter.add_var(t);
                OutputFunctionResultBuilderType::RefCell {
                    cell,
                    function_output: r,
                }
            },
            ExprOutputKnown::Discard => OutputFunctionResultBuilderType::Discard,
            ExprOutputKnown::Return => OutputFunctionResultBuilderType::Return,
        };

        Ok(OutputFunctionResultBuilder {
            builder_type,
            result: (),
        })
    }

    fn output_unit_result(self, expr_emitter: &mut ExprEmitter<'_>) -> EmitResult<Self::ResultType> {
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

struct AnyRegister;

impl ExprOutput for AnyRegister {
    type ResultType = vf::RegisterId;

    fn into_known_location(self, expr_emitter: &mut ExprEmitter<'_>, e: &Expr<DefaultExprContext>) -> EmitResult<(Self::ResultType, ExprOutputKnown)> {
        let t = get_expr_type(e);
        let t = expr_emitter.token_expr(&t)?;
        let r = expr_emitter.add_var(t);
        Ok((r.clone(), ExprOutputKnown::Register(r)))
    }

    fn copy_from(self, expr_emitter: &mut ExprEmitter<'_>, r: vf::RegisterId) -> EmitResult<Self::ResultType> {
        if &r.id < &BigUint::from(expr_emitter.var_offset) {
            // Skip copy because it is a parameter.
            Ok(r)
        }
        else {
            let v = &expr_emitter.declared_vars[(&r.id - expr_emitter.var_offset).to_usize().expect("register id is not a usize")];
            let t = (*v.r#type).clone();

            let dest = expr_emitter.add_var(t);
            expr_emitter.emit(vf::Instruction::Move {
                dest: Box::new(dest.clone()),
                src: Box::new(r),
            });
            Ok(dest)
        }

    }

    fn output_register(self, expr_emitter: &mut ExprEmitter<'_>, e: &Expr<DefaultExprContext>) -> EmitResult<OutputRegisterBuilder<Self>> {
        let t = get_expr_type(e);
        let t = expr_emitter.token_expr(&t)?;
        let r = expr_emitter.add_var(t);
        Ok(OutputRegisterBuilder {
            r,
            output: self,
        })
    }


    fn output_function_result(self, expr_emitter: &mut ExprEmitter<'_>, e: &Expr<DefaultExprContext>) -> EmitResult<OutputFunctionResultBuilder<Self::ResultType>> {
        let (result, ko) = self.into_known_location(expr_emitter, e)?;
        let builder = ko.output_function_result(expr_emitter, e)?;
        Ok(OutputFunctionResultBuilder {
            builder_type: builder.builder_type,
            result,
        })
    }

    fn output_unit_result(self, expr_emitter: &mut ExprEmitter<'_>) -> EmitResult<Self::ResultType> {
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

impl <O: ExprOutput> OutputRegisterBuilder<O> {
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

impl <R> OutputFunctionResultBuilder<R> {
    fn function_result(&self) -> vf::FunctionResult {
        match &self.builder_type {
            OutputFunctionResultBuilderType::Register(r) =>
                vf::FunctionResult::Register { id: Box::new(r.clone()) },
            OutputFunctionResultBuilderType::RefCell { function_output, .. } =>
                vf::FunctionResult::Register { id: Box::new(function_output.clone()) },
            OutputFunctionResultBuilderType::Discard => vf::FunctionResult::Discard {},
            OutputFunctionResultBuilderType::Return => vf::FunctionResult::ReturnValue {},
        }
    }

    fn into_result(self, expr_emitter: &mut ExprEmitter<'_>) -> Result<R, InternalCompilerError> {
        match self.builder_type {
            OutputFunctionResultBuilderType::RefCell { cell, function_output } => {
                expr_emitter.emit(vf::Instruction::UpdateReference {
                    r#ref: Box::new(cell),
                    value: Box::new(function_output),
                });
            },

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
}



struct DefaultExprNormalizer;

impl Normalizer<DefaultExprContext> for DefaultExprNormalizer {
    fn get_function_body(
        &mut self,
        function: &Arc<dyn Function>,
        arguments: &mut Vec<Expr<DefaultExprContext>>,
    ) -> Option<Expr<DefaultExprContext>> {
        if !function.metadata().is_inline {
            return None;
        }

        let implementation = function.clone().implementation()?;
        let FunctionImplementation::Expr(body) = implementation.as_ref() else {
            return None;
        };

        let signature = function.clone().signature();
        let owner = ExpressionOwner::Function(function.clone());
        let mut body = body.clone();
        let arguments = mem::take(arguments);
        let mut subst = SubstScanner::new();

        for ((parameter_index, parameter), argument) in
            signature.parameters.iter().enumerate().zip(&arguments)
        {
            let variable = Variable::Parameter(Box::new(
                parameter
                    .clone()
                    .to_parameter_var(owner.clone(), parameter_index),
            ));

            subst.add_substitution(variable, argument);
        }

        subst.scan(&mut body);
        Some(body)
    }
}

fn variable_erasure_mode(variable: &Variable<DefaultExprContext>) -> ErasureMode {
    match variable {
        Variable::Local(variable) => variable.erasure_mode,
        Variable::Parameter(variable) => variable.erasure_mode,
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
        UnaryOperatorIdentifier::LogicalNot => vf::UnaryOperator::LogicalNot,
    }
}

fn import_specifier_tube(import: &ImportSpecifier) -> &TubeName {
    match import {
        ImportSpecifier::Global { tube, .. } => tube,
        ImportSpecifier::Local { parent, .. } => import_specifier_tube(parent),
    }
}

fn encode_builtin_type(builtin: Builtin) -> vf::BuiltinType {
    encode_token_builtin_type(builtin)
        .unwrap_or_else(|| todo!("encode non-type builtin as VM builtin type"))
}

fn encode_token_builtin_type(builtin: Builtin) -> Option<vf::BuiltinType> {
    match builtin {
        Builtin::IntType => Some(vf::BuiltinType::Int {}),
        Builtin::BoolType => Some(vf::BuiltinType::Bool {}),
        Builtin::StringType => Some(vf::BuiltinType::String {}),
        Builtin::NeverType => Some(vf::BuiltinType::Never {}),
        Builtin::ArrayType => Some(vf::BuiltinType::Array {}),
        Builtin::ConjunctionType => Some(vf::BuiltinType::Conjunction {}),
        Builtin::DisjunctionType => Some(vf::BuiltinType::Disjunction {}),
        _ => None,
    }
}
