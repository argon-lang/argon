use alloc::collections::VecDeque;
use alloc::{boxed::Box, sync::Arc, vec::Vec};
use alloc::borrow::Cow;
use argon_compiler::erased_sig::{ErasedSignature, ErasedSignatureType, ImportSpecifier};
use argon_compiler::{
    BinaryOperatorIdentifier, Builtin, DefaultExprContext, Enum, Function, FunctionImplementation,
    FunctionSignature, Identifier, Instance, Module, ModuleExportBinding, ModuleExportEntry,
    Record, Trait, Tube, TubeName, UnaryOperatorIdentifier,
};
use argon_expr::{
    ErasureMode, Expr, ExprScannerMut, ExpressionOwner, FunctionArgument, Normalizer,
    NormalizerScanner, SubstScanner, Variable,
};
use argon_format::vm as vf;
use argon_util::{Fuel, InternalCompilerError, UniqueIdentifier};
use core::mem;
use esexpr::ESExprStatic;
use hashbrown::{HashMap, HashSet};
use num_bigint::{BigInt, BigUint};

use crate::ids::TubeIdProvider;

pub fn encode_vm_tube(
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
            }

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
                    let signature = self.emit_function_signature(
                        &ExpressionOwner::Function(function.clone()),
                        &function.clone().signature(),
                    )?;
                    let implementation = function
                        .clone()
                        .implementation()
                        .map(|implementation| {
                            self.emit_function_implementation(
                                &implementation,
                                &signature,
                                &import_specifier,
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
            }

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
            }

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
            }

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
            }

            EntryEmitter::Instance(_instance) => {
                todo!("emit VM instance references and definitions")
            }
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
                module_id: self
                    .ids
                    .module_ids
                    .get((tube.clone(), module.clone()))
                    .into(),
                name: Box::new(encode_identifier(name)?),
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
        let mut builder = FunctionSignatureBuilder::new();

        for (index, param) in sig.parameters.iter().enumerate() {
            builder.add_parameter(param.clone().to_parameter_var(owner.clone(), index), false)?;
        }

        builder.finish(self, &sig.return_type)
    }

    fn emit_function_implementation(
        &mut self,
        implementation: &FunctionImplementation,
        signature: &FunctionSignatureWithMapping,
        import_specifier: &ImportSpecifier,
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
        _expr: &Expr<argon_compiler::DefaultExprContext>,
        _signature: &FunctionSignatureWithMapping,
        _import_specifier: &ImportSpecifier,
    ) -> Result<vf::FunctionBody, InternalCompilerError> {
        todo!("lower Argon expressions to VM instructions")
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
    Function(Arc<dyn Function>),
    Record(Arc<dyn Record>),
    Enum(Arc<dyn Enum>),
    Trait(Arc<dyn Trait>),
    Instance(Arc<dyn Instance>),
}

struct FunctionSignatureWithMapping {
    sig: vf::FunctionSignature,
    arg_consumers: Vec<ArgConsumer>,
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
}

impl FunctionSignatureBuilder {
    fn new() -> Self {
        Self {
            token_parameters: Vec::new(),
            parameters: Vec::new(),
            arg_consumers: Vec::new(),
        }
    }

    fn add_parameter(
        &mut self,
        param: argon_expr::ParameterVariable<argon_compiler::DefaultExprContext>,
        _captured_ref: bool,
    ) -> Result<(), InternalCompilerError> {
        use argon_compiler::ErasureMode;

        match param.erasure_mode {
            ErasureMode::Erased => {
                self.arg_consumers.push(ArgConsumer::Erased);
            }
            ErasureMode::Token => {
                self.arg_consumers.push(ArgConsumer::Token);
                todo!("emit VM token signature parameters")
            }
            ErasureMode::Concrete => {
                self.arg_consumers.push(ArgConsumer::Arg);
                todo!("emit VM concrete signature parameters")
            }
        }

        Ok(())
    }

    fn finish(
        self,
        encoder: &mut VmEncoder,
        return_type: &Expr<argon_compiler::DefaultExprContext>,
    ) -> Result<FunctionSignatureWithMapping, InternalCompilerError> {
        let mut token_emitter = TokenEmitter {
            encoder,
            token_params: HashMap::new(),
        };

        Ok(FunctionSignatureWithMapping {
            sig: vf::FunctionSignature {
                token_parameters: self.token_parameters,
                parameters: self.parameters,
                return_type: Box::new(token_emitter.token_expr(return_type)?),
            },
            arg_consumers: self.arg_consumers,
        })
    }
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
    has_branch: bool,
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

    fn with_nested_scope<A>(&mut self, f: impl FnOnce(&mut Self) -> A) -> (vf::Block, A) {
        let mut instructions = mem::take(&mut self.instructions);
        let result = f(self);
        mem::swap(&mut self.instructions, &mut instructions);
        let block = vf::Block {
            instructions,
        };

        (block, result)
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
            _ => todo!("Unimplemented emit expression: {:?}", e)
        })
    }

    fn get_expr_type(&mut self, e: &Expr<DefaultExprContext>) -> Result<Expr<DefaultExprContext>, InternalCompilerError> {
        todo!()
    }

}

#[derive(Clone)]
enum VariableRealization {
    Reg(vf::RegisterId),
    RegRefCell(vf::RegisterId),
    Tok(vf::Token),
}


trait ExprOutput: Sized {
    type ResultType;

    fn into_known_location(self, expr_emitter: &mut ExprEmitter<'_>, e: &Expr<DefaultExprContext>) -> Result<(Self::ResultType, ExprOutputKnown), InternalCompilerError>;
    fn copy_from(self, expr_emitter: &mut ExprEmitter<'_>, r: vf::RegisterId) -> Self::ResultType;
    fn output_register(self, expr_emitter: &mut ExprEmitter<'_>, e: &Expr<DefaultExprContext>) -> Result<OutputRegisterBuilder<Self>, InternalCompilerError>;
    fn output_function_result(self, expr_emitter: &mut ExprEmitter<'_>, e: &Expr<DefaultExprContext>) -> Result<OutputFunctionResultBuilder<Self::ResultType>, InternalCompilerError>;
    // fn output_unit_result(self) -> Self::ResultType;
}

enum ExprOutputKnown {
    Register(vf::RegisterId),
    RefCell(vf::RegisterId),
    Discard,
    Return,
}

impl ExprOutput for ExprOutputKnown {
    type ResultType = ();

    fn into_known_location(self, _expr_emitter: &mut ExprEmitter<'_>, _e: &Expr<DefaultExprContext>) -> Result<(Self::ResultType, ExprOutputKnown), InternalCompilerError> {
        Ok(((), self))
    }

    fn copy_from(self, expr_emitter: &mut ExprEmitter<'_>, r: vf::RegisterId) -> Self::ResultType {
        match self {
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
            }
        }
    }

    fn output_register(self, expr_emitter: &mut ExprEmitter<'_>, e: &Expr<DefaultExprContext>) -> Result<OutputRegisterBuilder<Self>, InternalCompilerError> {
        Ok(match &self {
            ExprOutputKnown::Register(r) => {
                OutputRegisterBuilder {
                    r: r.clone(),
                    output: self,
                }
            }
            _ => {
                let t = expr_emitter.get_expr_type(e)?;
                let t = expr_emitter.token_expr(&t)?;
                let r = expr_emitter.add_var(t);
                OutputRegisterBuilder {
                    r,
                    output: self,
                }
            }
        })
    }

    fn output_function_result(self, expr_emitter: &mut ExprEmitter<'_>, e: &Expr<DefaultExprContext>) -> Result<OutputFunctionResultBuilder<Self::ResultType>, InternalCompilerError> {
        let builder_type = match self {
            ExprOutputKnown::Register(r) => OutputFunctionResultBuilderType::Register(r),
            ExprOutputKnown::RefCell(cell) => {
                let t = expr_emitter.get_expr_type(e)?;
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
}

struct AnyRegister;

impl ExprOutput for AnyRegister {
    type ResultType = vf::RegisterId;

    fn into_known_location(self, expr_emitter: &mut ExprEmitter<'_>, e: &Expr<DefaultExprContext>) -> Result<(Self::ResultType, ExprOutputKnown), InternalCompilerError> {
        let t = expr_emitter.get_expr_type(e)?;
        let t = expr_emitter.token_expr(&t)?;
        let r = expr_emitter.add_var(t);
        Ok((r.clone(), ExprOutputKnown::Register(r)))
    }

    fn copy_from(self, _expr_emitter: &mut ExprEmitter<'_>, r: vf::RegisterId) -> Self::ResultType {
        r
    }

    fn output_register(self, expr_emitter: &mut ExprEmitter<'_>, e: &Expr<DefaultExprContext>) -> Result<OutputRegisterBuilder<Self>, InternalCompilerError> {
        let t = expr_emitter.get_expr_type(e)?;
        let t = expr_emitter.token_expr(&t)?;
        let r = expr_emitter.add_var(t);
        Ok(OutputRegisterBuilder {
            r,
            output: self,
        })
    }


    fn output_function_result(self, expr_emitter: &mut ExprEmitter<'_>, e: &Expr<DefaultExprContext>) -> Result<OutputFunctionResultBuilder<Self::ResultType>, InternalCompilerError> {
        let (result, ko) = self.into_known_location(expr_emitter, e)?;
        let builder = ko.output_function_result(expr_emitter, e)?;
        Ok(OutputFunctionResultBuilder {
            builder_type: builder.builder_type,
            result,
        })
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

    fn into_result(self, expr_emitter: &mut ExprEmitter<'_>) -> O::ResultType {
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
        arguments: &mut Vec<FunctionArgument<DefaultExprContext>>,
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
            let variable = Variable::Parameter(Arc::new(
                parameter
                    .clone()
                    .to_parameter_var(owner.clone(), parameter_index),
            ));

            subst.add_substitution(variable, &argument.arg);
        }

        subst.scan(&mut body);
        Some(body)
    }
}

fn variable_erasure_mode(variable: &Variable<argon_compiler::DefaultExprContext>) -> ErasureMode {
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

fn encode_identifier(id: &Identifier) -> Result<vf::Identifier, InternalCompilerError> {
    Ok(match id {
        Identifier::Named(s) => vf::Identifier::Named { s: s.clone() },
        Identifier::BinaryOp(op) => vf::Identifier::BinOp {
            op: encode_binary_operator(*op),
        },
        Identifier::UnaryOp(op) => vf::Identifier::UnOp {
            op: encode_unary_operator(*op),
        },
        Identifier::Extension(inner) => vf::Identifier::Extension {
            inner: Box::new(encode_identifier(inner)?),
        },
        Identifier::Inverse(inner) => vf::Identifier::Inverse {
            inner: Box::new(encode_identifier(inner)?),
        },
        Identifier::Update(inner) => vf::Identifier::Update {
            inner: Box::new(encode_identifier(inner)?),
        },
    })
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
