use argon_format_vm::vm as vf;
use argon_vm::model::{ModuleExportEntry, ProgramIR, TubeModel};

pub struct Optimizer {
    passes: Vec<&'static dyn crate::pass::OptimizationPass>,
}

impl Optimizer {
    pub fn new() -> Self {
        Self { passes: Vec::new() }
    }

    pub fn add_pass(&mut self, pass: &'static dyn crate::pass::OptimizationPass) {
        self.passes.push(pass);
    }

    pub fn optimize<'a>(
        &self,
        tube: &mut TubeModel,
        referenced_tubes: impl IntoIterator<Item = &'a TubeModel>,
    ) {
        let referenced_tubes = referenced_tubes.into_iter().collect::<Vec<_>>();

        for module_index in 0..tube.modules.len() {
            let export_count = tube.modules[module_index].exports.len();
            for export_index in 0..export_count {
                match &tube.modules[module_index].exports[export_index] {
                    ModuleExportEntry::FunctionDefinition(_) => {
                        self.optimize_location(
                            tube,
                            &referenced_tubes,
                            FunctionLocation::Function {
                                module_index,
                                export_index,
                            },
                        );
                    }
                    ModuleExportEntry::TraitDefinition(definition) => {
                        for method_index in 0..definition.methods.len() {
                            self.optimize_location(
                                tube,
                                &referenced_tubes,
                                FunctionLocation::TraitMethod {
                                    module_index,
                                    export_index,
                                    method_index,
                                },
                            );
                        }
                    }
                    ModuleExportEntry::InstanceDefinition(definition) => {
                        for method_index in 0..definition.methods.len() {
                            self.optimize_location(
                                tube,
                                &referenced_tubes,
                                FunctionLocation::InstanceMethod {
                                    module_index,
                                    export_index,
                                    method_index,
                                },
                            );
                        }
                    }
                    ModuleExportEntry::RecordDefinition(_)
                    | ModuleExportEntry::EnumDefinition(_) => {}
                }
            }
        }
    }

    fn optimize_location(
        &self,
        tube: &mut TubeModel,
        referenced_tubes: &[&TubeModel],
        location: FunctionLocation,
    ) {
        let Some(mut pending) = take_implementation(tube, location) else {
            return;
        };

        {
            let argument_types = pending.argument_types();

            if let vf::FunctionImplementation::VmIr { body } = pending.implementation.as_mut() {
                let program = ProgramIR::new(tube, referenced_tubes.iter().copied());
                self.optimize_function(program, &argument_types, body);
            }
        }

        restore_implementation(tube, location, pending.implementation);
    }

    pub(crate) fn optimize_function<'program>(
        &self,
        program: ProgramIR<'program>,
        argument_types: &[vf::Token],
        function: &mut vf::FunctionBody,
    ) {
        let mut pass_states: Vec<OptimizationPassState> = self
            .passes
            .iter()
            .map(|pass| OptimizationPassState {
                pass: *pass,
                remaining_iterations: pass.max_iterations(),
            })
            .collect();

        let mut opt_state = OptimizationState::new(program, argument_types);
        opt_state.mark_changed();
        while opt_state.changed {
            opt_state.changed = false;

            pass_states.retain_mut(|pass_state| {
                pass_state.pass.optimize(&mut opt_state, function);

                if let Some(remaining_iterations) = &mut pass_state.remaining_iterations {
                    *remaining_iterations = remaining_iterations.saturating_sub(1);
                    *remaining_iterations > 0
                } else {
                    true
                }
            });
        }
    }
}

#[derive(Clone, Copy)]
enum FunctionLocation {
    Function {
        module_index: usize,
        export_index: usize,
    },
    TraitMethod {
        module_index: usize,
        export_index: usize,
        method_index: usize,
    },
    InstanceMethod {
        module_index: usize,
        export_index: usize,
        method_index: usize,
    },
}

struct PendingImplementation {
    is_method: bool,
    signature: vf::FunctionSignature,
    implementation: Box<vf::FunctionImplementation>,
}

impl PendingImplementation {
    fn argument_types(&self) -> Vec<vf::Token> {
        let signature_argument_types = self
            .signature
            .parameters
            .iter()
            .map(|param| param.param_type.as_ref().clone());

        if self.is_method {
            core::iter::once(vf::Token::Boxed {})
                .chain(signature_argument_types)
                .collect()
        } else {
            signature_argument_types.collect()
        }
    }
}

fn take_implementation(
    tube: &mut TubeModel,
    location: FunctionLocation,
) -> Option<PendingImplementation> {
    match location {
        FunctionLocation::Function {
            module_index,
            export_index,
        } => {
            let ModuleExportEntry::FunctionDefinition(definition) =
                &mut tube.modules[module_index].exports[export_index]
            else {
                unreachable!("function location did not point to a function definition");
            };

            Some(PendingImplementation {
                is_method: false,
                signature: (*definition.signature).clone(),
                implementation: definition.implementation.take()?,
            })
        }
        FunctionLocation::TraitMethod {
            module_index,
            export_index,
            method_index,
        } => {
            let ModuleExportEntry::TraitDefinition(definition) =
                &mut tube.modules[module_index].exports[export_index]
            else {
                unreachable!("trait method location did not point to a trait definition");
            };
            let method = &mut definition.methods[method_index];

            Some(PendingImplementation {
                is_method: true,
                signature: (*method.signature).clone(),
                implementation: method.implementation.take()?,
            })
        }
        FunctionLocation::InstanceMethod {
            module_index,
            export_index,
            method_index,
        } => {
            let ModuleExportEntry::InstanceDefinition(definition) =
                &mut tube.modules[module_index].exports[export_index]
            else {
                unreachable!("instance method location did not point to an instance definition");
            };
            let method = &mut definition.methods[method_index];

            Some(PendingImplementation {
                is_method: true,
                signature: (*method.signature).clone(),
                implementation: method.implementation.take()?,
            })
        }
    }
}

fn restore_implementation(
    tube: &mut TubeModel,
    location: FunctionLocation,
    implementation: Box<vf::FunctionImplementation>,
) {
    match location {
        FunctionLocation::Function {
            module_index,
            export_index,
        } => {
            let ModuleExportEntry::FunctionDefinition(definition) =
                &mut tube.modules[module_index].exports[export_index]
            else {
                unreachable!("function location did not point to a function definition");
            };

            debug_assert!(definition.implementation.is_none());
            definition.implementation = Some(implementation);
        }
        FunctionLocation::TraitMethod {
            module_index,
            export_index,
            method_index,
        } => {
            let ModuleExportEntry::TraitDefinition(definition) =
                &mut tube.modules[module_index].exports[export_index]
            else {
                unreachable!("trait method location did not point to a trait definition");
            };

            debug_assert!(definition.methods[method_index].implementation.is_none());
            definition.methods[method_index].implementation = Some(implementation);
        }
        FunctionLocation::InstanceMethod {
            module_index,
            export_index,
            method_index,
        } => {
            let ModuleExportEntry::InstanceDefinition(definition) =
                &mut tube.modules[module_index].exports[export_index]
            else {
                unreachable!("instance method location did not point to an instance definition");
            };

            debug_assert!(definition.methods[method_index].implementation.is_none());
            definition.methods[method_index].implementation = Some(implementation);
        }
    }
}

struct OptimizationPassState {
    pass: &'static dyn crate::pass::OptimizationPass,
    remaining_iterations: Option<usize>,
}

pub struct OptimizationState<'program, 'args> {
    program: ProgramIR<'program>,
    argument_types: &'args [vf::Token],
    changed: bool,
}

impl<'program, 'args> OptimizationState<'program, 'args> {
    pub fn new(program: ProgramIR<'program>, argument_types: &'args [vf::Token]) -> Self {
        Self {
            program,
            argument_types,
            changed: false,
        }
    }

    pub fn without_referenced_tubes(argument_types: &'args [vf::Token]) -> Self {
        Self::new(ProgramIR::default(), argument_types)
    }

    pub fn program(&self) -> &ProgramIR<'program> {
        &self.program
    }

    pub fn program_mut(&mut self) -> &mut ProgramIR<'program> {
        &mut self.program
    }

    pub fn referenced_tube(&self, name: &vf::TubeName) -> Option<&'program TubeModel> {
        self.program.referenced_tube(name)
    }

    pub fn argument_types(&self) -> &'args [vf::Token] {
        self.argument_types
    }

    pub fn changed(&self) -> bool {
        self.changed
    }

    pub fn mark_changed(&mut self) {
        self.changed = true;
    }
}
use alloc::{boxed::Box, vec::Vec};
