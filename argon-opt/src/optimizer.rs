use argon_format_vm::vm as vf;

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

    pub fn optimize(&self, argument_types: &[vf::Token], function: &mut vf::FunctionBody) {
        let mut pass_states: Vec<OptimizationPassState> = self
            .passes
            .iter()
            .map(|pass| OptimizationPassState {
                pass: *pass,
                remaining_iterations: pass.max_iterations(),
            })
            .collect();

        let mut opt_state = OptimizationState::new(argument_types);
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

struct OptimizationPassState {
    pass: &'static dyn crate::pass::OptimizationPass,
    remaining_iterations: Option<usize>,
}

pub struct OptimizationState<'a> {
    argument_types: &'a [vf::Token],
    changed: bool,
}

impl<'a> OptimizationState<'a> {
    pub fn new(argument_types: &'a [vf::Token]) -> Self {
        Self {
            argument_types,
            changed: false,
        }
    }

    pub fn argument_types(&self) -> &'a [vf::Token] {
        self.argument_types
    }

    pub fn changed(&self) -> bool {
        self.changed
    }

    pub fn mark_changed(&mut self) {
        self.changed = true;
    }
}
