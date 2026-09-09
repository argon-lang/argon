use argon_format_vm::vm as vf;
use argon_vm::analysis;
use argon_vm::model::{
    ModuleExportEntry, TubeModel, next_available_block_id, prepare_function_body_for_inline,
};
use num_bigint::BigUint;

use super::OptimizationPass;
use crate::optimizer::OptimizationState;

const MAX_INLINE_ITERATIONS: usize = 4;
const MAX_IMPLICIT_INLINE_INSTRUCTIONS: usize = 6;
const MAX_DECLARED_INLINE_INSTRUCTIONS: usize = 48;
const MAX_INLINE_VARIABLES: usize = 24;

#[derive(Debug, Default)]
pub struct FunctionInlining;

pub static FUNCTION_INLINING: FunctionInlining = FunctionInlining;

impl OptimizationPass for FunctionInlining {
    fn name(&self) -> &'static str {
        "function-inlining"
    }

    fn max_iterations(&self) -> Option<usize> {
        Some(MAX_INLINE_ITERATIONS)
    }

    fn optimize(&self, state: &mut OptimizationState, function: &mut vf::FunctionBody) {
        let first_block_id = next_available_block_id(&function.region);
        let vf::FunctionBody { variables, region } = function;
        if inline_region(state, variables, region, first_block_id) {
            state.mark_changed();
        }
    }
}

fn inline_region(
    state: &mut OptimizationState,
    variables: &mut vf::VariableDeclarations,
    region: &mut Box<vf::Region>,
    first_block_id: BigUint,
) -> bool {
    let replacement = match region.as_mut() {
        vf::Region::BasicBlock { instructions } => {
            inline_basic_block(state, variables, instructions, first_block_id)
        }
        vf::Region::Sequence { regions } => {
            return regions
                .iter_mut()
                .any(|region| inline_region(state, variables, region, first_block_id.clone()));
        }
        vf::Region::Block { region, .. } => {
            return inline_region(state, variables, region, first_block_id);
        }
        vf::Region::IfElse {
            condition,
            when_true,
            when_false,
            ..
        } => {
            return inline_region(state, variables, condition, first_block_id.clone())
                || inline_region(state, variables, when_true, first_block_id.clone())
                || inline_region(state, variables, when_false, first_block_id);
        }
        vf::Region::Finally { action, ensuring } => {
            return inline_region(state, variables, action, first_block_id.clone())
                || inline_region(state, variables, ensuring, first_block_id);
        }
    };

    if let Some(replacement) = replacement {
        *region = Box::new(replacement);
        true
    } else {
        false
    }
}

fn inline_basic_block(
    state: &mut OptimizationState,
    variables: &mut vf::VariableDeclarations,
    instructions: &[Box<vf::Instruction>],
    first_block_id: BigUint,
) -> Option<vf::Region> {
    for (index, instruction) in instructions.iter().enumerate() {
        let Some(replacement) = inline_instruction(
            state,
            variables,
            instruction.as_ref(),
            first_block_id.clone(),
        ) else {
            continue;
        };
        let before = basic_block(instructions[..index].to_vec());
        let after = basic_block(instructions[index + 1..].to_vec());

        return Some(sequence([before, Box::new(replacement), after]));
    }

    None
}

fn inline_instruction(
    state: &mut OptimizationState,
    variables: &mut vf::VariableDeclarations,
    instruction: &vf::Instruction,
    first_block_id: BigUint,
) -> Option<vf::Region> {
    let vf::Instruction::FunctionCall {
        function_id,
        token_args,
        args,
        dest,
    } = instruction
    else {
        return None;
    };

    let candidate = find_inline_candidate(state, function_id)?;
    let current_tube = state.program_mut().current_tube_mut()?;
    let body = candidate.convert_body_to_current_tube(current_tube)?;
    let argument_types = candidate.convert_argument_types_to_current_tube(current_tube)?;

    if args.len() != argument_types.len() {
        return None;
    }

    let return_register = match dest.as_ref() {
        vf::FunctionResult::Register { id } => Some(id.as_ref()),
        vf::FunctionResult::ReturnValue {} => None,
        vf::FunctionResult::Discard {} => return None,
    };

    let next_register = next_register(state, variables);
    let prepared = prepare_function_body_for_inline(
        &body,
        &argument_types,
        token_args,
        next_register,
        first_block_id,
        return_register,
    )?;
    let argument_registers = prepared.argument_registers.clone();
    let argument_moves = argument_moves(args, &argument_registers)?;

    variables
        .variables
        .extend(prepared.body.variables.variables);

    let mut regions = Vec::new();
    if !argument_moves.is_empty() {
        regions.push(Box::new(vf::Region::BasicBlock {
            instructions: argument_moves,
        }));
    }
    regions.push(prepared.body.region);

    Some(sequence(regions))
}

fn find_inline_candidate<'program>(
    state: &mut OptimizationState<'program, '_>,
    function_id: &BigUint,
) -> Option<InlineCandidate<'program>> {
    let (current_tube, referenced_tubes) = state.program_mut().current_and_referenced_tubes_mut();
    let current_tube = current_tube?;
    let function_import = current_tube
        .function_info
        .get(function_id)
        .map(|info| info.import_specifier.clone())?;

    if let Some(definition) = find_function_definition(current_tube, function_id) {
        let candidate = InlineCandidate {
            source: InlineSource::Current,
            definition: definition.clone(),
        };
        if should_inline(&candidate, false) {
            return Some(candidate);
        }
    }

    for tube in referenced_tubes.values().copied() {
        for definition in function_definitions(tube) {
            let Some(target_import) =
                current_tube.convert_import_specifier_from(tube, &definition.import)
            else {
                continue;
            };
            if target_import != function_import {
                continue;
            }

            let candidate = InlineCandidate {
                source: InlineSource::Referenced(tube),
                definition: definition.clone(),
            };
            if should_inline(&candidate, true) {
                return Some(candidate);
            }
        }
    }

    None
}

fn find_function_definition<'tube>(
    tube: &'tube TubeModel,
    function_id: &BigUint,
) -> Option<&'tube vf::FunctionDefinition> {
    tube.modules
        .iter()
        .flat_map(|module| module.exports.iter())
        .find_map(|export| {
            let ModuleExportEntry::FunctionDefinition(definition) = export else {
                return None;
            };
            if &definition.function_id == function_id {
                Some(definition)
            } else {
                None
            }
        })
}

fn function_definitions(tube: &TubeModel) -> impl Iterator<Item = &vf::FunctionDefinition> {
    tube.modules
        .iter()
        .flat_map(|module| module.exports.iter())
        .filter_map(|export| {
            let ModuleExportEntry::FunctionDefinition(definition) = export else {
                return None;
            };
            Some(definition)
        })
}

fn should_inline(candidate: &InlineCandidate<'_>, cross_tube: bool) -> bool {
    // A cross-tube body can legally refer to module-private declarations in its
    // defining tube. Moving that body into another JVM module would turn those
    // references into illegal package accesses, so keep the call boundary.
    if cross_tube {
        return false;
    }

    let Some(body) = candidate.body() else {
        return false;
    };

    let instruction_count = analysis::instructions(&body.region).count();
    let variable_count = body.variables.variables.len();
    let max_instruction_count = if candidate.definition.flags.inline {
        MAX_DECLARED_INLINE_INSTRUCTIONS
    } else {
        MAX_IMPLICIT_INLINE_INSTRUCTIONS
    };

    instruction_count <= max_instruction_count && variable_count <= MAX_INLINE_VARIABLES
}

fn argument_moves(
    args: &[Box<vf::RegisterId>],
    argument_registers: &[vf::RegisterId],
) -> Option<Vec<Box<vf::Instruction>>> {
    if args.len() != argument_registers.len() {
        return None;
    }

    Some(
        args.iter()
            .zip(argument_registers)
            .map(|(src, dest)| {
                Box::new(vf::Instruction::Move {
                    dest: Box::new(dest.clone()),
                    src: src.clone(),
                })
            })
            .collect(),
    )
}

fn next_register(state: &OptimizationState, variables: &vf::VariableDeclarations) -> BigUint {
    BigUint::from(state.argument_types().len() + variables.variables.len())
}

fn basic_block(instructions: Vec<Box<vf::Instruction>>) -> Box<vf::Region> {
    Box::new(vf::Region::BasicBlock { instructions })
}

fn sequence(regions: impl IntoIterator<Item = Box<vf::Region>>) -> vf::Region {
    let mut regions = regions
        .into_iter()
        .filter(|region| match region.as_ref() {
            vf::Region::BasicBlock { instructions } => !instructions.is_empty(),
            _ => true,
        })
        .collect::<Vec<_>>();

    if regions.len() == 1 {
        *regions.pop().unwrap()
    } else {
        vf::Region::Sequence { regions }
    }
}

struct InlineCandidate<'tube> {
    source: InlineSource<'tube>,
    definition: vf::FunctionDefinition,
}

enum InlineSource<'tube> {
    Current,
    Referenced(&'tube TubeModel),
}

impl<'tube> InlineCandidate<'tube> {
    fn body(&self) -> Option<&vf::FunctionBody> {
        let implementation = self.definition.implementation.as_ref()?;
        let vf::FunctionImplementation::VmIr { body } = implementation.as_ref() else {
            return None;
        };
        Some(body)
    }

    fn convert_body_to_current_tube(
        &self,
        current_tube: &mut TubeModel,
    ) -> Option<vf::FunctionBody> {
        let body = self.body()?;
        match self.source {
            InlineSource::Current => Some(body.clone()),
            InlineSource::Referenced(source_tube) => {
                current_tube.convert_function_body_from(source_tube, body)
            }
        }
    }

    fn convert_argument_types_to_current_tube(
        &self,
        current_tube: &mut TubeModel,
    ) -> Option<Vec<vf::Token>> {
        self.definition
            .signature
            .parameters
            .iter()
            .map(|parameter| match self.source {
                InlineSource::Current => Some((*parameter.param_type).clone()),
                InlineSource::Referenced(source_tube) => {
                    current_tube.convert_token_from(source_tube, &parameter.param_type)
                }
            })
            .collect()
    }
}
use alloc::{boxed::Box, vec::Vec};
