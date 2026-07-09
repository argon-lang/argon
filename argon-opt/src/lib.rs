mod analysis;
pub mod model;
mod mutator;
pub mod optimizer;
pub mod pass;

use argon_format_vm::vm as vf;

pub fn optimize_entry(optimizer: &optimizer::Optimizer, entry: &mut vf::TubeFileEntry) {
    match entry {
        vf::TubeFileEntry::FunctionDefinition { definition } => {
            optimize_implementation(
                optimizer,
                &definition.signature,
                &mut definition.implementation,
            );
        }

        vf::TubeFileEntry::TraitDefinition { definition } => {
            for method in &mut definition.methods {
                optimize_method_implementation(optimizer, method);
            }
        }

        vf::TubeFileEntry::InstanceDefinition { definition } => {
            for method in &mut definition.methods {
                optimize_method_implementation(optimizer, method);
            }
        }

        _ => {}
    }
}

fn optimize_method_implementation(
    optimizer: &optimizer::Optimizer,
    method: &mut vf::MethodDefinition,
) {
    let Some(implementation) = &mut method.implementation else {
        return;
    };

    let vf::FunctionImplementation::VmIr { body } = implementation.as_mut() else {
        return;
    };

    let argument_types = method_argument_types(&method.signature);
    optimizer.optimize(&argument_types, body);
}

fn optimize_implementation(
    optimizer: &optimizer::Optimizer,
    signature: &vf::FunctionSignature,
    implementation: &mut Option<Box<vf::FunctionImplementation>>,
) {
    let argument_types = signature_argument_types(signature);
    let Some(implementation) = implementation else {
        return;
    };

    let vf::FunctionImplementation::VmIr { body } = implementation.as_mut() else {
        return;
    };

    optimizer.optimize(&argument_types, body);
}

fn signature_argument_types(signature: &vf::FunctionSignature) -> Vec<vf::Token> {
    signature
        .parameters
        .iter()
        .map(|param| param.param_type.as_ref().clone())
        .collect()
}

fn method_argument_types(signature: &vf::FunctionSignature) -> Vec<vf::Token> {
    std::iter::once(vf::Token::Boxed {})
        .chain(signature_argument_types(signature))
        .collect()
}
