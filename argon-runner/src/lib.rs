mod context;
pub mod local_io;
mod tubes;

use crate::context::RunnerContext;
use crate::tubes::load_referenced_tube;
use argon_compiler::{ContextObject, TubeCollectionBuilder, TubeName};
use argon_io::{EmbeddedIoWrite, InputDirectory, InputFile, OutputDirectory, OutputFile};
use argon_source::SourceCodeTubeOptions;
use argon_util::sync::parallel::*;
use esexpr::ESExprCodec;
use esexpr_binary::{ExprGeneratorSync, GeneratorError};
use std::sync::Arc;

pub struct CompileOptions<I, R, O> {
    pub tube_name: TubeName,
    pub input_dirs: Vec<I>,
    pub referenced_tubes: Vec<R>,
    pub output_file: O,
}

pub struct GenIrOptions<I, R, O> {
    pub input_tube: I,
    pub referenced_tubes: Vec<R>,
    pub output_file: O,
}

pub struct JsCodeGenOptions<I, O> {
    pub input_file: I,
    pub output_dir: O,
}

pub fn compile<I, R, O>(options: CompileOptions<I, R, O>) -> bool
where
    I: InputDirectory + Sync,
    I::File: Send + Sync,
    R: InputFile + Sync,
    O: OutputFile,
{
    let context = Arc::new(RunnerContext::new());
    let tube_collection = TubeCollectionBuilder::new(context.clone());

    let referenced_tube_names = options
        .referenced_tubes
        .par_iter()
        .filter_map(|ref_tube| load_referenced_tube(context.clone(), &tube_collection, ref_tube))
        .map(|tube| tube.name().clone())
        .collect::<Vec<_>>();

    let source_options = SourceCodeTubeOptions {
        name: options.tube_name,
        referenced_tubes: referenced_tube_names,
        input_dirs: options.input_dirs,
    };

    let tube = argon_source::define_source_tube(context.clone(), source_options, &tube_collection);
    if context.runner_reporter().has_errors() {
        context.runner_reporter().print_error_messages();
        delete_output_file(&options.output_file);
        return false;
    }

    let out_file = match options.output_file.open() {
        Ok(file) => file,
        Err(err) => {
            context.reporter().report_error(err);
            context.runner_reporter().print_error_messages();
            delete_output_file(&options.output_file);
            return false;
        }
    };

    let mut out_file = EmbeddedIoWrite::new(out_file);
    let mut expr_gen = esexpr_binary::ExprGenerator::new(&mut out_file);

    for entry in argon_tube::encoder::encode_tube(tube) {
        let entry = match entry {
            Ok(entry) => entry,
            Err(e) => {
                context.reporter().report_error(e);
                context.runner_reporter().print_error_messages();
                drop(expr_gen);
                drop(out_file);
                delete_output_file(&options.output_file);
                return false;
            }
        };

        let expr = entry.encode_esexpr();
        match expr_gen.generate(&expr) {
            Ok(()) => (),
            Err(e) => {
                match e {
                    GeneratorError::IOError(ioe) => {
                        context.reporter().report_error(ioe);
                    }
                }
                context.runner_reporter().print_error_messages();
                drop(expr_gen);
                drop(out_file);
                delete_output_file(&options.output_file);
                return false;
            }
        }
    }

    if context.runner_reporter().has_errors() {
        context.runner_reporter().print_error_messages();
        drop(expr_gen);
        drop(out_file);
        delete_output_file(&options.output_file);
        false
    } else {
        true
    }
}

fn delete_output_file<O>(output_file: &O)
where
    O: OutputFile,
{
    if let Err(err) = output_file.delete() {
        eprintln!(
            "failed to delete output file {}: {err}",
            output_file.path().display()
        );
    }
}

pub fn gen_ir<I, R, O>(_options: GenIrOptions<I, R, O>)
where
    I: InputFile,
    R: InputFile,
    O: OutputFile,
{
    todo!("generate Argon VM IR from a tube")
}

pub fn codegen_js<I, O>(_options: JsCodeGenOptions<I, O>)
where
    I: InputFile,
    O: OutputDirectory,
{
    todo!("generate JavaScript code from Argon VM IR")
}
