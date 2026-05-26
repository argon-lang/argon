#![no_std]

extern crate alloc;
#[cfg(feature = "std")]
extern crate std;

mod context;
#[cfg(feature = "std")]
pub mod local_io;
mod tubes;

use crate::context::RunnerContext;
use crate::tubes::load_referenced_tube;
use alloc::{sync::Arc, vec::Vec};
use argon_compiler::{ContextObject, TubeCollectionBuilder, TubeName};
use argon_io::{EmbeddedIoWrite, InputDirectory, InputFile, OutputDirectory, OutputFile};
use argon_source::SourceCodeTubeOptions;
use argon_util::sync::{ThreadSafe, parallel::*};
use embedded_io::{Write, WriteFmtError};
use esexpr::ESExprCodec;
use esexpr_binary::{ExprGeneratorSync, GeneratorError};

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

pub fn compile<I, R, O, W>(options: CompileOptions<I, R, O>, error_output: &mut W) -> bool
where
    I: InputDirectory + ThreadSafe,
    I::File: ThreadSafe,
    R: InputFile + ThreadSafe,
    O: OutputFile,
    W: Write,
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

    'errors: {
        let tube = argon_source::define_source_tube(context.clone(), source_options, &tube_collection);
        if context.runner_reporter().has_errors() {
            break 'errors;
        }

        let out_file = match options.output_file.open() {
            Ok(file) => file,
            Err(err) => {
                context.reporter().report_error(err);
                break 'errors;
            }
        };

        let mut out_file = EmbeddedIoWrite::new(out_file);
        let mut expr_gen =
            esexpr_binary::ExprGenerator::<_, argon_util::InternalCompilerError>::new(&mut out_file);

        for entry in argon_tube::encoder::encode_tube(tube) {
            let entry = match entry {
                Ok(entry) => entry,
                Err(e) => {
                    context.reporter().report_error(e);
                    break 'errors;
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
                    break 'errors;
                }
            }
        }

        if context.runner_reporter().has_errors() {
            break 'errors;
        }

        let _ = writeln!(error_output, "Compilation succeeded.");
        return true;
    }


    let _ = context.runner_reporter().print_error_messages(error_output);
    let _ = delete_output_file(&options.output_file, error_output);
    false
}

fn delete_output_file<O, W>(
    output_file: &O,
    error_output: &mut W,
) -> Result<(), WriteFmtError<W::Error>>
where
    O: OutputFile,
    W: Write,
{
    if let Err(err) = output_file.delete() {
        #[cfg(feature = "std")]
        error_output.write_fmt(format_args!(
            "failed to delete output file {}: {err}",
            output_file.path().display()
        ))?;
        #[cfg(not(feature = "std"))]
        error_output.write_fmt(format_args!("failed to delete output file: {err}"))?;

        error_output.write_all(b"\n")?;
    }

    Ok(())
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
