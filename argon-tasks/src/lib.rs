#![no_std]

extern crate alloc;
#[cfg(feature = "std")]
extern crate std;
pub mod backend;
mod context;
#[cfg(feature = "std")]
pub mod local_io;
mod tubes;

use crate::backend::metadata::load_platform_metadata;
use crate::context::RunnerContext;
use crate::tubes::load_referenced_tube;
use alloc::{string::String, sync::Arc, vec::Vec};
use argon_compiler::{ContextObject, TubeCollectionBuilder, TubeMetadata, TubeName, Unload};
use argon_format_vm::vm as vf;
use argon_io::{InputDirectory, InputFile, OutputDirectory, OutputFile};
use argon_source::SourceCodeTubeOptions;
use argon_util::sync::{ThreadSafe, parallel::*};
use argon_util::{CompileError, ErrorReporter, InternalCompilerError, TubeFormatError};
use embedded_io::{Write, WriteFmtError};
use esexpr::ESExprCodec;
use esexpr_binary::{ExprGeneratorSync, ExprParserSync, GeneratorError, ParseError};

pub struct CompileOptions<ID, IF, O> {
    pub tube_name: TubeName,
    pub input_dirs: Vec<ID>,
    pub referenced_tubes: Vec<IF>,
    pub platform_metadata: Vec<IF>,
    pub output_file: O,
}

pub struct GenIrOptions<IF, O> {
    pub input_tube: IF,
    pub referenced_tubes: Vec<IF>,
    pub platform: String,
    pub output_file: O,
}

pub struct OptimizeOptions<IF, O> {
    pub input_file: IF,
    pub referenced_tubes: Vec<IF>,
    pub output_file: O,
    pub optimizations: Vec<String>,
}

pub use backend::{
    BackendJS, CodegenJSOptions as JsCodeGenOptions,
    PlatformMetadataJSOptions as JsPlatformMetadataOptions,
};

pub fn compile<ID, IF, O, W>(options: CompileOptions<ID, IF, O>, error_output: &mut W) -> bool
where
    ID: InputDirectory + ThreadSafe,
    ID::File: ThreadSafe,
    IF: InputFile + ThreadSafe,
    O: OutputFile,
    W: Write,
{
    let mut context = RunnerContext::new();
    for input_file in options.platform_metadata {
        load_platform_metadata(&mut context, input_file);
    }

    let platform_metadata = context.tube_platform_metadata();
    let context = Arc::new(context);
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
        metadata: TubeMetadata {
            platform: platform_metadata,
        },
    };

    'errors: {
        let tube =
            argon_source::define_source_tube(context.clone(), source_options, &tube_collection);
        if context.runner_reporter().has_errors() {
            break 'errors;
        }

        let mut out_file = match options.output_file.open() {
            Ok(file) => file,
            Err(err) => {
                context.reporter().report_error(err);
                break 'errors;
            }
        };

        let mut expr_gen = esexpr_binary::ExprGenerator::new(&mut out_file);

        for entry in argon_tube::encoder::encode_tube(context.clone(), tube) {
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
        tube_collection.tube_collection().unload();
        return true;
    }

    tube_collection.tube_collection().unload();
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

pub fn gen_ir<IF, O, W>(options: GenIrOptions<IF, O>, error_output: &mut W) -> bool
where
    IF: InputFile + ThreadSafe,
    O: OutputFile,
    W: Write,
{
    let context = RunnerContext::new();
    let context = Arc::new(context);
    let tube_collection = TubeCollectionBuilder::new(context.clone());

    options.referenced_tubes.par_iter().for_each(|ref_tube| {
        load_referenced_tube(context.clone(), &tube_collection, ref_tube);
    });

    'errors: {
        let Some(tube) =
            load_referenced_tube(context.clone(), &tube_collection, &options.input_tube)
        else {
            break 'errors;
        };

        if context.runner_reporter().has_errors() {
            break 'errors;
        }

        let mut out_file = match options.output_file.open() {
            Ok(file) => file,
            Err(e) => {
                context.reporter().report_error(e);
                break 'errors;
            }
        };

        match argon_tube::vm::encode_vm_tube(
            &mut out_file,
            context.clone(),
            tube,
            &options.platform,
        ) {
            Ok(_) => {}
            Err(e) => {
                context.reporter().report_error(e);
                break 'errors;
            }
        }

        if context.runner_reporter().has_errors() {
            break 'errors;
        }

        let _ = writeln!(error_output, "Compilation succeeded.");
        tube_collection.tube_collection().unload();
        return true;
    }

    tube_collection.tube_collection().unload();
    let _ = context.runner_reporter().print_error_messages(error_output);
    let _ = delete_output_file(&options.output_file, error_output);
    false
}

pub async fn platform_metadata_js<B, I, O, W>(
    backend: &B,
    options: JsPlatformMetadataOptions<I, O>,
    error_output: &mut W,
) -> bool
where
    B: BackendJS,
    I: InputFile + 'static,
    I::Reader: 'static,
    O: OutputFile + 'static,
    O::Writer: 'static,
    W: Write,
{
    backend
        .platform_metadata::<I, O, W>(options, error_output)
        .await
}

pub async fn codegen_js<B, I, O, W>(
    backend: &B,
    options: JsCodeGenOptions<I, O>,
    error_output: &mut W,
) -> bool
where
    B: BackendJS,
    I: InputFile + 'static,
    I::Reader: 'static,
    O: OutputDirectory + 'static,
    O::File: OutputFile + 'static,
    <O::File as OutputFile>::Writer: 'static,
    W: Write,
{
    backend.codegen_js(options, error_output).await
}

pub fn optimize<IF, O, W>(options: OptimizeOptions<IF, O>, error_output: &mut W) -> bool
where
    IF: InputFile,
    O: OutputFile,
    W: Write,
{
    let context = RunnerContext::new();

    let mut optimizer = argon_opt::optimizer::Optimizer::new();
    for optimization in &options.optimizations {
        let Some(pass) = argon_opt::pass::pass_by_name(optimization) else {
            context
                .runner_reporter()
                .report_error(CompileError::unknown_optimization(optimization));
            let _ = context.runner_reporter().print_error_messages(error_output);
            return false;
        };

        optimizer.add_pass(pass);
    }

    'errors: {
        let referenced_tubes = options
            .referenced_tubes
            .iter()
            .filter_map(|ref_tube| load_vm_tube_model(&context, ref_tube))
            .collect::<Vec<_>>();

        let Some(mut model) = load_vm_tube_model(&context, &options.input_file) else {
            break 'errors;
        };

        if context.runner_reporter().has_errors() {
            break 'errors;
        }

        argon_opt::optimize_tube(&optimizer, &mut model, referenced_tubes.iter());
        let entries = model.into_entries();

        let mut out_file = match options.output_file.open() {
            Ok(file) => file,
            Err(err) => {
                context.runner_reporter().report_error(err);
                break 'errors;
            }
        };

        let mut expr_gen = esexpr_binary::ExprGenerator::new(&mut out_file);
        for entry in entries {
            let expr = entry.encode_esexpr();
            match expr_gen.generate(&expr) {
                Ok(()) => {}
                Err(GeneratorError::IOError(err)) => {
                    context.runner_reporter().report_error(err);
                    break 'errors;
                }
            }
        }

        if context.runner_reporter().has_errors() {
            break 'errors;
        }

        let _ = writeln!(error_output, "Optimization succeeded.");
        return true;
    }

    let _ = context.runner_reporter().print_error_messages(error_output);
    let _ = delete_output_file(&options.output_file, error_output);
    false
}

fn load_vm_tube_model<F>(
    context: &RunnerContext,
    input_file: &F,
) -> Option<argon_vm::model::TubeModel>
where
    F: InputFile,
{
    let mut in_file = match input_file.open() {
        Ok(file) => file,
        Err(err) => {
            context.runner_reporter().report_error(err);
            return None;
        }
    };

    let mut entries = Vec::new();
    let mut expr_stream = esexpr_binary::parse_sync(&mut in_file);
    loop {
        let expr = match expr_stream.try_read_next_expr() {
            Ok(Some(expr)) => expr,
            Ok(None) => break,
            Err(err) => {
                context
                    .runner_reporter()
                    .report_error(ir_parse_error(input_file, err));
                return None;
            }
        };

        let entry = match vf::TubeFileEntry::decode_esexpr(expr) {
            Ok(entry) => entry,
            Err(err) => {
                context
                    .runner_reporter()
                    .report_error(ir_format_error(input_file, TubeFormatError::from(err)));
                return None;
            }
        };

        entries.push(entry);
    }

    Some(argon_vm::model::TubeModel::from_entries(entries))
}

fn ir_format_error<F>(file: &F, error: TubeFormatError) -> InternalCompilerError
where
    F: InputFile,
{
    match error {
        #[cfg(feature = "std")]
        TubeFormatError::FileError(_, err) => InternalCompilerError::TubeFormatError(
            TubeFormatError::FileError(file.path().to_path_buf(), err),
        ),
        error => InternalCompilerError::TubeFormatError(error),
    }
}

fn ir_parse_error<F>(file: &F, error: ParseError<InternalCompilerError>) -> InternalCompilerError
where
    F: InputFile,
{
    match error {
        ParseError::IOError(err) => match err {
            #[cfg(feature = "std")]
            InternalCompilerError::IoError(_, io_err) => {
                InternalCompilerError::IoError(file.path().to_path_buf(), io_err)
            }
            err => err,
        },
        ParseError::InvalidTokenByte(value) => InternalCompilerError::TubeFormatError(
            TubeFormatError::ExprParseErrorNoIo(ParseError::InvalidTokenByte(value)),
        ),
        ParseError::InvalidStringTableIndex => InternalCompilerError::TubeFormatError(
            TubeFormatError::ExprParseErrorNoIo(ParseError::InvalidStringTableIndex),
        ),
        ParseError::InvalidLength => InternalCompilerError::TubeFormatError(
            TubeFormatError::ExprParseErrorNoIo(ParseError::InvalidLength),
        ),
        ParseError::UnexpectedKeywordToken => InternalCompilerError::TubeFormatError(
            TubeFormatError::ExprParseErrorNoIo(ParseError::UnexpectedKeywordToken),
        ),
        ParseError::UnexpectedConstructorEnd => InternalCompilerError::TubeFormatError(
            TubeFormatError::ExprParseErrorNoIo(ParseError::UnexpectedConstructorEnd),
        ),
        ParseError::UnexpectedEndOfFile => InternalCompilerError::TubeFormatError(
            TubeFormatError::ExprParseErrorNoIo(ParseError::UnexpectedEndOfFile),
        ),
        ParseError::InvalidStringPool(err) => InternalCompilerError::TubeFormatError(
            TubeFormatError::ExprParseErrorNoIo(ParseError::InvalidStringPool(err)),
        ),
        ParseError::Utf8Error(err) => InternalCompilerError::TubeFormatError(
            TubeFormatError::ExprParseErrorNoIo(ParseError::Utf8Error(err)),
        ),
    }
}
