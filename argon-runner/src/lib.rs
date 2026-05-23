mod context;
mod tubes;

use crate::context::RunnerContext;
use crate::tubes::load_referenced_tube;
use argon_compiler::{Context, ContextObject, TubeCollection, TubeCollectionBuilder, TubeName};
use argon_source::SourceCodeTubeOptions;
use argon_util::{InternalCompilerError, TubeEncodingError};
use clap::Args;
use esexpr::ESExprCodec;
use esexpr_binary::ExprGeneratorSync;
use rayon::prelude::*;
use std::path::PathBuf;
use std::sync::Arc;

#[derive(Args, Debug)]
pub struct CompileOptions {
    /// The name of the tube.
    #[arg(short = 'n', long)]
    pub tube_name: TubeName,

    /// Input directories for source code
    #[arg(short, long = "input", required = true)]
    pub input_dirs: Vec<PathBuf>,

    /// Referenced tubes
    #[arg(short, long)]
    pub referenced_tubes: Vec<PathBuf>,

    /// Output tube file
    #[arg(short, long)]
    pub output_file: PathBuf,
}

#[derive(Args, Debug)]
pub struct GenIrOptions {
    /// Input tube file
    #[arg(short, long)]
    pub input_tube: PathBuf,

    /// Referenced tubes
    #[arg(short, long)]
    pub referenced_tubes: Vec<PathBuf>,

    /// Output Argon VM IR file
    #[arg(short, long)]
    pub output_file: PathBuf,
}

#[derive(Args, Debug)]
pub struct JsCodeGenOptions {
    /// Input IR file.
    #[arg(short, long)]
    pub input_file: PathBuf,

    /// Output directory for generated JS code.
    #[arg(short, long)]
    pub output_dir: PathBuf,
}

pub fn compile(options: CompileOptions) {
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
        sources: options.input_dirs,
    };

    let tube = argon_source::define_source_tube(context.clone(), source_options, &tube_collection);

    let mut out_file = match std::fs::File::create(&options.output_file) {
        Ok(file) => file,
        Err(e) => {
            context
                .reporter()
                .report_error(InternalCompilerError::IoError(
                    options.output_file.clone(),
                    e,
                ));
            return;
        }
    };

    let mut expr_gen = esexpr_binary::ExprGenerator::new(&mut out_file);

    for entry in argon_tube::encoder::encode_tube(tube) {
        let entry = match entry {
            Ok(entry) => entry,
            Err(e) => {
                context
                    .reporter()
                    .report_error(InternalCompilerError::TubeEncodingError(e));
                return;
            }
        };

        let expr = entry.encode_esexpr();
        match expr_gen.generate(&expr) {
            Ok(()) => (),
            Err(e) => {
                context
                    .reporter()
                    .report_error(InternalCompilerError::TubeEncodingError(
                        TubeEncodingError::GeneratorError(e),
                    ));
            }
        }
    }
}

pub fn gen_ir(_options: GenIrOptions) {
    todo!("generate Argon VM IR from a tube")
}

pub fn codegen_js(_options: JsCodeGenOptions) {
    todo!("generate JavaScript code from Argon VM IR")
}
