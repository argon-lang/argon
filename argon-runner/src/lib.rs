mod context;
mod tubes;

use crate::context::RunnerContext;
use crate::tubes::load_referenced_tube;
use argon_compiler::{ContextObject, TubeCollectionBuilder, TubeName};
use argon_io::InputFile;
use argon_source::SourceCodeTubeOptions;
use argon_util::{InternalCompilerError, TubeEncodingError};
use clap::Args;
use embedded_io::{ErrorType, Read};
use esexpr::ESExprCodec;
use esexpr_binary::ExprGeneratorSync;
use rayon::prelude::*;
use std::ffi::OsStr;
use std::io::ErrorKind;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use walkdir::WalkDir;

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

#[derive(Clone)]
struct SourcePath {
    path: PathBuf,
}

struct SourceFileReader {
    file: std::fs::File,
    path: PathBuf,
}

impl ErrorType for SourceFileReader {
    type Error = InternalCompilerError;
}

impl Read for SourceFileReader {
    fn read(&mut self, buf: &mut [u8]) -> Result<usize, Self::Error> {
        std::io::Read::read(&mut self.file, buf)
            .map_err(|err| InternalCompilerError::IoError(self.path.clone(), err))
    }
}

impl InputFile for SourcePath {
    type Reader = SourceFileReader;

    fn path(&self) -> &Path {
        &self.path
    }

    fn open(&self) -> Result<Self::Reader, InternalCompilerError> {
        std::fs::File::open(&self.path)
            .map(|file| SourceFileReader {
                file,
                path: self.path.clone(),
            })
            .map_err(|err| InternalCompilerError::IoError(self.path.clone(), err))
    }
}

pub fn compile(options: CompileOptions) -> bool {
    let context = Arc::new(RunnerContext::new());
    let tube_collection = TubeCollectionBuilder::new(context.clone());

    let referenced_tube_names = options
        .referenced_tubes
        .par_iter()
        .filter_map(|ref_tube| load_referenced_tube(context.clone(), &tube_collection, ref_tube))
        .map(|tube| tube.name().clone())
        .collect::<Vec<_>>();

    let source_files = collect_source_files(context.clone(), &options.input_dirs);

    let source_options = SourceCodeTubeOptions {
        name: options.tube_name,
        referenced_tubes: referenced_tube_names,
        sources: source_files,
    };

    let tube = argon_source::define_source_tube(context.clone(), source_options, &tube_collection);
    if context.runner_reporter().has_errors() {
        context.runner_reporter().print_error_messages();
        delete_output_file(&options.output_file);
        return false;
    }

    let mut out_file = match std::fs::File::create(&options.output_file) {
        Ok(file) => file,
        Err(e) => {
            context
                .reporter()
                .report_error(InternalCompilerError::IoError(
                    options.output_file.clone(),
                    e,
                ));
            context.runner_reporter().print_error_messages();
            delete_output_file(&options.output_file);
            return false;
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
                context
                    .reporter()
                    .report_error(InternalCompilerError::TubeEncodingError(
                        TubeEncodingError::GeneratorError(e),
                    ));
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

pub fn gen_ir(_options: GenIrOptions) {
    todo!("generate Argon VM IR from a tube")
}

pub fn codegen_js(_options: JsCodeGenOptions) {
    todo!("generate JavaScript code from Argon VM IR")
}

fn collect_source_files(context: Arc<RunnerContext>, input_dirs: &[PathBuf]) -> Vec<SourcePath> {
    input_dirs
        .par_iter()
        .flat_map(|source_dir| WalkDir::new(source_dir).into_iter().par_bridge())
        .filter_map(|entry| match entry {
            Ok(entry) => Some(entry),
            Err(e) => {
                context
                    .reporter()
                    .report_error(InternalCompilerError::WalkDirError(e));
                None
            }
        })
        .filter(|entry| {
            entry.file_type().is_file() && entry.path().extension() == Some(OsStr::new("argon"))
        })
        .map(|entry| SourcePath {
            path: entry.path().to_path_buf(),
        })
        .collect()
}

fn delete_output_file(output_file: &Path) {
    match std::fs::remove_file(output_file) {
        Ok(()) => {}
        Err(err) if err.kind() == ErrorKind::NotFound => {}
        Err(err) => eprintln!(
            "failed to delete output file {}: {err}",
            output_file.display()
        ),
    }
}
