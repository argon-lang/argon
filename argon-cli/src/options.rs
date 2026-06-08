use argon_compiler::TubeName;
use clap::{Args, Parser, Subcommand};
use std::path::PathBuf;

#[derive(Parser)]
#[command(name = "argonc")]
pub struct CommandLineOptions {
    #[clap(subcommand)]
    pub command: Command,
}

#[derive(Subcommand)]
pub enum Command {
    /// Compile Argon source code to a tube (.artube).
    Compile(CompileOptions),

    /// Generate Argon VM IR from a tube.
    #[command(name = "genir")]
    GenIR(GenIrOptions),

    /// Generate code from Argon VM IR.
    #[command(name = "codegen")]
    CodeGen(CodeGenCommand),

    /// Load platform-specific metadata.
    #[command(name = "platform-metadata")]
    PlatformMetadata(PlatformMetadataCommand),
}

#[derive(Args, Debug)]
pub struct CompileOptions {
    /// The name of the tube.
    #[arg(short = 'n', long)]
    pub tube_name: TubeName,

    /// Input directories for source code
    #[arg(short, long = "input", required = true)]
    pub input_dirs: Vec<PathBuf>,

    /// Referenced tubes
    #[arg(short, long = "reference")]
    pub referenced_tubes: Vec<PathBuf>,

    /// Platform metadata
    #[arg(short, long)]
    pub platform: Vec<PathBuf>,

    /// Output tube file
    #[arg(short, long = "output")]
    pub output_file: PathBuf,
}

#[derive(Args, Debug)]
pub struct GenIrOptions {
    /// Input tube file
    #[arg(short, long = "input")]
    pub input_tube: PathBuf,

    /// Referenced tubes
    #[arg(short, long = "reference")]
    pub referenced_tubes: Vec<PathBuf>,

    /// Output Argon VM IR file
    #[arg(short, long = "output")]
    pub output_file: PathBuf,

    #[arg(long)]
    pub platform: String,
}

#[derive(Args)]
pub struct CodeGenCommand {
    #[clap(subcommand)]
    pub backend_command: CodeGenBackendCommand,
}

#[derive(Subcommand)]
pub enum CodeGenBackendCommand {
    /// Generate JavaScript code from Argon VM IR.
    JS(JsCodeGenOptions),
}

#[derive(Args, Debug)]
pub struct JsCodeGenOptions {
    /// Input Argon VM IR file.
    #[arg(short, long)]
    pub input: PathBuf,

    /// Output directory.
    #[arg(short, long)]
    pub output: PathBuf,
}

#[derive(Args)]
pub struct PlatformMetadataCommand {
    #[clap(subcommand)]
    pub backend_command: PlatformMetadataBackendCommand,
}

#[derive(Subcommand)]
pub enum PlatformMetadataBackendCommand {
    /// Load platform metadata for JavaScript.
    JS(JsPlatformMetadataOptions),
}

#[derive(Args, Debug)]
pub struct JsPlatformMetadataOptions {
    /// NPM package name for the generated tube.
    #[arg(long)]
    pub package_name: Option<String>,

    /// JavaScript extern file.
    #[arg(long = "extern")]
    pub extern_files: Vec<PathBuf>,

    /// Output platform metadata file.
    #[arg(short, long)]
    pub output_file: PathBuf,
}
