use argon_compiler::TubeName;
use clap::{Args, Parser, Subcommand, ValueEnum};
use std::path::PathBuf;

#[derive(Parser)]
#[command(name = "argonc")]
pub struct CommandLineOptions {
    /// Format used for task messages written to standard output.
    #[arg(long, global = true, value_enum, default_value_t = OutputFormat::Text)]
    pub output_format: OutputFormat,

    #[clap(subcommand)]
    pub command: Command,
}

#[derive(Clone, Copy, Debug, Default, ValueEnum)]
pub enum OutputFormat {
    #[default]
    Text,
    #[value(name = "esexpr")]
    ESExpr,
}

#[derive(Subcommand)]
pub enum Command {
    /// Compile Argon source code to a tube (.artube).
    Compile(CompileOptions),

    /// Generate Argon VM IR from a tube.
    #[command(name = "genir")]
    GenIR(GenIrOptions),

    /// Optimize Argon VM IR.
    #[command(name = "optimize")]
    Optimize(OptimizeOptions),

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

#[derive(Args, Debug)]
pub struct OptimizeOptions {
    /// Input Argon VM IR file.
    #[arg(short, long)]
    pub input: PathBuf,

    /// Referenced Argon VM IR files.
    #[arg(short, long = "reference")]
    pub referenced_tubes: Vec<PathBuf>,

    /// Output Argon VM IR file.
    #[arg(short, long)]
    pub output: PathBuf,

    /// Optimization pass to run.
    #[arg(short = 'O', long = "optimization", required = true)]
    pub optimizations: Vec<String>,
}

#[derive(Args)]
pub struct CodeGenCommand {
    #[clap(subcommand)]
    pub backend_command: CodeGenBackendCommand,
}

#[derive(Subcommand)]
pub enum CodeGenBackendCommand {
    /// Generate a Perl distribution from Argon VM IR.
    #[command(name = "perl")]
    Perl(PerlCodeGenOptions),
    /// Generate JavaScript code from Argon VM IR.
    #[command(name = "js")]
    JS(JsCodeGenOptions),

    /// Generate JVM code from Argon VM IR.
    #[command(name = "jvm")]
    JVM(JvmCodeGenOptions),
}

#[derive(Args, Debug)]
pub struct PerlCodeGenOptions {
    #[arg(short, long)]
    pub input: PathBuf,
    #[arg(short, long)]
    pub output: PathBuf,
    #[arg(long)]
    pub executable: Option<String>,
}

#[derive(Args, Debug)]
pub struct JsCodeGenOptions {
    /// Input Argon VM IR file.
    #[arg(short, long)]
    pub input: PathBuf,

    /// Output directory.
    #[arg(short, long)]
    pub output: PathBuf,

    /// Name of the executable defined by this tube.
    #[arg(long)]
    pub executable: Option<String>,
}

#[derive(Args, Debug)]
pub struct JvmCodeGenOptions {
    /// Input Argon VM IR file.
    #[arg(short, long)]
    pub input: PathBuf,

    /// Output JAR file.
    #[arg(short, long)]
    pub output: PathBuf,

    /// Generate an executable Main class.
    #[arg(long)]
    pub executable: bool,
}

#[derive(Args)]
pub struct PlatformMetadataCommand {
    #[clap(subcommand)]
    pub backend_command: PlatformMetadataBackendCommand,
}

#[derive(Subcommand)]
pub enum PlatformMetadataBackendCommand {
    /// Load platform metadata for Perl.
    #[command(name = "perl")]
    Perl(PerlPlatformMetadataOptions),
    /// Load platform metadata for JavaScript.
    #[command(name = "js")]
    JS(JsPlatformMetadataOptions),

    /// Load platform metadata for JVM.
    #[command(name = "jvm")]
    JVM(JvmPlatformMetadataOptions),
}

#[derive(Args, Debug)]
pub struct PerlPlatformMetadataOptions {
    #[arg(long)]
    pub distribution_name: Option<String>,
    #[arg(long = "root-package", value_delimiter = ':')]
    pub root_package: Option<Vec<String>>,
    #[arg(long = "extern")]
    pub extern_files: Vec<PathBuf>,
    #[arg(short, long)]
    pub output_file: PathBuf,
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

#[derive(Args, Debug)]
pub struct JvmPlatformMetadataOptions {
    /// JVM extern classfile or directory.
    #[arg(long = "extern")]
    pub extern_files: Vec<PathBuf>,

    /// Output platform metadata file.
    #[arg(short, long)]
    pub output_file: PathBuf,
}
