use std::path::PathBuf;
use clap::{Args, Parser, Subcommand};
use argon_compiler::TubeName;

#[derive(Parser)]
#[command(name = "argon")]
struct CommandLineOptions {
    #[clap(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Compile Argon source code to a tube (.artube).
    Compile(CompileCommand),

    /// Generate Argon VM IR from a tube.
    #[command(name = "genir")]
    GenIR(GenIRCommand),

    /// Generate code from Argon VM IR.
    #[command(name = "codegen")]
    CodeGen(CodeGenCommand),
}

#[derive(Args)]
struct CompileCommand {
    /// The name of the tube.
    #[arg(short = 'n', long)]
    tube_name: TubeName,

    /// Input directories for source code
    #[arg(short, long)]
    input_dirs: Vec<PathBuf>,

    /// Referenced tubes
    #[arg(short, long)]
    referenced_tubes: Vec<PathBuf>,

    /// Output tube file
    #[arg(short, long)]
    output_file: PathBuf,
}

#[derive(Args)]
struct GenIRCommand {
    /// Input tube file
    #[arg(short, long)]
    input_tube: PathBuf,

    /// Referenced tubes
    #[arg(short, long)]
    referenced_tubes: Vec<PathBuf>,

    /// Output Argon VM IR file
    #[arg(short, long)]
    output_file: PathBuf,
}

#[derive(Args)]
struct CodeGenCommand {
    #[clap(subcommand)]
    backend_command: CodeGenBackendCommand,
}

#[derive(Subcommand)]
enum CodeGenBackendCommand {
    /// Generate JavaScript NPM package.
    JS(JSBackendCommand),
}

#[derive(Args)]
struct JSBackendCommand {
    /// Input IR file.
    #[arg(short, long)]
    input_file: PathBuf,

    /// Output directory for generated JS code.
    #[arg(short, long)]
    output_dir: PathBuf,
}


fn main() {
    let options = CommandLineOptions::parse();
    match options.command {
        Command::Compile(cmd) => {
            println!("Compiling with input directories: {:?}", cmd.input_dirs);
        }
        Command::GenIR(cmd) => {
            println!("Generating IR with input tube: {:?}", cmd.input_tube);
        }
        Command::CodeGen(cmd) => {
            match cmd.backend_command {
                CodeGenBackendCommand::JS(js_cmd) => {
                    println!("Generating JS code with input IR file");
                }
            }
        }
    }
}
