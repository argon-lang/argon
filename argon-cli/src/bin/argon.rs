use argon_compiler::TubeName;
use argon_runner::local_io::{
    LocalInputFile, LocalOutputDirectory, LocalOutputFile, LocalSourceDirectory,
};
use clap::{Args, Parser, Subcommand};
use std::path::PathBuf;

#[derive(Parser)]
#[command(name = "argon")]
struct CommandLineOptions {
    #[clap(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Compile Argon source code to a tube (.artube).
    Compile(CompileOptions),

    /// Generate Argon VM IR from a tube.
    #[command(name = "genir")]
    GenIR(GenIrOptions),

    /// Generate code from Argon VM IR.
    #[command(name = "codegen")]
    CodeGen(CodeGenCommand),
}

#[derive(Args, Debug)]
struct CompileOptions {
    /// The name of the tube.
    #[arg(short = 'n', long)]
    tube_name: TubeName,

    /// Input directories for source code
    #[arg(short, long = "input", required = true)]
    input_dirs: Vec<PathBuf>,

    /// Referenced tubes
    #[arg(short, long)]
    referenced_tubes: Vec<PathBuf>,

    /// Output tube file
    #[arg(short, long)]
    output_file: PathBuf,
}

#[derive(Args, Debug)]
struct GenIrOptions {
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

#[derive(Args, Debug)]
struct JsCodeGenOptions {
    /// Input IR file.
    #[arg(short, long)]
    input_file: PathBuf,

    /// Output directory for generated JS code.
    #[arg(short, long)]
    output_dir: PathBuf,
}

#[derive(Args)]
struct CodeGenCommand {
    #[clap(subcommand)]
    backend_command: CodeGenBackendCommand,
}

#[derive(Subcommand)]
enum CodeGenBackendCommand {
    /// Generate JavaScript NPM package.
    JS(JsCodeGenOptions),
}

fn main() {
    let options = CommandLineOptions::parse();
    match options.command {
        Command::Compile(cmd) => {
            let output_file = LocalOutputFile::new(cmd.output_file.clone());
            let runner_options = argon_runner::CompileOptions {
                tube_name: cmd.tube_name,
                input_dirs: cmd
                    .input_dirs
                    .into_iter()
                    .map(LocalSourceDirectory::new)
                    .collect(),
                referenced_tubes: cmd
                    .referenced_tubes
                    .into_iter()
                    .map(LocalInputFile::new)
                    .collect(),
                output_file: output_file.clone(),
            };

            if !argon_runner::compile(runner_options) {
                std::process::exit(1);
            }
            println!("Compilation succeeded.");
        }
        Command::GenIR(cmd) => {
            argon_runner::gen_ir(argon_runner::GenIrOptions {
                input_tube: LocalInputFile::new(cmd.input_tube),
                referenced_tubes: cmd
                    .referenced_tubes
                    .into_iter()
                    .map(LocalInputFile::new)
                    .collect(),
                output_file: LocalOutputFile::new(cmd.output_file),
            });
        }
        Command::CodeGen(cmd) => match cmd.backend_command {
            CodeGenBackendCommand::JS(js_cmd) => {
                argon_runner::codegen_js(argon_runner::JsCodeGenOptions {
                    input_file: LocalInputFile::new(js_cmd.input_file),
                    output_dir: LocalOutputDirectory::new(js_cmd.output_dir),
                });
            }
        },
    }
}
