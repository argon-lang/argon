use argon_runner::{CompileOptions, GenIrOptions, JsCodeGenOptions};
use clap::{Args, Parser, Subcommand};

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
            if !argon_runner::compile(cmd) {
                std::process::exit(1);
            }
        }
        Command::GenIR(cmd) => {
            argon_runner::gen_ir(cmd);
        }
        Command::CodeGen(cmd) => match cmd.backend_command {
            CodeGenBackendCommand::JS(js_cmd) => {
                argon_runner::codegen_js(js_cmd);
            }
        },
    }
}
