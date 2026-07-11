use argon_cli::backend::Backend;
use argon_cli::options::{
    CodeGenBackendCommand, Command, CommandLineOptions, PlatformMetadataBackendCommand,
};
use argon_runner::local_io::{LocalInputFile, LocalOutputFile, LocalSourceDirectory, StdIoWrite};
use clap::Parser;

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
                platform_metadata: cmd.platform.into_iter().map(LocalInputFile::new).collect(),
                output_file: output_file.clone(),
            };

            let mut stderr = StdIoWrite::new(std::io::stderr());
            if !argon_runner::compile(runner_options, &mut stderr) {
                std::process::exit(1);
            }
        }
        Command::GenIR(cmd) => {
            let runner_options = argon_runner::GenIrOptions {
                input_tube: LocalInputFile::new(cmd.input_tube),
                referenced_tubes: cmd
                    .referenced_tubes
                    .into_iter()
                    .map(LocalInputFile::new)
                    .collect(),
                output_file: LocalOutputFile::new(cmd.output_file),
                platform: cmd.platform,
            };

            let mut stderr = StdIoWrite::new(std::io::stderr());
            if !argon_runner::gen_ir(runner_options, &mut stderr) {
                std::process::exit(1);
            }
        }
        Command::Optimize(cmd) => {
            let runner_options = argon_runner::OptimizeOptions {
                input_file: LocalInputFile::new(cmd.input),
                referenced_tubes: cmd
                    .referenced_tubes
                    .into_iter()
                    .map(LocalInputFile::new)
                    .collect(),
                output_file: LocalOutputFile::new(cmd.output),
                optimizations: cmd.optimizations,
            };

            let mut stderr = StdIoWrite::new(std::io::stderr());
            if !argon_runner::optimize(runner_options, &mut stderr) {
                std::process::exit(1);
            }
        }
        Command::CodeGen(cmd) => match cmd.backend_command {
            CodeGenBackendCommand::JS(_) => {
                execute_backend_subcommand(Backend::JavaScript);
            }
            CodeGenBackendCommand::JVM(_) => {
                execute_backend_subcommand(Backend::JVM);
            }
        },
        Command::PlatformMetadata(cmd) => match cmd.backend_command {
            PlatformMetadataBackendCommand::JS(_) => {
                execute_backend_subcommand(Backend::JavaScript);
            }
            PlatformMetadataBackendCommand::JVM(_) => {
                execute_backend_subcommand(Backend::JVM);
            }
        },
    }
}

#[cfg(unix)]
fn execute_backend_subcommand(backend: Backend) {
    use std::os::unix::process::CommandExt;
    use std::process::Command;

    let mut command;

    let mut executable_path =
        std::env::current_exe().expect("failed to get current executable path");
    executable_path.pop();
    executable_path.pop();
    executable_path.pop();

    match backend {
        Backend::JavaScript => {
            executable_path.push("backend/js/lib/main.js");

            command = Command::new("node");
            command.arg(executable_path);
        }
        Backend::JVM => {
            executable_path.push("backend/jvm");

            command = Command::new("java");
            command
                .arg("--module-path")
                .arg(executable_path)
                .arg("--module")
                .arg("dev.argon.backend");
        }
    }

    command.args(std::env::args_os().skip(1));
    let error = command.exec();

    eprintln!("unable to execute backend subcommand: {}", error);
    std::process::exit(1);
}

#[cfg(not(unix))]
fn execute_backend_subcommand(_backend: Backend) {
    eprintln!("backend subcommands are not implemented on this platform");
}
