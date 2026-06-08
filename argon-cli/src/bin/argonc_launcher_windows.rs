#[cfg(not(windows))]
fn main() {
    eprintln!("argonc_launcher_windows is only supported on Windows");
    std::process::exit(1);
}

#[cfg(windows)]
fn main() {
    use argon_cli::backend::Backend;
    use argon_cli::options::{
        CodeGenBackendCommand, Command, CommandLineOptions, PlatformMetadataBackendCommand,
    };
    use clap::Parser;
    use std::ffi::OsString;
    use std::os::windows::ffi::OsStringExt;
    use std::os::windows::process::CommandExt;
    use std::path::PathBuf;
    use std::process::Command as ProcessCommand;
    use windows_sys::Win32::System::Environment::GetCommandLineW;
    use windows_sys::Win32::System::SystemInformation::{
        GetNativeSystemInfo, PROCESSOR_ARCHITECTURE_AMD64, PROCESSOR_ARCHITECTURE_ARM64,
        SYSTEM_INFO,
    };

    let options = CommandLineOptions::parse();

    let mut executable_path =
        std::env::current_exe().expect("failed to get current executable path");
    executable_path.pop();

    fn backend_command(backend: Backend, mut executable_path: PathBuf) -> ProcessCommand {
        match backend {
            Backend::JavaScript => {
                executable_path.push("backend/js/lib/main.js");
                let mut command = ProcessCommand::new("node");
                command.arg(executable_path);
                command
            }
        }
    }

    let mut command = match options.command {
        Command::Compile(_) | Command::GenIR(_) => {
            let arch_name = unsafe {
                let mut info = SYSTEM_INFO::default();
                GetNativeSystemInfo(&mut info);

                match info.Anonymous.Anonymous.wProcessorArchitecture {
                    PROCESSOR_ARCHITECTURE_AMD64 => "x86_64-pc-windows-gnu",
                    PROCESSOR_ARCHITECTURE_ARM64 => "aarch64-pc-windows-gnu",
                    _ => "i686-pc-windows-gnu",
                }
            };

            executable_path.push("arch");
            executable_path.push(arch_name);
            executable_path.push("argonc.exe");

            ProcessCommand::new(executable_path)
        }

        Command::CodeGen(cmd) => match cmd.backend_command {
            CodeGenBackendCommand::JS(_) => backend_command(Backend::JavaScript, executable_path),
        },
        Command::PlatformMetadata(cmd) => match cmd.backend_command {
            PlatformMetadataBackendCommand::JS(_) => {
                backend_command(Backend::JavaScript, executable_path)
            }
        },
    };

    let command_line = unsafe {
        let ptr = GetCommandLineW();

        let mut len = 0;
        while *ptr.add(len) != 0 {
            len += 1;
        }

        OsString::from_wide(std::slice::from_raw_parts(ptr, len))
    };

    command.raw_arg(command_line);

    let mut child = command.spawn().expect("failed to spawn child process");
    let exit_status = child.wait().expect("failed to wait for child process");
    std::process::exit(exit_status.code().unwrap_or(1));
}
