fn main() {
    let mut stdout = argon_tasks::local_io::StdIoWrite::new(std::io::stdout());
    let exit_code =
        futures_lite::future::block_on(argon_cli::main(std::env::args_os().collect(), &mut stdout));
    std::process::exit(exit_code);
}
