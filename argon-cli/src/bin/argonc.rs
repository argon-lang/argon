fn main() {
    let mut stdout = argon_tasks::local_io::StdIoWrite::new(std::io::stdout());
    argon_cli::main(std::env::args_os().collect(), &mut stdout);
}
