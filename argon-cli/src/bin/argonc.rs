#[tokio::main]
async fn main() {
    let mut stdout = argon_tasks::local_io::StdIoWrite::new(std::io::stdout());
    let mut stderr = argon_tasks::local_io::StdIoWrite::new(std::io::stderr());
    let exit_code = argon_cli::main(std::env::args_os().collect(), &mut stdout, &mut stderr).await;
    std::process::exit(exit_code);
}
