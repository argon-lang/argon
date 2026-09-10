#[tokio::main]
async fn main() {
    let mut stdout = argon_tasks::local_io::StdIoWrite::new(std::io::stdout());
    let exit_code = argon_cli::main(std::env::args_os().collect(), &mut stdout).await;
    std::process::exit(exit_code);
}
