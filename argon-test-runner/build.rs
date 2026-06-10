use std::env;
use std::path::{Path, PathBuf};
use std::process::Command;

fn main() {
    println!("cargo::rerun-if-changed=build.rs");
    println!("cargo::rerun-if-env-changed=PATH");

    let workspace_dir = PathBuf::from(env::var_os("CARGO_MANIFEST_DIR").unwrap())
        .parent()
        .unwrap()
        .to_owned();

    let api_dir = workspace_dir.join("backend/api/js");
    let backend_dir = workspace_dir.join("backend/backends/js");

    watch_js_package(&api_dir);
    watch_js_package(&backend_dir);
    println!(
        "cargo::rerun-if-changed={}",
        workspace_dir.join("backend/vm/vm.nidl").display(),
    );
    println!(
        "cargo::rerun-if-changed={}",
        workspace_dir
            .join("backend/api/nobleidl/metadata.nidl")
            .display(),
    );

    npm_ci(&api_dir);
    npm_run_build(&api_dir);

    npm_ci(&backend_dir);
    npm_run_build(&backend_dir);
}

fn watch_js_package(package_dir: &Path) {
    println!(
        "cargo::rerun-if-changed={}/package.json",
        package_dir.display()
    );
    println!(
        "cargo::rerun-if-changed={}/package-lock.json",
        package_dir.display()
    );
    println!(
        "cargo::rerun-if-changed={}/tsconfig.json",
        package_dir.display()
    );
    println!("cargo::rerun-if-changed={}/src", package_dir.display());
}

fn npm_run_build(package_dir: &Path) {
    npm(package_dir, &["run", "build"]);
}

fn npm_ci(package_dir: &Path) {
    npm(package_dir, &["ci"]);
}

fn npm(package_dir: &Path, args: &[&str]) {
    let status = Command::new("npm")
        .args(args)
        .current_dir(package_dir)
        .status()
        .unwrap_or_else(|err| {
            panic!(
                "Failed to start npm {} in {}: {}",
                args.join(" "),
                package_dir.display(),
                err,
            )
        });

    assert!(
        status.success(),
        "npm {} failed in {} with status {}",
        args.join(" "),
        package_dir.display(),
        status,
    );
}
