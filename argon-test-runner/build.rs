use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

fn main() {
    println!("cargo::rerun-if-changed=build.rs");
    println!("cargo::rerun-if-env-changed=PATH");

    let workspace_dir = PathBuf::from(env::var_os("CARGO_MANIFEST_DIR").unwrap())
        .parent()
        .unwrap()
        .to_owned();

    let api_dir = workspace_dir.join("backend/js/api");
    let backend_dir = workspace_dir.join("backend/js/backend");
    let jvm_dir = workspace_dir.join("backend/jvm");
    let out_dir = PathBuf::from(env::var_os("OUT_DIR").unwrap());

    watch_js_package(&api_dir);
    watch_js_package(&backend_dir);
    watch_jvm_project(&jvm_dir);
    println!(
        "cargo::rerun-if-changed={}",
        workspace_dir.join("backend/nobleidl/vm/vm.nidl").display(),
    );
    println!(
        "cargo::rerun-if-changed={}",
        workspace_dir
            .join("backend/nobleidl/api/metadata.nidl")
            .display(),
    );
    println!(
        "cargo::rerun-if-changed={}",
        workspace_dir
            .join("backend/nobleidl/js/js-platform-metadata.nidl")
            .display(),
    );

    npm_install_if_needed(&api_dir, &out_dir, "api-js");
    npm_run_build(&api_dir);

    npm_install_if_needed(&backend_dir, &out_dir, "backend-js");
    npm_run_build(&backend_dir);

    gradle(
        &jvm_dir,
        &[
            ":backend:installDist",
            ":extern-compiler:installDist",
            ":runtime:jar",
        ],
    );
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

fn watch_jvm_project(project_dir: &Path) {
    println!(
        "cargo::rerun-if-changed={}/settings.gradle.kts",
        project_dir.display()
    );
    println!(
        "cargo::rerun-if-changed={}/build.gradle.kts",
        project_dir.display()
    );
    println!(
        "cargo::rerun-if-changed={}/gradle.properties",
        project_dir.display()
    );
    println!(
        "cargo::rerun-if-changed={}/gradle/libs.versions.toml",
        project_dir.display()
    );
    println!(
        "cargo::rerun-if-changed={}/api/build.gradle.kts",
        project_dir.display()
    );
    println!(
        "cargo::rerun-if-changed={}/api/src/main/java",
        project_dir.display()
    );
    println!(
        "cargo::rerun-if-changed={}/backend/build.gradle.kts",
        project_dir.display()
    );
    println!(
        "cargo::rerun-if-changed={}/backend/src/main/java",
        project_dir.display()
    );
    println!(
        "cargo::rerun-if-changed={}/extern-compiler/build.gradle.kts",
        project_dir.display()
    );
    println!(
        "cargo::rerun-if-changed={}/extern-compiler/src/main/java",
        project_dir.display()
    );
    println!(
        "cargo::rerun-if-changed={}/runtime/build.gradle.kts",
        project_dir.display()
    );
    println!(
        "cargo::rerun-if-changed={}/runtime/src/main/java",
        project_dir.display()
    );
}

fn npm_install_if_needed(package_dir: &Path, out_dir: &Path, package_name: &str) {
    let stamp_path = out_dir.join(format!("{package_name}.npm-install-stamp"));
    let package_hash = package_install_hash(package_dir);
    let previous_hash = fs::read_to_string(&stamp_path).ok();
    let node_modules_dir = package_dir.join("node_modules");

    if previous_hash.as_deref() == Some(package_hash.as_str()) && node_modules_dir.is_dir() {
        return;
    }

    npm(package_dir, &["install"]);
    let package_hash = package_install_hash(package_dir);
    fs::write(&stamp_path, package_hash).unwrap_or_else(|err| {
        panic!(
            "Failed to write npm install stamp {}: {}",
            stamp_path.display(),
            err,
        )
    });
}

fn package_install_hash(package_dir: &Path) -> String {
    let mut hash = 0xcbf29ce484222325u64;

    for file_name in ["package.json", "package-lock.json"] {
        fnv1a_hash(file_name.as_bytes(), &mut hash);

        let file_path = package_dir.join(file_name);
        let contents = fs::read(&file_path).unwrap_or_else(|err| {
            panic!("Failed to read {}: {}", file_path.display(), err);
        });

        fnv1a_hash(&contents, &mut hash);
    }

    format!("{hash:016x}")
}

fn fnv1a_hash(bytes: &[u8], hash: &mut u64) {
    for byte in bytes {
        *hash ^= u64::from(*byte);
        *hash = hash.wrapping_mul(0x100000001b3);
    }
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

fn gradle(project_dir: &Path, args: &[&str]) {
    let status = Command::new(project_dir.join("gradlew"))
        .args(args)
        .current_dir(project_dir)
        .status()
        .unwrap_or_else(|err| {
            panic!(
                "Failed to start gradle {} in {}: {}",
                args.join(" "),
                project_dir.display(),
                err,
            )
        });

    assert!(
        status.success(),
        "gradle {} failed in {} with status {}",
        args.join(" "),
        project_dir.display(),
        status,
    );
}
