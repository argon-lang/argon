use std::collections::BTreeSet;
use std::env;
use std::ffi::{OsStr, OsString};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::thread::available_parallelism;

const EMSCRIPTEN_VERSION: &str = "6.0.9";
const Z3_VERSION: &str = "5.1.0";

fn main() {
    if let Err(error) = build_distribution() {
        eprintln!("argon-dist: {error}");
        std::process::exit(1);
    }
}

fn build_distribution() -> Result<(), String> {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let root = manifest_dir
        .parent()
        .ok_or_else(|| "the argon-dist manifest has no parent directory".to_owned())?
        .canonicalize()
        .map_err(|error| format!("failed to resolve the repository root: {error}"))?;

    let emsdk_dir = root.join(".emsdk").join(EMSCRIPTEN_VERSION);
    let z3_dir = root.join(".z3").join(Z3_VERSION);
    let z3_build_dir = z3_dir.join("build");

    build_binaries(&root)?;
    prepare_emscripten(&root, &emsdk_dir)?;

    let path = emscripten_path(&emsdk_dir)?;
    prepare_z3(&root, &z3_dir, &z3_build_dir, &path)?;
    build_wasm_and_backends(&root, &path)?;
    copy_distribution(&root, &z3_build_dir, &path)
}

fn build_binaries(root: &Path) -> Result<(), String> {
    for target in [
        "x86_64-unknown-linux-gnu.2.17",
        "i686-unknown-linux-gnu.2.17",
        "aarch64-unknown-linux-gnu.2.17",
    ] {
        run(
            root,
            OsStr::new("cargo"),
            &args(&[
                "zigbuild",
                "--bin",
                "argonc",
                "--release",
                "--target",
                target,
            ]),
            &[],
        )?;
    }

    Ok(())
}

fn prepare_emscripten(root: &Path, emsdk_dir: &Path) -> Result<(), String> {
    if !emsdk_dir.is_dir() {
        if let Some(parent) = emsdk_dir.parent() {
            fs::create_dir_all(parent)
                .map_err(|error| format!("failed to create {}: {error}", parent.display()))?;
        }

        run(
            root,
            OsStr::new("git"),
            &[
                OsString::from("clone"),
                OsString::from("--depth"),
                OsString::from("1"),
                OsString::from("--branch"),
                OsString::from(EMSCRIPTEN_VERSION),
                OsString::from("https://github.com/emscripten-core/emsdk.git"),
                emsdk_dir.as_os_str().to_owned(),
            ],
            &[],
        )?;
    }

    run(
        root,
        emsdk_dir.join("emsdk").as_os_str(),
        &args(&["install", EMSCRIPTEN_VERSION]),
        &[],
    )
}

fn emscripten_path(emsdk_dir: &Path) -> Result<OsString, String> {
    let current_path = env::var_os("PATH").unwrap_or_default();
    let mut paths = vec![
        emsdk_dir.join("upstream/emscripten").into_os_string(),
        emsdk_dir.join("upstream/bin").into_os_string(),
    ];
    paths.extend(env::split_paths(&current_path).map(PathBuf::into_os_string));
    env::join_paths(paths)
        .map_err(|error| format!("failed to construct the Emscripten PATH: {error}"))
}

fn prepare_z3(root: &Path, z3_dir: &Path, z3_build_dir: &Path, path: &OsStr) -> Result<(), String> {
    if !z3_dir.is_dir() {
        if let Some(parent) = z3_dir.parent() {
            fs::create_dir_all(parent)
                .map_err(|error| format!("failed to create {}: {error}", parent.display()))?;
        }

        run(
            root,
            OsStr::new("git"),
            &[
                OsString::from("clone"),
                OsString::from("--depth"),
                OsString::from("1"),
                OsString::from("--branch"),
                OsString::from(format!("z3-{Z3_VERSION}")),
                OsString::from("https://github.com/Z3Prover/z3.git"),
                z3_dir.as_os_str().to_owned(),
            ],
            &[],
        )?;
    }

    let path_env = [(OsStr::new("PATH"), path.to_owned())];

    if !z3_build_dir.join("Makefile").is_file() {
        run(
            z3_dir,
            OsStr::new("emconfigure"),
            &[
                OsString::from("python"),
                OsString::from("scripts/mk_make.py"),
                OsString::from("--staticlib"),
                OsString::from("--single-threaded"),
                OsString::from("--arm64=false"),
            ],
            &[
                (OsStr::new("PATH"), path.to_owned()),
                (
                    OsStr::new("CXXFLAGS"),
                    OsString::from("-fwasm-exceptions -sWASM_LEGACY_EXCEPTIONS=0"),
                ),
            ],
        )?;
    }

    if !z3_build_dir.join("libz3.a").is_file() {
        let jobs = available_parallelism().map_or_else(
            |_| "1".to_owned(),
            |parallelism| parallelism.get().to_string(),
        );
        run(
            root,
            OsStr::new("make"),
            &[
                OsString::from("-C"),
                z3_build_dir.as_os_str().to_owned(),
                OsString::from(format!("-j{jobs}")),
                OsString::from("libz3.a"),
            ],
            &path_env,
        )?;
    }

    if !z3_build_dir.join("z3.wasm").is_file() {
        let exported_functions = exported_z3_functions(z3_dir)?;
        run(
            z3_build_dir,
            OsStr::new("em++"),
            &[
                OsString::from("-g"),
                OsString::from("libz3.a"),
                OsString::from("--no-entry"),
                OsString::from("-fwasm-exceptions"),
                OsString::from("-sPURE_WASI=1"),
                OsString::from("-sWASM_LEGACY_EXCEPTIONS=0"),
                OsString::from("-sSTANDALONE_WASM=1"),
                OsString::from("-sINITIAL_MEMORY=2GB"),
                OsString::from("-sSTACK_SIZE=20MB"),
                OsString::from(format!("-sEXPORTED_FUNCTIONS={exported_functions}")),
                OsString::from("-o"),
                OsString::from("z3.wasm"),
            ],
            &[(OsStr::new("PATH"), path.to_owned())],
        )?;
    }

    Ok(())
}

fn exported_z3_functions(z3_dir: &Path) -> Result<String, String> {
    let api_dir = z3_dir.join("src/api");
    let mut functions = BTreeSet::from(["_free".to_owned(), "_malloc".to_owned()]);

    let entries = fs::read_dir(&api_dir)
        .map_err(|error| format!("failed to read {}: {error}", api_dir.display()))?;
    for entry in entries {
        let entry = entry.map_err(|error| format!("failed to read a Z3 API entry: {error}"))?;
        let path = entry.path();
        let Some(name) = path.file_name().and_then(OsStr::to_str) else {
            continue;
        };
        if !name.starts_with("z3") || path.extension() != Some(OsStr::new("h")) {
            continue;
        }

        let contents = fs::read_to_string(&path)
            .map_err(|error| format!("failed to read {}: {error}", path.display()))?;
        for line in contents.lines() {
            let Some(rest) = line.strip_prefix("Z3_API ").or_else(|| {
                line.find("Z3_API ")
                    .map(|index| &line[index + "Z3_API ".len()..])
            }) else {
                continue;
            };
            let Some(before_arguments) = rest.split_once('(').map(|(before, _)| before) else {
                continue;
            };
            let Some(function_name) = before_arguments.split_whitespace().last() else {
                continue;
            };
            if function_name.starts_with("Z3_")
                && function_name
                    .bytes()
                    .all(|byte| byte.is_ascii_alphanumeric() || byte == b'_')
            {
                functions.insert(format!("_{function_name}"));
            }
        }
    }

    Ok(functions.into_iter().collect::<Vec<_>>().join(","))
}

fn build_wasm_and_backends(root: &Path, path: &OsStr) -> Result<(), String> {
    run(
        root,
        OsStr::new("cargo"),
        &args(&[
            "build",
            "--package",
            "argon-cli",
            "--lib",
            "--release",
            "--target",
            "wasm32-unknown-unknown",
        ]),
        &[(OsStr::new("PATH"), path.to_owned())],
    )?;

    let wasm_dir = root.join("target/wasm32-unknown-unknown/release");
    let argonc_input_dir = wasm_dir.join("argon-dist");
    fs::create_dir_all(&argonc_input_dir)
        .map_err(|error| format!("failed to create {}: {error}", argonc_input_dir.display()))?;
    let argonc_wasm = argonc_input_dir.join("argonc.wasm");
    fs::copy(wasm_dir.join("argon_cli.wasm"), &argonc_wasm).map_err(|error| {
        format!(
            "failed to stage argon_cli.wasm as {}: {error}",
            argonc_wasm.display()
        )
    })?;
    move_argonc_memory(&argonc_wasm)?;
    let bindgen_dir = wasm_dir.join("wasm-bindgen");
    if bindgen_dir.exists() {
        fs::remove_dir_all(&bindgen_dir).map_err(|error| {
            format!(
                "failed to clear generated wasm-bindgen bindings {}: {error}",
                bindgen_dir.display()
            )
        })?;
    }
    let mut bindgen = wasm_bindgen_cli_support::Bindgen::new();
    bindgen.input_path(&argonc_wasm);
    bindgen.out_name("argon_wasm");
    bindgen.nodejs_module(true).map_err(|error| {
        format!("failed to select the Node.js ES module wasm-bindgen target: {error}")
    })?;
    bindgen
        .generate(&bindgen_dir)
        .map_err(|error| format!("failed to generate Node.js wasm-bindgen bindings: {error}"))?;
    patch_bindgen_imports(&bindgen_dir.join("argon_wasm.js"))?;

    let argonc_memory = generated_argonc_memory(&bindgen_dir.join("argon_wasm_bg.wasm"))?;
    compile_memory_bridge(root, &argonc_memory)?;

    for (directory, command) in [
        ("backend/js/api", "build"),
        ("backend/js/backend", "build"),
        ("backend/js/util/copy-deploy", "build"),
    ] {
        run(
            root,
            OsStr::new("npm"),
            &args(&["ci", "--prefix", directory]),
            &[(OsStr::new("PATH"), path.to_owned())],
        )?;
        run(
            root,
            OsStr::new("npm"),
            &args(&["run", command, "--prefix", directory]),
            &[(OsStr::new("PATH"), path.to_owned())],
        )?;
    }

    run(
        root,
        root.join("backend/jvm/gradlew").as_os_str(),
        &args(&["-p", "backend/jvm", ":backend:installDist"]),
        &[(OsStr::new("PATH"), path.to_owned())],
    )
}

fn move_argonc_memory(path: &Path) -> Result<(u64, Option<u64>), String> {
    let bytes =
        fs::read(path).map_err(|error| format!("failed to read {}: {error}", path.display()))?;
    let mut module = walrus::ModuleConfig::new()
        .parse(&bytes)
        .map_err(|error| format!("failed to parse {}: {error}", path.display()))?;

    let memory = module
        .memories
        .iter()
        .find(|memory| memory.import.is_none())
        .ok_or_else(|| format!("{} does not contain a local memory", path.display()))?;
    let memory_id = memory.id();
    let initial = memory.initial;
    let maximum = memory.maximum;
    let import = module.imports.add(
        "argon-memory",
        "memory",
        walrus::ImportKind::Memory(memory_id),
    );
    module.memories.get_mut(memory_id).import = Some(import);
    module
        .emit_wasm_file(path)
        .map_err(|error| format!("failed to rewrite {}: {error}", path.display()))?;

    Ok((initial, maximum))
}

fn generated_argonc_memory(path: &Path) -> Result<(u64, Option<u64>), String> {
    let bytes =
        fs::read(path).map_err(|error| format!("failed to read {}: {error}", path.display()))?;
    let module = walrus::ModuleConfig::new()
        .parse(&bytes)
        .map_err(|error| format!("failed to parse {}: {error}", path.display()))?;
    let mut memories = module
        .memories
        .iter()
        .filter(|memory| memory.import.is_some());
    let memory = memories
        .next()
        .ok_or_else(|| format!("{} does not import its linear memory", path.display()))?;
    if memories.next().is_some() {
        return Err(format!(
            "{} imports multiple linear memories",
            path.display()
        ));
    }
    Ok((memory.initial, memory.maximum))
}

fn compile_memory_bridge(root: &Path, argonc_memory: &(u64, Option<u64>)) -> Result<(), String> {
    let wat_path = root.join("scripts/argon-memory.wat");
    let wasm = wat::parse_file(&wat_path)
        .map_err(|error| format!("failed to compile {}: {error}", wat_path.display()))?;
    let output_dir = root.join("target/wasm32-unknown-unknown/release");
    let wasm_output = output_dir.join("argon-memory.wasm");
    fs::write(&wasm_output, wasm)
        .map_err(|error| format!("failed to write {}: {error}", wasm_output.display()))?;
    declare_argonc_memory(&wasm_output, argonc_memory)
}

fn declare_argonc_memory(path: &Path, memory_limits: &(u64, Option<u64>)) -> Result<(), String> {
    let bytes =
        fs::read(path).map_err(|error| format!("failed to read {}: {error}", path.display()))?;
    let mut module = walrus::ModuleConfig::new()
        .parse(&bytes)
        .map_err(|error| format!("failed to parse {}: {error}", path.display()))?;

    let memory = module
        .memories
        .iter()
        .find(|memory| {
            let Some(import) = memory.import else {
                return false;
            };
            let import = module.imports.get(import);
            import.module == "argonc" && import.name == "memory"
        })
        .ok_or_else(|| {
            format!(
                "{} does not contain the dummy argonc memory import",
                path.display()
            )
        })?;
    let memory_id = memory.id();
    let import_id = memory
        .import
        .expect("the selected dummy argonc memory must be imported");

    let memory = module.memories.get_mut(memory_id);
    memory.initial = memory_limits.0;
    memory.maximum = memory_limits.1;
    memory.import = None;
    module.imports.delete(import_id);
    if module.exports.iter().all(|export| export.name != "memory") {
        module.exports.add("memory", memory_id);
    }

    module
        .emit_wasm_file(path)
        .map_err(|error| format!("failed to rewrite {}: {error}", path.display()))
}

fn patch_bindgen_imports(path: &Path) -> Result<(), String> {
    let source = fs::read_to_string(path)
        .map_err(|error| format!("failed to read {}: {error}", path.display()))?;
    let name = path
        .file_stem()
        .ok_or_else(|| format!("wasm-bindgen output has no file name: {}", path.display()))?
        .to_string_lossy();
    let table_start = source
        .find(&format!(
            "    return {{\n        __proto__: null,\n        \"./{name}_bg.js\": import0,"
        ))
        .ok_or_else(|| {
            format!(
                "wasm-bindgen output {} did not contain its expected import table",
                path.display()
            )
        })?;
    let table_end = source[table_start..]
        .find("\n    };\n}")
        .map(|offset| table_start + offset)
        .ok_or_else(|| {
            format!(
                "wasm-bindgen output {} had an unterminated import table",
                path.display()
            )
        })?;
    let mut source = source;
    source.insert_str(table_end, "\n        ...globalThis.__argon_wasm_imports,");
    let default_imports = "__wbg_get_imports())";
    let shared_imports =
        "__wbg_get_imports(globalThis.__argon_wasm_imports[\"argon-memory\"].memory))";
    if !source.contains(default_imports) {
        return Err(format!(
            "wasm-bindgen output {} did not contain its expected instantiation",
            path.display()
        ));
    }
    source = source.replace(default_imports, shared_imports);
    let mut patched = String::with_capacity(source.len());
    for line in source.split_inclusive('\n') {
        let Some(import) = line.strip_prefix("import * as ") else {
            patched.push_str(line);
            continue;
        };
        let Some((binding, module)) = import.split_once(" from \"") else {
            patched.push_str(line);
            continue;
        };
        let Some(module) = module
            .strip_suffix("\"\n")
            .or_else(|| module.strip_suffix('"'))
        else {
            patched.push_str(line);
            continue;
        };
        if module.starts_with('.') {
            patched.push_str(line);
        } else {
            patched.push_str(&format!(
                "const {binding} = globalThis.__argon_wasm_imports[{module:?}];\n"
            ));
        }
    }
    fs::write(path, patched).map_err(|error| format!("failed to write {}: {error}", path.display()))
}

fn copy_distribution(root: &Path, z3_build_dir: &Path, path: &OsStr) -> Result<(), String> {
    let dist = root.join("dist");
    if dist.exists() {
        fs::remove_dir_all(&dist)
            .map_err(|error| format!("failed to remove {}: {error}", dist.display()))?;
    }

    for directory in [
        "arch/x86_64-unknown-linux-gnu",
        "arch/i686-unknown-linux-gnu",
        "arch/aarch64-unknown-linux-gnu",
        "arch/wasm32-unknown-unknown",
        "backend/jvm",
        "bin",
    ] {
        fs::create_dir_all(dist.join(directory))
            .map_err(|error| format!("failed to create dist/{directory}: {error}"))?;
    }

    for (source, destination) in [
        (
            "target/x86_64-unknown-linux-gnu/release/argonc",
            "arch/x86_64-unknown-linux-gnu/argonc",
        ),
        (
            "target/i686-unknown-linux-gnu/release/argonc",
            "arch/i686-unknown-linux-gnu/argonc",
        ),
        (
            "target/aarch64-unknown-linux-gnu/release/argonc",
            "arch/aarch64-unknown-linux-gnu/argonc",
        ),
        ("scripts/argonc.js", "arch/wasm32-unknown-unknown/argonc.js"),
    ] {
        copy_file(&root.join(source), &dist.join(destination))?;
    }
    copy_directory(
        &root.join("target/wasm32-unknown-unknown/release/wasm-bindgen"),
        &dist.join("arch/wasm32-unknown-unknown"),
    )?;
    copy_file(
        &z3_build_dir.join("z3.wasm"),
        &dist.join("arch/wasm32-unknown-unknown/z3.wasm"),
    )?;
    copy_file(
        &root.join("target/wasm32-unknown-unknown/release/argon-memory.wasm"),
        &dist.join("arch/wasm32-unknown-unknown/argon-memory.wasm"),
    )?;

    run(
        root,
        OsStr::new("node"),
        &[
            root.join("backend/js/util/copy-deploy/lib/main.js")
                .as_os_str()
                .to_owned(),
            root.join("backend/js/backend").as_os_str().to_owned(),
            dist.join("backend/js").as_os_str().to_owned(),
        ],
        &[(OsStr::new("PATH"), path.to_owned())],
    )?;

    copy_jars(
        &root.join("backend/jvm/backend/build/install/backend/lib"),
        &dist.join("backend/jvm"),
    )?;

    let launcher = root.join("scripts/argonc");
    let installed_launcher = dist.join("bin/argonc");
    copy_file(&launcher, &installed_launcher)?;
    make_executable(&launcher)?;
    make_executable(&installed_launcher)
}

fn copy_directory(source: &Path, destination: &Path) -> Result<(), String> {
    fs::create_dir_all(destination)
        .map_err(|error| format!("failed to create {}: {error}", destination.display()))?;
    let entries = fs::read_dir(source)
        .map_err(|error| format!("failed to read {}: {error}", source.display()))?;
    for entry in entries {
        let entry =
            entry.map_err(|error| format!("failed to read a generated wasm binding: {error}"))?;
        let source_path = entry.path();
        let destination_path = destination.join(entry.file_name());
        if source_path.is_dir() {
            copy_directory(&source_path, &destination_path)?;
        } else {
            copy_file(&source_path, &destination_path)?;
        }
    }
    Ok(())
}

fn copy_file(source: &Path, destination: &Path) -> Result<(), String> {
    fs::copy(source, destination).map_err(|error| {
        format!(
            "failed to copy {} to {}: {error}",
            source.display(),
            destination.display()
        )
    })?;

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;

        let mode = fs::metadata(source)
            .map_err(|error| format!("failed to stat {}: {error}", source.display()))?
            .permissions()
            .mode();
        let mut permissions = fs::metadata(destination)
            .map_err(|error| format!("failed to stat {}: {error}", destination.display()))?
            .permissions();
        permissions.set_mode(mode);
        fs::set_permissions(destination, permissions).map_err(|error| {
            format!(
                "failed to set permissions on {}: {error}",
                destination.display()
            )
        })?;
    }

    Ok(())
}

fn copy_jars(source_dir: &Path, destination_dir: &Path) -> Result<(), String> {
    let entries = fs::read_dir(source_dir)
        .map_err(|error| format!("failed to read {}: {error}", source_dir.display()))?;
    let mut copied = false;
    for entry in entries {
        let entry = entry.map_err(|error| format!("failed to read a JVM library: {error}"))?;
        let source = entry.path();
        if source.is_file() && source.extension() == Some(OsStr::new("jar")) {
            copy_file(&source, &destination_dir.join(entry.file_name()))?;
            copied = true;
        }
    }

    if copied {
        Ok(())
    } else {
        Err(format!(
            "no JVM libraries found in {}",
            source_dir.display()
        ))
    }
}

#[cfg(unix)]
fn make_executable(path: &Path) -> Result<(), String> {
    use std::os::unix::fs::PermissionsExt;

    let mut permissions = fs::metadata(path)
        .map_err(|error| format!("failed to stat {}: {error}", path.display()))?
        .permissions();
    permissions.set_mode(permissions.mode() | 0o111);
    fs::set_permissions(path, permissions)
        .map_err(|error| format!("failed to make {} executable: {error}", path.display()))
}

#[cfg(not(unix))]
fn make_executable(_path: &Path) -> Result<(), String> {
    Ok(())
}

fn args(values: &[&str]) -> Vec<OsString> {
    values.iter().map(OsString::from).collect()
}

fn run(
    cwd: &Path,
    program: &OsStr,
    arguments: &[OsString],
    environment: &[(&OsStr, OsString)],
) -> Result<(), String> {
    let mut command = Command::new(program);
    command.current_dir(cwd).args(arguments);
    for (key, value) in environment {
        command.env(key, value);
    }

    let status = command.status().map_err(|error| {
        format!(
            "failed to execute {} in {}: {error}",
            program.to_string_lossy(),
            cwd.display()
        )
    })?;
    if status.success() {
        Ok(())
    } else {
        Err(format!(
            "{} exited with status {status}",
            program.to_string_lossy()
        ))
    }
}

#[cfg(test)]
#[allow(
    clippy::unwrap_used,
    reason = "The post-processing tests use unwrap for concise fixture setup."
)]
mod tests {
    use super::*;

    #[test]
    fn move_argonc_memory_rewrites_a_local_memory() {
        let path = std::env::temp_dir().join(format!(
            "argon-dist-memory-test-{}.wasm",
            std::process::id()
        ));
        fs::write(
            &path,
            wat::parse_str("(module (memory 3) (export \"memory\" (memory 0)))").unwrap(),
        )
        .unwrap();

        let memory = move_argonc_memory(&path).unwrap();
        assert_eq!(memory, (3, None));

        let module = walrus::ModuleConfig::new()
            .parse(&fs::read(&path).unwrap())
            .unwrap();
        let memory = module.memories.iter().next().unwrap();
        assert!(memory.import.is_some());
        let import = module.imports.get(memory.import.unwrap());
        assert_eq!(import.module, "argon-memory");
        assert_eq!(import.name, "memory");

        let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .to_path_buf();
        compile_memory_bridge(&root, &(3, None)).unwrap();
        let bridge =
            fs::read(root.join("target/wasm32-unknown-unknown/release/argon-memory.wasm")).unwrap();
        let bridge = walrus::ModuleConfig::new().parse(&bridge).unwrap();
        let bridge_memory = bridge
            .memories
            .iter()
            .find(|memory| memory.import.is_none())
            .unwrap();
        assert_eq!(bridge_memory.initial, 3);
        assert!(bridge_memory.import.is_none());
        assert!(bridge.exports.iter().any(|export| export.name == "memory"));

        fs::remove_file(path).unwrap();
    }

    #[test]
    fn bindgen_handles_the_library_wasm() {
        let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .to_path_buf();
        let input = root.join("target/wasm32-unknown-unknown/release/argon_cli.wasm");
        if !input.is_file() {
            return;
        }

        let temporary =
            std::env::temp_dir().join(format!("argon-dist-bindgen-test-{}", std::process::id()));
        fs::create_dir_all(&temporary).unwrap();
        let input_copy = temporary.join("argon_cli.wasm");
        fs::copy(input, &input_copy).unwrap();
        move_argonc_memory(&input_copy).unwrap();

        let output = temporary.join("output");
        let mut bindgen = wasm_bindgen_cli_support::Bindgen::new();
        bindgen.input_path(&input_copy).out_name("argon_wasm");
        bindgen.nodejs_module(true).unwrap();
        bindgen.generate(&output).unwrap();
        patch_bindgen_imports(&output.join("argon_wasm.js")).unwrap();
        let memory = generated_argonc_memory(&output.join("argon_wasm_bg.wasm")).unwrap();
        compile_memory_bridge(&root, &memory).unwrap();

        let generated = fs::read_to_string(output.join("argon_wasm.js")).unwrap();
        assert!(generated.contains("...globalThis.__argon_wasm_imports"));
        assert!(!generated.contains(" from \"z3\""));
        assert!(!generated.contains(" from \"argon-memory\""));
        let bridge = root.join("target/wasm32-unknown-unknown/release/argon-memory.wasm");
        let script = temporary.join("load.js");
        fs::write(
            &script,
            format!(
                "const fs = require('node:fs');\n\
                 const wasm = fs.readFileSync({:?});\n\
                 const z3 = new WebAssembly.Memory({{ initial: 1 }});\n\
                 const bridge = new WebAssembly.Instance(new WebAssembly.Module(wasm), {{ z3: {{ memory: z3 }} }});\n\
                 const z3Exports = new Proxy({{}}, {{ get: () => () => 0 }});\n\
                 globalThis.__argon_wasm_imports = {{ 'argon-memory': bridge.exports, z3: z3Exports }};\n\
                 const argonc = require({:?});\n\
                 if (typeof argonc.main !== 'function') process.exit(1);\n",
                bridge,
                output.join("argon_wasm.js"),
            ),
        )
        .unwrap();
        let status = Command::new("node").arg(&script).status().unwrap();
        assert!(status.success());
        fs::remove_dir_all(temporary).unwrap();
    }
}
