use std::ffi::OsString;
use std::path::PathBuf;

pub(crate) fn platform_metadata_args(
    platform: &str,
    extern_files: Vec<PathBuf>,
    output_file: PathBuf,
) -> Vec<OsString> {
    let mut args = vec![
        OsString::from("platform-metadata"),
        OsString::from(platform),
    ];

    for extern_file in extern_files {
        args.push(OsString::from("--extern"));
        args.push(extern_file.into_os_string());
    }

    args.push(OsString::from("--output-file"));
    args.push(output_file.into_os_string());

    args
}

pub(crate) fn js_platform_metadata_args(
    extern_files: Vec<PathBuf>,
    output_file: PathBuf,
) -> Vec<OsString> {
    platform_metadata_args("js", extern_files, output_file)
}

pub(crate) fn codegen_args(
    platform: &str,
    input_file: PathBuf,
    output: PathBuf,
    executable: bool,
) -> Vec<OsString> {
    let mut args = vec![
        OsString::from("codegen"),
        OsString::from(platform),
        OsString::from("--input"),
        input_file.into_os_string(),
        OsString::from("--output"),
        output.into_os_string(),
    ];

    if executable {
        args.push(OsString::from("--executable"));
    }

    args
}

pub(crate) fn js_codegen_args(
    input_file: PathBuf,
    output_dir: PathBuf,
    executable: Option<&str>,
) -> Vec<OsString> {
    let mut args = codegen_args("js", input_file, output_dir, false);

    if let Some(executable_name) = executable {
        args.push(OsString::from("--executable"));
        args.push(OsString::from(executable_name));
    }

    args
}

pub(crate) fn jvm_platform_metadata_args(
    extern_files: Vec<PathBuf>,
    output_file: PathBuf,
) -> Vec<OsString> {
    platform_metadata_args("jvm", extern_files, output_file)
}

pub(crate) fn jvm_codegen_args(
    input_file: PathBuf,
    output_file: PathBuf,
    executable: bool,
) -> Vec<OsString> {
    codegen_args("jvm", input_file, output_file, executable)
}

pub(crate) fn perl_platform_metadata_args(
    extern_files: Vec<PathBuf>,
    output_file: PathBuf,
) -> Vec<OsString> {
    platform_metadata_args("perl", extern_files, output_file)
}

pub(crate) fn perl_codegen_args(
    input_file: PathBuf,
    output_dir: PathBuf,
    executable: Option<&str>,
) -> Vec<OsString> {
    let mut args = codegen_args("perl", input_file, output_dir, false);
    if let Some(name) = executable {
        args.push(OsString::from("--executable"));
        args.push(OsString::from(name));
    }
    args
}
