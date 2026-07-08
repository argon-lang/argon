use std::ffi::OsString;
use std::path::PathBuf;

pub(crate) fn js_platform_metadata_args(
    extern_files: Vec<PathBuf>,
    output_file: PathBuf,
) -> Vec<OsString> {
    let mut args = vec![OsString::from("platform-metadata"), OsString::from("js")];

    for extern_file in extern_files {
        args.push(OsString::from("--extern"));
        args.push(extern_file.into_os_string());
    }

    args.push(OsString::from("--output-file"));
    args.push(output_file.into_os_string());

    args
}

pub(crate) fn js_codegen_args(input_file: PathBuf, output_dir: PathBuf, executable: Option<&str>) -> Vec<OsString> {
    let mut args = vec![
        OsString::from("codegen"),
        OsString::from("js"),
        OsString::from("--input"),
        input_file.into_os_string(),
        OsString::from("--output"),
        output_dir.into_os_string(),
    ];

    if let Some(executable_name) = executable {
        args.push(OsString::from("--executable"));
        args.push(OsString::from(executable_name));
    }

    args
}
