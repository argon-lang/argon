use argon_io::{
    InputDirectory, InputFile, InputStream, OutputDirectory, OutputFile, OutputStream, Read, Write,
};
use std::path::{Path, PathBuf};

pub(crate) fn stage_input_dirs<ID>(
    temp_path: &Path,
    input_dirs: Vec<ID>,
) -> Result<Vec<PathBuf>, String>
where
    ID: InputDirectory,
{
    input_dirs
        .into_iter()
        .enumerate()
        .map(|(dir_index, input_dir)| {
            let staged_dir = temp_path.join(format!("input-{dir_index}"));
            std::fs::create_dir_all(&staged_dir).map_err(|err| {
                format!(
                    "failed to create staged input directory {}: {err}",
                    staged_dir.display()
                )
            })?;

            let files = tokio::runtime::Runtime::new()
                .map_err(|err| err.to_string())?
                .block_on(input_dir.list_files());
            for (file_index, input_file) in files.into_iter().enumerate() {
                let input_file = input_file.map_err(|err| err.to_string())?;
                let staged_file = staged_dir.join(staged_source_file_name(file_index, &input_file));
                copy_input_file_to_path(input_file, &staged_file)?;
            }

            Ok(staged_dir)
        })
        .collect()
}

pub(crate) fn stage_input_files<IF>(
    temp_path: &Path,
    name: &str,
    input_files: Vec<IF>,
) -> Result<Vec<PathBuf>, String>
where
    IF: InputFile,
{
    input_files
        .into_iter()
        .enumerate()
        .map(|(index, input_file)| stage_input_file(temp_path, name, index, input_file))
        .collect()
}

pub(crate) fn stage_input_file<IF>(
    temp_path: &Path,
    name: &str,
    index: usize,
    input_file: IF,
) -> Result<PathBuf, String>
where
    IF: InputFile,
{
    let staged_file = temp_path.join(staged_file_name(name, index, &input_file));
    copy_input_file_to_path(input_file, &staged_file)?;
    Ok(staged_file)
}

pub(crate) fn copy_temp_output<O>(input_path: &Path, output_file: O) -> Result<(), String>
where
    O: OutputFile,
{
    let mut reader = std::fs::File::open(input_path).map_err(|err| {
        format!(
            "failed to open temporary output {}: {err}",
            input_path.display()
        )
    })?;
    let runtime = tokio::runtime::Runtime::new().map_err(|err| err.to_string())?;
    runtime.block_on(async {
        let mut writer = output_file.open().await.map_err(|err| err.to_string())?;
        copy_std_reader_to_writer(&mut reader, &mut writer).await?;
        writer.close().await.map_err(|err| err.to_string())
    })
}

pub(crate) fn copy_temp_output_dir<O>(input_dir: &Path, output_dir: O) -> Result<(), String>
where
    O: OutputDirectory,
{
    for entry in walkdir::WalkDir::new(input_dir) {
        let entry = entry.map_err(|err| err.to_string())?;
        if !entry.file_type().is_file() {
            continue;
        }

        let path = entry.path();
        let relative_path = path.strip_prefix(input_dir).map_err(|err| {
            format!(
                "failed to compute relative output path for {}: {err}",
                path.display()
            )
        })?;
        let relative_path = relative_path.to_str().ok_or_else(|| {
            format!(
                "generated output path is not valid UTF-8: {}",
                relative_path.display()
            )
        })?;

        let mut reader = std::fs::File::open(path)
            .map_err(|err| format!("failed to open generated output {}: {err}", path.display()))?;
        let output_file = output_dir
            .create_file(relative_path)
            .map_err(|err| err.to_string())?;
        let runtime = tokio::runtime::Runtime::new().map_err(|err| err.to_string())?;
        runtime
            .block_on(async {
                let mut writer = output_file.open().await.map_err(|err| err.to_string())?;
                copy_std_reader_to_writer(&mut reader, &mut writer).await?;
                writer.close().await.map_err(|err| err.to_string())
            })
            .map_err(|err| {
                format!(
                    "failed to copy generated output {} to {}: {err}",
                    path.display(),
                    relative_path
                )
            })?;
    }

    Ok(())
}

fn staged_source_file_name<IF>(index: usize, input_file: &IF) -> String
where
    IF: InputFile,
{
    let file_name = input_file
        .path()
        .file_name()
        .and_then(|name| name.to_str())
        .filter(|name| !name.is_empty())
        .unwrap_or("source.argon");

    format!("{index:04}-{file_name}")
}

fn staged_file_name<IF>(name: &str, index: usize, input_file: &IF) -> String
where
    IF: InputFile,
{
    let extension = input_file
        .path()
        .extension()
        .and_then(|extension| extension.to_str())
        .filter(|extension| !extension.is_empty())
        .unwrap_or("bin");

    format!("{name}-{index}.{extension}")
}

fn copy_input_file_to_path<IF>(input_file: IF, output_path: &Path) -> Result<(), String>
where
    IF: InputFile,
{
    if let Some(parent) = output_path.parent() {
        std::fs::create_dir_all(parent).map_err(|err| {
            format!(
                "failed to create staged file directory {}: {err}",
                parent.display()
            )
        })?;
    }

    let mut writer = std::fs::File::create(output_path).map_err(|err| {
        format!(
            "failed to create staged file {}: {err}",
            output_path.display()
        )
    })?;

    let runtime = tokio::runtime::Runtime::new().map_err(|err| err.to_string())?;
    runtime
        .block_on(async {
            let mut reader = input_file.open().await.map_err(|err| err.to_string())?;
            copy_reader_to_std_writer(&mut reader, &mut writer).await?;
            reader.close().await.map_err(|err| err.to_string())
        })
        .map_err(|err| {
            format!(
                "failed to write staged file {}: {err}",
                output_path.display()
            )
        })
}

async fn copy_reader_to_std_writer<R, W>(reader: &mut R, writer: &mut W) -> Result<(), String>
where
    R: Read,
    W: std::io::Write,
{
    let mut buffer = [0u8; 8192];
    loop {
        let count = reader
            .read(&mut buffer)
            .await
            .map_err(|err| err.to_string())?;
        if count == 0 {
            return Ok(());
        }

        std::io::Write::write_all(writer, &buffer[..count]).map_err(|err| err.to_string())?;
    }
}

async fn copy_std_reader_to_writer<R, W>(reader: &mut R, writer: &mut W) -> Result<(), String>
where
    R: std::io::Read,
    W: Write,
{
    let mut buffer = [0u8; 8192];
    loop {
        let count = std::io::Read::read(reader, &mut buffer).map_err(|err| err.to_string())?;
        if count == 0 {
            break;
        }

        writer
            .write_all(&buffer[..count])
            .await
            .map_err(|err| err.to_string())?;
    }

    writer.flush().await.map_err(|err| err.to_string())
}
