//! Pure Rust Perl platform metadata extraction and distribution generation.

mod emitter;
mod externs;
mod names;
mod program;

use argon_io::{OutputFile, OutputStream, Write};

pub use externs::{
    ExternImplementation, ExternKind, ExternParseError, ExternSource, parse_externs,
};
pub use names::{decode_component, encode_component, module_package};

/// Options stored in Perl platform metadata.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct PerlPlatformMetadataOptions {
    pub distribution_name: Option<String>,
    pub root_package: Option<Vec<String>>,
}

/// Options controlling distribution emission.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct PerlCodegenOptions {
    pub executable: Option<String>,
}

/// Generate the distribution envelope for a decoded VM tube.
pub async fn generate_distribution<O: argon_io::OutputDirectory>(
    model: &argon_vm::model::TubeModel,
    metadata: &PerlPlatformMetadataOptions,
    options: &PerlCodegenOptions,
    output: &O,
) -> Result<(), String> {
    let tube = core::iter::once(model.metadata.name.head.as_str())
        .chain(model.metadata.name.tail.iter().map(String::as_str))
        .collect::<Vec<_>>();
    let root = metadata.root_package.clone().unwrap_or_else(|| {
        let mut value = vec!["Argon".into(), "Tube".into(), format!("T{}", tube.len())];
        value.extend(tube.iter().map(|s| encode_component(s)));
        value
    });
    validate_root(&root)?;
    let program = program::Program::new(model, root.clone())?;
    let distribution = metadata.distribution_name.clone().unwrap_or_else(|| {
        tube.iter()
            .map(|s| encode_component(s))
            .collect::<Vec<_>>()
            .join("-")
    });
    for module in &model.modules {
        let path = module
            .path
            .path
            .iter()
            .map(|s| s.as_str())
            .collect::<Vec<_>>();
        let package = module_package(&root, &path);
        let file_path = format!("lib/{}.pm", package.replace("::", "/"));
        let file = output.create_file(&file_path).map_err(display_error)?;
        let mut writer = file.open().await.map_err(display_error)?;
        let emitted = emitter::emit_module(&program, module, &package, &mut writer).await;
        let closed = writer.close().await.map_err(display_error);
        if let Err(error) = emitted.and(closed) {
            let _ = file.delete().await;
            return Err(error);
        }
    }
    let meta = format!(
        "{{\n  \"abstract\": \"Argon generated Perl distribution\",\n  \"dynamic_config\": false,\n  \"generated_by\": \"argon-backend-perl\",\n  \"license\": [\"unknown\"],\n  \"meta-spec\": {{\"url\": \"https://metacpan.org/pod/CPAN::Meta::Spec\", \"version\": 2}},\n  \"name\": \"{}\",\n  \"prereqs\": {{\"runtime\": {{\"requires\": {{\"Argon::Runtime\": \"0.1.0\", \"perl\": \"5.020\"}}}}}},\n  \"version\": \"0.1.0\"\n}}\n",
        json_escape(&distribution)
    );
    write_file(output, "META.json", meta.as_bytes()).await?;
    let makefile = format!(
        "use 5.020;\nuse ExtUtils::MakeMaker;\nWriteMakefile(NAME => '{}', VERSION => '0.1.0', PREREQ_PM => {{ 'Argon::Runtime' => '0.1.0' }});\n",
        perl_quote(&distribution)
    );
    write_file(output, "Makefile.PL", makefile.as_bytes()).await?;
    if let Some(name) = &options.executable {
        let main = module_package(&root, &[]);
        let main_symbol=model.function_info.values().find_map(|f| match &f.import_specifier{
            argon_format_vm::vm::ImportSpecifier::Global{name,..} if matches!(name.as_ref(),argon_format_vm::vm::Identifier::Named{s} if s=="main")=>program.callable_name(&f.import_specifier).ok(), _=>None
        }).ok_or("executable requested but root main was not found")?;
        let script = format!(
            "#!/usr/bin/env perl\nuse 5.020;\nuse strict;\nuse warnings;\nuse {main};\nmy $result = {main_symbol}([]);\ndie 'Argon main did not return the empty tuple' unless ref($result) eq 'ARRAY' && !@$result;\nexit 0;\n"
        );
        write_file(output, &format!("script/{name}"), script.as_bytes()).await?;
    }
    Ok(())
}

async fn write_file<O: argon_io::OutputDirectory>(
    output: &O,
    path: &str,
    contents: &[u8],
) -> Result<(), String> {
    let file = output.create_file(path).map_err(display_error)?;
    let mut writer = file.open().await.map_err(display_error)?;
    let written = writer.write_all(contents).await.map_err(display_error);
    let closed = writer.close().await.map_err(display_error);
    if let Err(error) = written.and(closed) {
        let _ = file.delete().await;
        return Err(error);
    }
    Ok(())
}

fn display_error(error: impl core::fmt::Display) -> String {
    error.to_string()
}

fn validate_root(root: &[String]) -> Result<(), String> {
    if root.is_empty()
        || root.iter().any(|s| {
            s.is_empty()
                || !s.bytes().enumerate().all(|(i, b)| {
                    b.is_ascii_alphabetic() || b == b'_' || (i > 0 && b.is_ascii_digit())
                })
        })
    {
        return Err("root-package must contain valid Perl package components".into());
    }
    Ok(())
}

fn json_escape(s: &str) -> String {
    s.replace('\\', "\\\\").replace('"', "\\\"")
}
fn perl_quote(s: &str) -> String {
    s.replace('\\', "\\\\").replace('\'', "\\'")
}
