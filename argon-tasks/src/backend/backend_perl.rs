use crate::backend::metadata::{Extern, PlatformMetadataResult};
use crate::message::{TaskLogger, task_error};
use alloc::{
    boxed::Box,
    collections::BTreeMap,
    format,
    string::{String, ToString},
    vec::Vec,
};
use argon_backend_perl::{
    ExternKind, PerlCodegenOptions as CoreCodegenOptions,
    PerlPlatformMetadataOptions as CoreMetadataOptions,
};
use argon_format_vm::vm as vf;
use argon_io::{InputFile, InputStream, OutputDirectory, OutputFile, OutputStream, Read, Write};
use esexpr::{ESExpr, ESExprCodec, ESExprStatic};
use esexpr_binary::{ExprGeneratorAsync, ExprParserAsync};

pub struct PerlPlatformMetadataOptions<I, O> {
    pub distribution_name: Option<String>,
    pub root_package: Option<Vec<String>>,
    pub extern_files: Vec<I>,
    pub output_file: O,
}
pub struct PerlCodegenOptions<I, O> {
    pub input_file: I,
    pub output_dir: O,
    pub executable: Option<String>,
}

pub trait BackendPerl {
    async fn platform_metadata<I: InputFile, O: OutputFile>(
        &self,
        options: PerlPlatformMetadataOptions<I, O>,
        logger: &mut dyn TaskLogger,
    ) -> bool;
    async fn codegen_perl<I: InputFile, O: OutputDirectory>(
        &self,
        options: PerlCodegenOptions<I, O>,
        logger: &mut dyn TaskLogger,
    ) -> bool;
}

#[derive(Clone, Copy, Debug, Default)]
pub struct PerlBackend;
impl PerlBackend {
    pub const fn new() -> Self {
        Self
    }
}

impl BackendPerl for PerlBackend {
    async fn platform_metadata<I: InputFile, O: OutputFile>(
        &self,
        options: PerlPlatformMetadataOptions<I, O>,
        logger: &mut dyn TaskLogger,
    ) -> bool {
        let mut parsed = Vec::new();
        for file in options.extern_files {
            let bytes = match read_file(&file).await {
                Ok(v) => v,
                Err(e) => return report(logger, e),
            };
            let source = match String::from_utf8(bytes) {
                Ok(v) => v,
                Err(e) => return report(logger, e),
            };
            match argon_backend_perl::parse_externs(&source) {
                Ok(v) => parsed.push(v),
                Err(e) => return report(logger, e),
            }
        }
        let mut seen = hashbrown::HashMap::new();
        let mut externs = hashbrown::HashMap::new();
        for group in parsed {
            for implementation in group.implementations {
                if seen
                    .insert(implementation.name.clone(), implementation.kind)
                    .is_some()
                {
                    return report(
                        logger,
                        format!("duplicate extern name {}", implementation.name),
                    );
                }
                let expr = implementation_expr(&implementation);
                let item = match implementation.kind {
                    ExternKind::Function => Extern::ExternFunction {
                        name: implementation.name.clone(),
                        implementation: expr,
                    },
                    ExternKind::Method => Extern::ExternMethod {
                        name: implementation.name.clone(),
                        implementation: expr,
                    },
                    ExternKind::StaticMethod => Extern::ExternStaticMethod {
                        name: implementation.name.clone(),
                        implementation: expr,
                    },
                };
                externs.insert(implementation.name, Box::new(item));
            }
        }
        let mut kwargs = BTreeMap::new();
        if let Some(v) = options.distribution_name {
            kwargs.insert("distribution-name".into(), ESExpr::Str(v.into()));
        }
        if let Some(v) = options.root_package {
            kwargs.insert(
                "root-package".into(),
                ESExpr::constructor(
                    "list",
                    v.into_iter()
                        .map(|s| ESExpr::Str(s.into()))
                        .collect::<Vec<_>>(),
                    BTreeMap::new(),
                ),
            );
        }
        let result = PlatformMetadataResult {
            platform: "perl".into(),
            tube_metadata: ESExprStatic::new(ESExpr::constructor(
                "perl-platform-metadata",
                Vec::new(),
                kwargs,
            )),
            externs: externs.into_iter().collect(),
        };
        let mut writer = match options.output_file.open().await {
            Ok(v) => v,
            Err(e) => return report(logger, e),
        };
        let mut generator = esexpr_binary::ExprGenerator::new(&mut writer);
        if let Err(e) = generator.generate(&result.encode_esexpr()).await {
            return report(logger, format!("could not encode metadata: {e:?}"));
        }
        drop(generator);
        if let Err(e) = writer.flush().await {
            return report(logger, e);
        }
        if let Err(e) = writer.close().await {
            return report(logger, e);
        }
        true
    }
    async fn codegen_perl<I: InputFile, O: OutputDirectory>(
        &self,
        options: PerlCodegenOptions<I, O>,
        logger: &mut dyn TaskLogger,
    ) -> bool {
        let mut reader = match options.input_file.open().await {
            Ok(v) => v,
            Err(e) => return report(logger, e),
        };
        let mut stream = esexpr_binary::parse_async(&mut reader);
        let mut entries = Vec::new();
        loop {
            match stream.try_read_next_expr().await {
                Ok(Some(e)) => match vf::TubeFileEntry::decode_esexpr(e) {
                    Ok(v) => entries.push(v),
                    Err(e) => return report(logger, format!("invalid VM IR entry: {e:?}")),
                },
                Ok(None) => break,
                Err(e) => return report(logger, format!("invalid VM IR: {e:?}")),
            }
        }
        drop(stream);
        let _ = reader.close().await;
        if entries.len() < 2 {
            return report(logger, "VM IR is missing its header or metadata");
        }
        let model = argon_vm::model::TubeModel::from_entries(entries);
        let metadata = decode_metadata(&model);
        if let Err(error) = argon_backend_perl::generate_distribution(
            &model,
            &metadata,
            &CoreCodegenOptions {
                executable: options.executable,
            },
            &options.output_dir,
        )
        .await
        {
            return report(logger, error);
        }
        true
    }
}

fn implementation_expr(v: &argon_backend_perl::ExternImplementation) -> ESExprStatic {
    let mut k = BTreeMap::new();
    k.insert(
        "kind".into(),
        ESExpr::Str(
            match v.kind {
                ExternKind::Function => "function",
                ExternKind::Method => "method",
                ExternKind::StaticMethod => "static-method",
            }
            .into(),
        ),
    );
    k.insert("imports".into(), list(v.imports.clone()));
    k.insert("declarations".into(), list(v.declarations.clone()));
    k.insert(
        "expression".into(),
        ESExpr::Str(v.expression.clone().into()),
    );
    ESExprStatic::new(ESExpr::constructor("perl-extern", Vec::new(), k))
}
fn list(v: Vec<String>) -> ESExpr<'static> {
    ESExpr::constructor(
        "list",
        v.into_iter()
            .map(|s| ESExpr::Str(s.into()))
            .collect::<Vec<_>>(),
        BTreeMap::new(),
    )
}
fn decode_metadata(model: &argon_vm::model::TubeModel) -> CoreMetadataOptions {
    let mut value = CoreMetadataOptions::default();
    if let Some(expr) = model.metadata.platform_metadata.as_ref() {
        let inner = expr.encode_esexpr();
        if let ESExpr::Constructor(c) = inner {
            if c.name.to_string() == "perl-platform-metadata" {
                for (key, item) in &c.kwargs {
                    if key.to_string() == "distribution-name" {
                        if let ESExpr::Str(s) = item {
                            value.distribution_name = Some(s.to_string());
                        }
                    } else if key.to_string() == "root-package" {
                        if let ESExpr::Constructor(l) = item {
                            value.root_package = Some(
                                l.args
                                    .iter()
                                    .filter_map(|x| {
                                        if let ESExpr::Str(s) = x {
                                            Some(s.to_string())
                                        } else {
                                            None
                                        }
                                    })
                                    .collect(),
                            );
                        }
                    }
                }
            }
        }
    }
    value
}
async fn read_file<I: InputFile>(file: &I) -> Result<Vec<u8>, String> {
    let mut r = file.open().await.map_err(|e| e.to_string())?;
    let mut out = Vec::new();
    let mut b = [0; 8192];
    loop {
        let n = r.read(&mut b).await.map_err(|e| e.to_string())?;
        if n == 0 {
            break;
        }
        out.extend_from_slice(&b[..n]);
    }
    r.close().await.map_err(|e| e.to_string())?;
    Ok(out)
}
fn report(logger: &mut dyn TaskLogger, e: impl core::fmt::Display) -> bool {
    logger.log(task_error(alloc::format!("Perl backend error: {e}")));
    false
}
