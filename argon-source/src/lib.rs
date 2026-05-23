use crate::module::process_source_file;
use argon_compiler::{
    Context, ModulePath, TubeBuilder, TubeCollectionBuilder, TubeMetadata, TubeName,
};
use argon_util::ErrorReporter;
use rayon::prelude::*;
use std::collections::HashMap;
use std::ffi::OsStr;
use std::path::{Path, PathBuf};
use walkdir::WalkDir;

mod function;
mod modifiers;
mod module;
mod signature;
mod type_checker;

pub struct SourceCodeTubeOptions {
    name: TubeName,
    referenced_tubes: Vec<TubeName>,
    sources: Vec<PathBuf>,
}

pub fn define_source_tube(
    context: Context,
    options: SourceCodeTubeOptions,
    tube_collection: &TubeCollectionBuilder<'_>,
) {
    let tb = tube_collection.add_tube(
        options.name,
        TubeMetadata {
            platform: HashMap::new(),
        },
        options.referenced_tubes,
    );

    let module_results = options
        .sources
        .par_iter()
        .flat_map(|source_dir| WalkDir::new(source_dir).into_iter().par_bridge())
        .filter_map(|entry| match entry {
            Ok(entry) => Some(entry),
            Err(e) => {
                context.reporter().report_error(e);
                None
            }
        })
        .filter(|entry| {
            entry.file_type().is_file() && entry.path().extension() == Some(OsStr::new("argon"))
        })
        .map(|entry| {
            process_source_file(context.clone(), entry.path(), &tb);
        })
        .collect::<Vec<_>>();
}
