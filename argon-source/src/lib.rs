use crate::module::process_source_file;
use argon_compiler::{Context, ModulePath, TubeBuilder, TubeCollectionBuilder, TubeName};
use argon_util::ErrorReporter;
use rayon::prelude::*;
use std::ffi::OsStr;
use std::path::{Path, PathBuf};
use walkdir::WalkDir;

mod function;
mod modifiers;
mod module;
mod type_checker;

pub struct SourceCodeTubeOptions {
    name: TubeName,
    referenced_tubes: Vec<TubeName>,
    sources: Vec<PathBuf>,
}

pub fn define_source_tube<C: Context>(
    context: &C,
    options: SourceCodeTubeOptions,
    tube_collection: &TubeCollectionBuilder<C>,
) {
    let tb = tube_collection.add_tube(options.name, options.referenced_tubes);

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
            process_source_file(context, entry.path(), &tb);
        })
        .collect::<Vec<_>>();
}
