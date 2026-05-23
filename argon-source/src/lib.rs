use crate::module::process_source_file;
use argon_compiler::{
    Context, ModulePath, Tube, TubeBuilder, TubeCollectionBuilder, TubeMetadata, TubeName,
};
use argon_util::InternalCompilerError;
use rayon::prelude::*;
use std::collections::HashMap;
use std::ffi::OsStr;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use walkdir::WalkDir;

mod function;
mod modifiers;
mod module;
mod signature;
mod type_checker;

pub struct SourceCodeTubeOptions {
    pub name: TubeName,
    pub referenced_tubes: Vec<TubeName>,
    pub sources: Vec<PathBuf>,
}

pub fn define_source_tube(
    context: Context,
    options: SourceCodeTubeOptions,
    tube_collection: &TubeCollectionBuilder,
) -> Arc<Tube> {
    let tb = tube_collection.add_tube(
        options.name,
        TubeMetadata {
            platform: HashMap::new(),
        },
        options.referenced_tubes,
    );

    options
        .sources
        .par_iter()
        .flat_map(|source_dir| WalkDir::new(source_dir).into_iter().par_bridge())
        .filter_map(|entry| match entry {
            Ok(entry) => Some(entry),
            Err(e) => {
                context
                    .reporter()
                    .report_error(InternalCompilerError::WalkDirError(e));
                None
            }
        })
        .filter(|entry| {
            entry.file_type().is_file() && entry.path().extension() == Some(OsStr::new("argon"))
        })
        .for_each(|entry| {
            process_source_file(
                context.clone(),
                entry.path(),
                &tb,
                tube_collection.tube_collection(),
            );
        });

    tb.tube()
}
