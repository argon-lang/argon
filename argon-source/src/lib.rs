use crate::module::{process_source_file, register_module_reexports};
use argon_compiler::{Context, Tube, TubeCollectionBuilder, TubeMetadata, TubeName};
use argon_util::InternalCompilerError;
use rayon::prelude::*;
use std::collections::HashMap;
use std::ffi::OsStr;
use std::path::PathBuf;
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

    let parse_results = options
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
        .filter_map(|entry| {
            process_source_file(
                context.clone(),
                entry.path(),
                &tb,
                tube_collection.tube_collection(),
            )
        })
        .collect::<Vec<_>>();

    register_module_reexports(parse_results);

    tb.tube()
}

#[cfg(test)]
mod tests {
    use super::{SourceCodeTubeOptions, define_source_tube};
    use argon_compiler::{
        CompileErrorReporter, Context, ContextObject, ModulePath, TubeCollectionBuilder, TubeName,
    };
    use argon_parser::ast::Identifier;
    use argon_util::{CompileError, ErrorCode, ErrorReporter, Fuel, InternalCompilerError};
    use nonempty_collections::NEVec;
    use std::fs;
    use std::path::Path;
    use std::sync::{Arc, Mutex};
    use std::time::{SystemTime, UNIX_EPOCH};

    #[derive(Default)]
    struct TestReporter {
        compile_errors: Mutex<Vec<CompileError>>,
        internal_errors: Mutex<Vec<InternalCompilerError>>,
    }

    impl TestReporter {
        fn compile_errors(&self) -> Vec<CompileError> {
            self.compile_errors.lock().unwrap().clone()
        }
    }

    impl ErrorReporter<CompileError> for TestReporter {
        fn report_error(&self, error: CompileError) {
            self.compile_errors.lock().unwrap().push(error);
        }
    }

    impl ErrorReporter<InternalCompilerError> for TestReporter {
        fn report_error(&self, error: InternalCompilerError) {
            self.internal_errors.lock().unwrap().push(error);
        }
    }

    struct TestContext {
        reporter: TestReporter,
    }

    impl ContextObject for TestContext {
        fn reporter(&self) -> &dyn CompileErrorReporter {
            &self.reporter
        }

        fn normalize_fuel(&self) -> Fuel {
            Fuel::new(5)
        }
    }

    fn test_context() -> (Arc<TestContext>, Context) {
        let concrete = Arc::new(TestContext {
            reporter: TestReporter::default(),
        });
        let context: Context = concrete.clone();
        (concrete, context)
    }

    fn tube_name() -> TubeName {
        TubeName(NEVec::new("Test".to_string()))
    }

    fn write_source(dir: &Path, name: &str, contents: &str) {
        fs::write(dir.join(name), contents).unwrap();
    }

    fn temp_source_dir(test_name: &str) -> std::path::PathBuf {
        let id = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        let dir = std::env::temp_dir().join(format!("argon-source-{test_name}-{id}"));
        fs::create_dir(&dir).unwrap();
        dir
    }

    #[test]
    fn transitive_wildcard_reexports_are_registered() {
        let dir = temp_source_dir("transitive-reexports");
        write_source(
            &dir,
            "A.argon",
            r#"
module A
export ::B::*
"#,
        );
        write_source(
            &dir,
            "B.argon",
            r#"
module B
export ::C::*
"#,
        );
        write_source(
            &dir,
            "C.argon",
            r#"
module C
public def c: type = __argon_builtin never_type
"#,
        );

        let (reporter_context, context) = test_context();
        let tube_collection = TubeCollectionBuilder::new(context.clone());
        let tube = define_source_tube(
            context,
            SourceCodeTubeOptions {
                name: tube_name(),
                referenced_tubes: Vec::new(),
                sources: vec![dir.clone()],
            },
            &tube_collection,
        );

        assert!(reporter_context.reporter.compile_errors().is_empty());
        assert!(
            tube.module(&ModulePath(vec!["A".to_string()]))
                .unwrap()
                .named_exports(&Identifier::Named("c".to_string()))
                .is_some()
        );

        fs::remove_dir_all(dir).unwrap();
    }

    #[test]
    fn circular_reexports_are_reported() {
        let dir = temp_source_dir("circular-reexports");
        write_source(
            &dir,
            "A.argon",
            r#"
module A
export ::B::*
"#,
        );
        write_source(
            &dir,
            "B.argon",
            r#"
module B
export ::A::*
"#,
        );

        let (reporter_context, context) = test_context();
        let tube_collection = TubeCollectionBuilder::new(context.clone());
        define_source_tube(
            context,
            SourceCodeTubeOptions {
                name: tube_name(),
                referenced_tubes: Vec::new(),
                sources: vec![dir.clone()],
            },
            &tube_collection,
        );

        assert!(
            reporter_context
                .reporter
                .compile_errors()
                .iter()
                .any(|error| error.code == ErrorCode::CircularReexport)
        );

        fs::remove_dir_all(dir).unwrap();
    }
}
