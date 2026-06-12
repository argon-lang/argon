#![no_std]

extern crate alloc;
#[cfg(any(test, feature = "std"))]
extern crate std;

use crate::module::{process_source_file, register_module_reexports};
use alloc::{sync::Arc, vec::Vec};
use argon_compiler::{Context, Tube, TubeCollectionBuilder, TubeMetadata, TubeName};
use argon_io::InputDirectory;
use argon_util::sync::{ThreadSafe, parallel::*};
use hashbrown::HashMap;

mod enums;
mod function;
mod modifiers;
mod module;
mod record;
mod signature;
mod traits;
mod type_checker;
mod method;

pub struct SourceCodeTubeOptions<I> {
    pub name: TubeName,
    pub referenced_tubes: Vec<TubeName>,
    pub input_dirs: Vec<I>,
}

pub fn define_source_tube<I>(
    context: Context,
    options: SourceCodeTubeOptions<I>,
    tube_collection: &TubeCollectionBuilder,
) -> Arc<Tube>
where
    I: InputDirectory + ThreadSafe,
    I::File: ThreadSafe,
{
    let tb = tube_collection.add_tube(
        options.name,
        TubeMetadata {
            platform: HashMap::new(),
        },
        options.referenced_tubes,
    );

    let parse_results = options
        .input_dirs
        .par_iter()
        .flat_map_iter(|source_dir| source_dir.list_files().into_iter())
        .filter_map(|source| {
            let source = match source {
                Ok(source) => source,
                Err(err) => {
                    context.reporter().report_error(err);
                    return None;
                }
            };

            process_source_file(
                context.clone(),
                &source,
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
    use alloc::{string::String, string::ToString, vec, vec::Vec};
    use argon_compiler::access::AccessModifier;
    use argon_compiler::platform::PlatformExtern;
    use argon_compiler::{
        CompileErrorReporter, Context, ContextObject, FunctionImplementation, MethodOwner, MethodSlot,
        ModuleExportBinding, ModulePath, TubeCollectionBuilder, TubeName,
    };
    use argon_expr::{Expr, Variable};
    use argon_io::{InputDirectory, InputFile};
    use argon_parser::ast::Identifier;
    use argon_util::sync::Mutex;
    use argon_util::{CompileError, ErrorCode, ErrorReporter, Fuel, InternalCompilerError};
    use embedded_io::{ErrorType, Read};
    use hashbrown::HashMap;
    use mitsein::vec1::Vec1;
    use parse18_runtime::WithLocation;
    use std::fs;
    use std::path::Path;
    use std::sync::Arc;
    use tempfile::TempDir;
    use walkdir::WalkDir;

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

        fn extern_function(&self, _name: &WithLocation<String>) -> PlatformExtern {
            PlatformExtern {
                externs: HashMap::new(),
            }
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
        TubeName(Vec1::from_one("Test".to_string()))
    }

    fn write_source(dir: &Path, name: &str, contents: &str) {
        fs::write(dir.join(name), contents).unwrap();
    }

    fn temp_source_dir(test_name: &str) -> TempDir {
        tempfile::Builder::new()
            .prefix(&alloc::format!("argon-source-{test_name}-"))
            .tempdir()
            .unwrap()
    }

    #[derive(Clone)]
    struct TestSourcePath {
        path: std::path::PathBuf,
        #[cfg(not(feature = "std"))]
        location_file: String,
    }

    #[derive(Clone)]
    struct TestSourceDir {
        path: std::path::PathBuf,
    }

    struct TestSourceFileReader {
        file: std::fs::File,
    }

    impl ErrorType for TestSourceFileReader {
        type Error = InternalCompilerError;
    }

    impl Read for TestSourceFileReader {
        fn read(&mut self, buf: &mut [u8]) -> Result<usize, Self::Error> {
            std::io::Read::read(&mut self.file, buf).map_err(|_| InternalCompilerError::WriteZero)
        }
    }

    impl InputFile for TestSourcePath {
        type Reader = TestSourceFileReader;

        #[cfg(feature = "std")]
        fn path(&self) -> &Path {
            &self.path
        }

        #[cfg(not(feature = "std"))]
        fn location_file(&self) -> &parse18_runtime::LocationFileView {
            &self.location_file
        }

        fn open(&self) -> Result<Self::Reader, InternalCompilerError> {
            let file =
                std::fs::File::open(&self.path).map_err(|_| InternalCompilerError::WriteZero)?;
            Ok(TestSourceFileReader { file })
        }
    }

    impl InputDirectory for TestSourceDir {
        type File = TestSourcePath;
        type Files = Vec<Result<TestSourcePath, InternalCompilerError>>;

        fn list_files(&self) -> Self::Files {
            WalkDir::new(&self.path)
                .into_iter()
                .map(Result::unwrap)
                .filter(|entry| entry.file_type().is_file())
                .map(|entry| {
                    let path = entry.path().to_path_buf();
                    Ok(TestSourcePath {
                        #[cfg(not(feature = "std"))]
                        location_file: path.display().to_string(),
                        path,
                    })
                })
                .collect()
        }
    }

    fn source_dir(dir: &Path) -> TestSourceDir {
        TestSourceDir {
            path: dir.to_path_buf(),
        }
    }

    #[test]
    fn transitive_wildcard_reexports_are_registered() {
        let dir = temp_source_dir("transitive-reexports");
        write_source(
            dir.path(),
            "A.argon",
            r#"
module A
export ::B::*
"#,
        );
        write_source(
            dir.path(),
            "B.argon",
            r#"
module B
export ::C::*
"#,
        );
        write_source(
            dir.path(),
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
                input_dirs: vec![source_dir(dir.path())],
            },
            &tube_collection,
        );

        assert!(reporter_context.reporter.compile_errors().is_empty());
        assert!(
            tube.module(&ModulePath(vec!["A".to_string()]))
                .unwrap()
                .export_groups()
                .contains_key(&Identifier::Named("c".to_string()))
        );
    }

    #[test]
    fn circular_reexports_are_reported() {
        let dir = temp_source_dir("circular-reexports");
        write_source(
            dir.path(),
            "A.argon",
            r#"
module A
export ::B::*
"#,
        );
        write_source(
            dir.path(),
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
                input_dirs: vec![source_dir(dir.path())],
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
    }
}
