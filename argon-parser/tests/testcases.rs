use std::cell::RefCell;
use std::path::PathBuf;
use std::rc::Rc;

use argon_testcases::load_test_case;
use argon_util::{CompileError, ErrorReporter, InternalCompilerError};
use rstest::rstest;

#[derive(Clone, Default)]
struct TestErrorReporter {
    internal_errors: Rc<RefCell<Vec<InternalCompilerError>>>,
    parse_errors: Rc<RefCell<Vec<CompileError>>>,
}

impl ErrorReporter<InternalCompilerError> for TestErrorReporter {
    fn report_error(&self, error: InternalCompilerError) {
        self.internal_errors.borrow_mut().push(error);
    }
}

impl ErrorReporter<CompileError> for TestErrorReporter {
    fn report_error(&self, error: CompileError) {
        self.parse_errors.borrow_mut().push(error);
    }
}

mod testcases {
    use super::*;

    #[rstest]
    fn parses_test_case_sources(
        #[base_dir = "../argon-testcases/testcases"]
        #[files("**/*.xml")]
        test_case_path: PathBuf,
    ) {
        let test_case = load_test_case(&test_case_path).unwrap();

        for input_source in test_case.input_sources {
            let reporter = TestErrorReporter::default();
            let file_name = PathBuf::from(&input_source.name);

            let module = argon_parser::parse(input_source.source.as_bytes(), &file_name, &reporter);

            assert!(
                reporter.internal_errors.borrow().is_empty(),
                "{}:{} reported internal compiler errors",
                test_case_path.display(),
                input_source.name
            );
            assert!(
                reporter.parse_errors.borrow().is_empty(),
                "{}:{} reported parse errors: {:?}",
                test_case_path.display(),
                input_source.name,
                reporter.parse_errors.borrow()
            );

            assert!(
                !module.stmts.is_empty(),
                "{}:{} is empty",
                test_case_path.display(),
                input_source.name
            );
        }
    }
}
