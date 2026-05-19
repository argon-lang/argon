use std::cell::RefCell;
use std::io;
use std::path::PathBuf;
use std::rc::Rc;

use argon_testcases::load_test_case;
use argon_util::{CompileError, ErrorReporter};
use rstest::rstest;

#[derive(Clone, Default)]
struct TestErrorReporter {
    io_errors: Rc<RefCell<Vec<String>>>,
    parse_errors: Rc<RefCell<Vec<CompileError>>>,
}

impl ErrorReporter<io::Error> for TestErrorReporter {
    fn report_error(&self, error: io::Error) {
        self.io_errors.borrow_mut().push(error.to_string());
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

            let module =
                argon_parser::parse(input_source.source.as_bytes(), &file_name, reporter.clone());

            assert!(
                reporter.io_errors.borrow().is_empty(),
                "{}:{} reported IO errors: {:?}",
                test_case_path.display(),
                input_source.name,
                reporter.io_errors.borrow()
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
