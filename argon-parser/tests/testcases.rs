use std::cell::RefCell;
use std::convert::Infallible;
use std::path::PathBuf;
use std::rc::Rc;

use argon_parser::ast::{Expr, FunctionBody, Identifier, Stmt};
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

impl ErrorReporter<Infallible> for TestErrorReporter {
    fn report_error(&self, error: Infallible) {
        match error {}
    }
}

#[test]
fn parses_label_statement() {
    let reporter = TestErrorReporter::default();
    let file_name = PathBuf::from("index.argon");
    let source = r#"
        module _

        proc main(): () do
            @entry:
        end
    "#;

    let module = argon_parser::parse(source.as_bytes(), &file_name, &reporter);

    assert!(reporter.internal_errors.borrow().is_empty());
    assert!(
        reporter.parse_errors.borrow().is_empty(),
        "parse errors: {:?}",
        reporter.parse_errors.borrow()
    );

    let Stmt::FunctionDeclaration(function) = &module.stmts[0].value else {
        panic!("expected function declaration");
    };
    let FunctionBody::ExprBody(body) = &function.body else {
        panic!("expected expression function body");
    };
    let Expr::Block { body, .. } = &body.value else {
        panic!("expected block function body");
    };
    let Stmt::Label(label) = &body.value[0].value else {
        panic!("expected label statement");
    };
    assert!(matches!(label.name.value, Identifier::Named(ref name) if name == "entry"));
}

#[test]
fn parses_labeled_break_with_value() {
    let reporter = TestErrorReporter::default();
    let file_name = PathBuf::from("index.argon");
    let source = r#"
        module _

        proc main(): () do
            loop @done do
                break @done 7
            end
        end
    "#;

    let module = argon_parser::parse(source.as_bytes(), &file_name, &reporter);

    assert!(reporter.internal_errors.borrow().is_empty());
    assert!(
        reporter.parse_errors.borrow().is_empty(),
        "parse errors: {:?}",
        reporter.parse_errors.borrow()
    );

    let Stmt::FunctionDeclaration(function) = &module.stmts[0].value else {
        panic!("expected function declaration");
    };
    let FunctionBody::ExprBody(body) = &function.body else {
        panic!("expected expression function body");
    };
    let Expr::Block { body, .. } = &body.value else {
        panic!("expected block function body");
    };
    let Stmt::Expr(loop_expr) = &body.value[0].value else {
        panic!("expected loop expression statement");
    };
    let Expr::Loop { label, body } = &loop_expr.value else {
        panic!("expected loop expression");
    };
    assert!(
        matches!(label.as_ref().map(|label| &label.value), Some(Identifier::Named(name)) if name == "done")
    );

    let Stmt::Expr(break_expr) = &body.value[0].value else {
        panic!("expected break expression statement");
    };
    let Expr::Break { label, value } = &break_expr.value else {
        panic!("expected break expression");
    };
    assert!(
        matches!(label.as_ref().map(|label| &label.value), Some(Identifier::Named(name)) if name == "done")
    );
    assert!(matches!(
        value.as_ref().map(|value| &value.value),
        Some(Expr::IntLiteral { value, .. }) if *value == 7.into()
    ));
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
