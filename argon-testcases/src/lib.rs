extern crate self as argon_testcases;
use std::error::Error;
use std::fmt;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};

use serde::Deserialize;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TestCase {
    pub name: String,
    pub input_sources: Vec<InputSource>,
    pub libraries: Vec<String>,
    pub expected: ExpectedResult,
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
pub struct InputSource {
    #[serde(rename = "@name")]
    pub name: String,
    #[serde(rename = "$text")]
    pub source: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExpectedResult {
    Output(Vec<String>),
    CompileErrors(Vec<String>),
    ExecutionErrors(Vec<String>),
}

impl TestCase {
    pub fn from_xml(xml: &str) -> Result<Self, DecodeError> {
        quick_xml::de::from_str::<RawTestCase>(xml)?.try_into()
    }
}

impl std::str::FromStr for TestCase {
    type Err = DecodeError;

    fn from_str(xml: &str) -> Result<Self, Self::Err> {
        Self::from_xml(xml)
    }
}

pub fn load_test_case(
    path: impl AsRef<Path>,
) -> Result<TestCase, LoadError> {
    match fs::read_to_string(&path) {
        Ok(xml) => {
            let path = path.as_ref().to_owned();
            TestCase::from_xml(&xml).map_err(|source| LoadError::Decode {
                path,
                source,
            })
        }
        Err(err) => {
            let path = path.as_ref().to_owned();
            Err(LoadError::Io { path, source: err })
        },
    }
}

#[derive(Debug)]
pub enum LoadError {
    Io { path: PathBuf, source: io::Error },
    Decode { path: PathBuf, source: DecodeError },
}

impl fmt::Display for LoadError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Io { path, source } => {
                write!(f, "failed to read test case {}: {source}", path.display())
            }
            Self::Decode { path, source } => {
                write!(f, "failed to decode test case {}: {source}", path.display())
            }
        }
    }
}

impl Error for LoadError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::Io { source, .. } => Some(source),
            Self::Decode { source, .. } => Some(source),
        }
    }
}

#[derive(Debug)]
pub enum DecodeError {
    Xml(quick_xml::DeError),
    MissingExpectation,
    MultipleExpectationTypes {
        output_count: usize,
        compile_error_count: usize,
        execution_error_count: usize,
    },
}

impl fmt::Display for DecodeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Xml(err) => write!(f, "failed to decode XML test case: {err}"),
            Self::MissingExpectation => write!(f, "test case does not define an expected result"),
            Self::MultipleExpectationTypes {
                output_count,
                compile_error_count,
                execution_error_count,
            } => write!(
                f,
                "test case defines multiple expected result types: {output_count} outputs, \
                 {compile_error_count} compile errors, {execution_error_count} execution errors"
            ),
        }
    }
}

impl Error for DecodeError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::Xml(err) => Some(err),
            Self::MissingExpectation | Self::MultipleExpectationTypes { .. } => None,
        }
    }
}

impl From<quick_xml::DeError> for DecodeError {
    fn from(value: quick_xml::DeError) -> Self {
        Self::Xml(value)
    }
}

#[derive(Debug, Deserialize)]
#[serde(rename = "ArgonTest")]
struct RawTestCase {
    #[serde(rename = "Name")]
    name: String,
    #[serde(rename = "InputSource", default)]
    input_sources: Vec<InputSource>,
    #[serde(rename = "Libraries", default)]
    libraries: Libraries,
    #[serde(rename = "ExpectedOutput", default)]
    expected_output: Vec<String>,
    #[serde(rename = "ExpectedError", default)]
    expected_error: Vec<String>,
    #[serde(rename = "ExpectedExecutionError", default)]
    expected_execution_error: Vec<String>,
}

#[derive(Debug, Default, Deserialize)]
struct Libraries {
    #[serde(rename = "Library", default)]
    libraries: Vec<String>,
}

impl TryFrom<RawTestCase> for TestCase {
    type Error = DecodeError;

    fn try_from(value: RawTestCase) -> Result<Self, Self::Error> {
        let expectation_type_count = [
            !value.expected_output.is_empty(),
            !value.expected_error.is_empty(),
            !value.expected_execution_error.is_empty(),
        ]
        .into_iter()
        .filter(|has_expectation| *has_expectation)
        .count();

        let expected = match expectation_type_count {
            0 => return Err(DecodeError::MissingExpectation),
            1 if !value.expected_output.is_empty() => ExpectedResult::Output(value.expected_output),
            1 if !value.expected_error.is_empty() => {
                ExpectedResult::CompileErrors(value.expected_error)
            }
            1 => ExpectedResult::ExecutionErrors(value.expected_execution_error),
            _ => {
                return Err(DecodeError::MultipleExpectationTypes {
                    output_count: value.expected_output.len(),
                    compile_error_count: value.expected_error.len(),
                    execution_error_count: value.expected_execution_error.len(),
                });
            }
        };

        Ok(Self {
            name: value.name,
            input_sources: value.input_sources,
            libraries: value.libraries.libraries,
            expected,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::{ExpectedResult, TestCase};
    use rstest::*;
    use std::fs;
    use std::path::PathBuf;

    #[test]
    fn decodes_single_file_test_case() {
        let xml = fs::read_to_string("testcases/hello_world/HelloWorld.xml").unwrap();
        let test_case = TestCase::from_xml(&xml).unwrap();

        assert_eq!(test_case.name, "Hello World");
        assert_eq!(test_case.input_sources.len(), 1);
        assert_eq!(test_case.input_sources[0].name, "index.argon");
        assert_eq!(
            test_case.expected,
            ExpectedResult::Output(vec!["Hello World".to_string()])
        );
    }

    #[test]
    fn decodes_libraries_and_multiple_execution_errors() {
        let xml = fs::read_to_string(
            "testcases/access/function/Access_public_function_different_tube.xml",
        )
        .unwrap();
        let test_case = TestCase::from_xml(&xml).unwrap();

        assert_eq!(
            test_case.libraries,
            vec!["Argon.Core", "Argon.TestReference"]
        );

        let xml = fs::read_to_string("testcases/exceptions/finally_method_error.xml").unwrap();
        let test_case = TestCase::from_xml(&xml).unwrap();

        assert_eq!(
            test_case.expected,
            ExpectedResult::ExecutionErrors(vec![
                "My custom error message".to_string(),
                "Hello".to_string()
            ])
        );
    }

    #[rstest]
    fn decode_test_case(
        #[base_dir = "testcases"]
        #[files("**/*.xml")]
        test_case_path: PathBuf
    ) {
        let test_case = TestCase::from_xml(&fs::read_to_string(test_case_path).unwrap()).unwrap();
        assert!(!test_case.name.is_empty());
        assert!(!test_case.input_sources.is_empty());
    }
}
