extern crate self as argon_testcases;
use std::error::Error;
use std::fmt;
use std::fs;
use std::io;
use std::path::{Component, Path, PathBuf};

use base64::Engine;
use serde::Deserialize;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TestCase {
    pub name: String,
    pub input_sources: Vec<InputSource>,
    pub resource_files: Vec<ResourceFile>,
    pub libraries: Vec<String>,
    pub expected: ExpectedResult,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResourceFile {
    pub name: String,
    pub contents: Vec<u8>,
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
    Output(String),
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

pub fn load_test_case(path: impl AsRef<Path>) -> Result<TestCase, LoadError> {
    match fs::read_to_string(&path) {
        Ok(xml) => {
            let path = path.as_ref().to_owned();
            TestCase::from_xml(&xml).map_err(|source| LoadError::Decode { path, source })
        }
        Err(err) => {
            let path = path.as_ref().to_owned();
            Err(LoadError::Io { path, source: err })
        }
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
    InvalidResourceEncoding {
        name: String,
        encoding: String,
    },
    InvalidResourceContents {
        name: String,
        source: base64::DecodeError,
    },
    InvalidResourcePath {
        name: String,
    },
    DuplicateResourcePath {
        name: String,
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
            Self::InvalidResourceEncoding { name, encoding } => write!(
                f,
                "resource file {name} has unsupported encoding {encoding}"
            ),
            Self::InvalidResourceContents { name, source } => {
                write!(f, "resource file {name} contains invalid base64: {source}")
            }
            Self::InvalidResourcePath { name } => {
                write!(
                    f,
                    "resource file path is not relative and contained: {name}"
                )
            }
            Self::DuplicateResourcePath { name } => {
                write!(f, "resource file path is duplicated: {name}")
            }
        }
    }
}

impl Error for DecodeError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::Xml(err) => Some(err),
            Self::InvalidResourceContents { source, .. } => Some(source),
            Self::MissingExpectation
            | Self::MultipleExpectationTypes { .. }
            | Self::InvalidResourceEncoding { .. }
            | Self::InvalidResourcePath { .. }
            | Self::DuplicateResourcePath { .. } => None,
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
    #[serde(rename = "ResourceFile", default)]
    resource_files: Vec<RawResourceFile>,
    #[serde(rename = "Libraries", default)]
    libraries: Libraries,
    #[serde(rename = "ExpectedOutput", default)]
    expected_output: Option<String>,
    #[serde(rename = "ExpectedError", default)]
    expected_error: Vec<String>,
    #[serde(rename = "ExpectedExecutionError", default)]
    expected_execution_error: Vec<String>,
}

#[derive(Debug, Deserialize)]
struct RawResourceFile {
    #[serde(rename = "@name")]
    name: String,
    #[serde(rename = "@encoding", default)]
    encoding: Option<String>,
    #[serde(rename = "$text", default)]
    contents: String,
}

#[derive(Debug, Default, Deserialize)]
struct Libraries {
    #[serde(rename = "Library", default)]
    libraries: Vec<String>,
}

impl TryFrom<RawTestCase> for TestCase {
    type Error = DecodeError;

    fn try_from(value: RawTestCase) -> Result<Self, Self::Error> {
        let mut resource_paths = std::collections::HashSet::new();
        let mut resource_files = Vec::with_capacity(value.resource_files.len());
        for resource in value.resource_files {
            let path = Path::new(&resource.name);
            if resource.name.is_empty()
                || path.is_absolute()
                || path.components().any(|component| {
                    matches!(
                        component,
                        Component::ParentDir | Component::RootDir | Component::Prefix(_)
                    )
                })
            {
                return Err(DecodeError::InvalidResourcePath {
                    name: resource.name,
                });
            }
            if !resource_paths.insert(resource.name.clone()) {
                return Err(DecodeError::DuplicateResourcePath {
                    name: resource.name,
                });
            }

            let contents = match resource.encoding.as_deref().unwrap_or("text") {
                "text" => resource.contents.into_bytes(),
                "base64" => {
                    let encoded = resource
                        .contents
                        .bytes()
                        .filter(|byte| !byte.is_ascii_whitespace())
                        .collect::<Vec<_>>();
                    base64::engine::general_purpose::STANDARD
                        .decode(encoded)
                        .map_err(|source| DecodeError::InvalidResourceContents {
                            name: resource.name.clone(),
                            source,
                        })?
                }
                encoding => {
                    return Err(DecodeError::InvalidResourceEncoding {
                        name: resource.name,
                        encoding: encoding.to_owned(),
                    });
                }
            };
            resource_files.push(ResourceFile {
                name: resource.name,
                contents,
            });
        }

        let expected = if let Some(expected_output) = value.expected_output {
            ExpectedResult::Output(expected_output)
        } else if !value.expected_error.is_empty() {
            ExpectedResult::CompileErrors(value.expected_error)
        } else if !value.expected_execution_error.is_empty() {
            ExpectedResult::ExecutionErrors(value.expected_execution_error)
        } else {
            return Err(DecodeError::MissingExpectation);
        };

        Ok(Self {
            name: value.name,
            input_sources: value.input_sources,
            resource_files,
            libraries: value.libraries.libraries,
            expected,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::{DecodeError, ExpectedResult, ResourceFile, TestCase};
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
        assert!(test_case.resource_files.is_empty());
        assert_eq!(
            test_case.expected,
            ExpectedResult::Output("Hello World".to_string())
        );
    }

    #[test]
    fn decodes_text_and_base64_resource_files() {
        let test_case = TestCase::from_xml(
            r#"<ArgonTest>
                <Name>Resources</Name>
                <InputSource name="index.argon">module _</InputSource>
                <ResourceFile name="hello.txt">Hello</ResourceFile>
                <ResourceFile name="data.bin" encoding="base64">
                    AAH+/w==
                </ResourceFile>
                <ExpectedOutput />
            </ArgonTest>"#,
        )
        .expect("decode test case with resources");

        assert_eq!(
            test_case.resource_files,
            vec![
                ResourceFile {
                    name: "hello.txt".to_owned(),
                    contents: b"Hello".to_vec(),
                },
                ResourceFile {
                    name: "data.bin".to_owned(),
                    contents: vec![0, 1, 254, 255],
                },
            ]
        );
    }

    #[rstest]
    #[case("../outside", "text", "data")]
    #[case("/absolute", "text", "data")]
    #[case("same", "hex", "00")]
    #[case("same", "base64", "not base64!")]
    fn rejects_invalid_resource_files(
        #[case] name: &str,
        #[case] encoding: &str,
        #[case] contents: &str,
    ) {
        let xml = format!(
            r#"<ArgonTest>
                <Name>Invalid resource</Name>
                <InputSource name="index.argon">module _</InputSource>
                <ResourceFile name="{name}" encoding="{encoding}">{contents}</ResourceFile>
                <ExpectedOutput />
            </ArgonTest>"#
        );

        assert!(TestCase::from_xml(&xml).is_err());
    }

    #[test]
    fn rejects_duplicate_resource_files() {
        let err = TestCase::from_xml(
            r#"<ArgonTest>
                <Name>Duplicate resource</Name>
                <InputSource name="index.argon">module _</InputSource>
                <ResourceFile name="same">one</ResourceFile>
                <ResourceFile name="same">two</ResourceFile>
                <ExpectedOutput />
            </ArgonTest>"#,
        )
        .expect_err("duplicate resources should be rejected");

        assert!(matches!(err, DecodeError::DuplicateResourcePath { .. }));
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
        test_case_path: PathBuf,
    ) {
        let test_case = TestCase::from_xml(&fs::read_to_string(test_case_path).unwrap()).unwrap();
        assert!(!test_case.name.is_empty());
        assert!(!test_case.input_sources.is_empty());
    }
}
