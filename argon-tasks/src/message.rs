noble_idl_runtime::include_noble_idl!();

use alloc::{boxed::Box, string::String};
use argon_util::CompileError as CompilerCompileError;
use embedded_io::Write;
use esexpr::ESExprCodec;
use esexpr_binary::ExprGeneratorSync;

pub trait TaskLogger {
    fn log(&mut self, message: TaskMessage);
}

pub fn compile_error(error: CompilerCompileError) -> TaskMessage {
    let location = error.location.map(|location| {
        #[cfg(feature = "std")]
        let file = location.file.to_string_lossy().into_owned();
        #[cfg(not(feature = "std"))]
        let file = location.file;

        Box::new(SourceLocation {
            file,
            start: Box::new(SourcePosition {
                line: location.start.line.into(),
                column: location.start.column.into(),
            }),
            end: Box::new(SourcePosition {
                line: location.end.line.into(),
                column: location.end.column.into(),
            }),
        })
    });

    TaskMessage::CompileError {
        error: Box::new(CompileError {
            code: error.code.code().into(),
            message: error.message,
            location,
        }),
    }
}

pub fn task_error(message: impl Into<String>) -> TaskMessage {
    TaskMessage::TaskError {
        message: message.into(),
    }
}

pub struct TextTaskLogger<'a, W> {
    output: &'a mut W,
}

impl<'a, W> TextTaskLogger<'a, W> {
    pub fn new(output: &'a mut W) -> Self {
        Self { output }
    }
}

impl<W: Write> TaskLogger for TextTaskLogger<'_, W> {
    fn log(&mut self, message: TaskMessage) {
        match message {
            TaskMessage::CompileError { error } => {
                let _ = self
                    .output
                    .write_fmt(format_args!("compile error: [AR{:04X}] ", error.code));
                if let Some(location) = error.location {
                    let _ = self.output.write_fmt(format_args!(
                        "{}:{}:{}-{}:{} ",
                        location.file,
                        location.start.line,
                        location.start.column,
                        location.end.line,
                        location.end.column,
                    ));
                }
                let _ = self.output.write_all(error.message.as_bytes());
                let _ = self.output.write_all(b"\n");
            }
            TaskMessage::TaskError { message } => {
                let _ = self.output.write_all(message.as_bytes());
                if !message.ends_with('\n') {
                    let _ = self.output.write_all(b"\n");
                }
            }
        }
        let _ = self.output.flush();
    }
}

pub struct ESExprTaskLogger<'a, W> {
    generator: esexpr_binary::ExprGenerator<'a, W>,
}

impl<'a, W> ESExprTaskLogger<'a, W> {
    pub fn new(output: &'a mut W) -> Self {
        Self {
            generator: esexpr_binary::ExprGenerator::new(output),
        }
    }
}

impl<W: Write> TaskLogger for ESExprTaskLogger<'_, W> {
    fn log(&mut self, message: TaskMessage) {
        let expression = message.encode_esexpr();
        let _ = self.generator.generate(&expression);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::vec::Vec;
    use esexpr::{ESExpr, ESExprCodec};

    #[test]
    fn compile_error_is_encoded_inline() {
        let message = TaskMessage::CompileError {
            error: Box::new(CompileError {
                code: 0x11_u32.into(),
                message: "Unknown identifier".into(),
                location: None,
            }),
        };

        let ESExpr::Constructor(expression) = message.encode_esexpr() else {
            panic!("compile error must be encoded as a constructor");
        };
        assert_eq!(&*expression.name, "compile-error");
    }

    #[test]
    fn text_logger_formats_compile_error() {
        let mut output = Vec::new();
        let mut logger = TextTaskLogger::new(&mut output);
        logger.log(TaskMessage::CompileError {
            error: Box::new(CompileError {
                code: 0x11_u32.into(),
                message: "Unknown identifier: x".into(),
                location: Some(Box::new(SourceLocation {
                    file: "src/main.argon".into(),
                    start: Box::new(SourcePosition {
                        line: 2_u32.into(),
                        column: 3_u32.into(),
                    }),
                    end: Box::new(SourcePosition {
                        line: 2_u32.into(),
                        column: 4_u32.into(),
                    }),
                })),
            }),
        });

        assert_eq!(
            String::from_utf8(output).unwrap(),
            "compile error: [AR0011] src/main.argon:2:3-2:4 Unknown identifier: x\n"
        );
    }

    #[test]
    fn text_logger_terminates_task_errors() {
        let mut output = Vec::new();
        let mut logger = TextTaskLogger::new(&mut output);
        logger.log(task_error("backend failed"));
        assert_eq!(String::from_utf8(output).unwrap(), "backend failed\n");
    }
}
