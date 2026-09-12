use crate::context::RunnerContext;
use alloc::boxed::Box;
use argon_compiler::platform::{Extern as PlatformExtern, ExternType, PlatformMetadata};
use argon_io::{InputFile, InputStream};
use argon_util::{ErrorReporter, InternalCompilerError, TubeFormatError};
use esexpr::ESExprCodec;
use esexpr_binary::{ExprParserAsync, ParseError};
use hashbrown::HashMap;

noble_idl_runtime::include_noble_idl!();

pub async fn load_platform_metadata(context: &mut RunnerContext, input_file: impl InputFile) {
    let mut file = match input_file.open().await {
        Ok(file) => file,
        Err(err) => {
            context.runner_reporter().report_error(err);
            return;
        }
    };

    let metadata = {
        let mut expr_stream = esexpr_binary::parse_async(&mut file);
        match expr_stream.try_read_next_expr().await {
            Ok(Some(expr)) => PlatformMetadataResult::decode_esexpr(expr)
                .map_err(|err| metadata_format_error(&input_file, TubeFormatError::from(err))),
            Ok(None) => Err(metadata_parse_error(
                &input_file,
                ParseError::UnexpectedEndOfFile,
            )),
            Err(err) => Err(metadata_parse_error(&input_file, err)),
        }
    };
    if let Err(err) = file.close().await {
        context.runner_reporter().report_error(err);
        return;
    }
    let metadata = match metadata {
        Ok(metadata) => metadata,
        Err(err) => {
            context.runner_reporter().report_error(err);
            return;
        }
    };
    context.add_platform(
        metadata.platform,
        PlatformMetadata {
            metadata: metadata.tube_metadata.into_inner(),
            externs: metadata
                .externs
                .into_iter()
                .map(|(name, extern_)| (name, convert_extern(extern_)))
                .collect::<HashMap<_, _>>(),
        },
    );
}

fn convert_extern(extern_: Box<Extern>) -> PlatformExtern {
    match *extern_ {
        Extern::ExternFunction {
            name: _,
            implementation,
        } => PlatformExtern {
            extern_type: ExternType::Function,
            implementation: implementation.into_inner(),
        },
        Extern::ExternMethod {
            name: _,
            implementation,
        } => PlatformExtern {
            extern_type: ExternType::Method,
            implementation: implementation.into_inner(),
        },
        Extern::ExternStaticMethod {
            name: _,
            implementation,
        } => PlatformExtern {
            extern_type: ExternType::StaticMethod,
            implementation: implementation.into_inner(),
        },
    }
}

fn metadata_format_error<F>(file: &F, error: TubeFormatError) -> InternalCompilerError
where
    F: InputFile,
{
    match error {
        #[cfg(feature = "std")]
        TubeFormatError::FileError(_, err) => InternalCompilerError::TubeFormatError(
            TubeFormatError::FileError(file.path().to_path_buf(), err),
        ),
        error => InternalCompilerError::TubeFormatError(error),
    }
}

fn metadata_parse_error<F>(
    file: &F,
    error: ParseError<InternalCompilerError>,
) -> InternalCompilerError
where
    F: InputFile,
{
    match error {
        ParseError::IOError(err) => match err {
            #[cfg(feature = "std")]
            InternalCompilerError::IoError(_, io_err) => {
                InternalCompilerError::IoError(file.path().to_path_buf(), io_err)
            }
            err => err,
        },
        ParseError::InvalidTokenByte(value) => InternalCompilerError::TubeFormatError(
            TubeFormatError::ExprParseErrorNoIo(ParseError::InvalidTokenByte(value)),
        ),
        ParseError::InvalidStringTableIndex => InternalCompilerError::TubeFormatError(
            TubeFormatError::ExprParseErrorNoIo(ParseError::InvalidStringTableIndex),
        ),
        ParseError::InvalidLength => InternalCompilerError::TubeFormatError(
            TubeFormatError::ExprParseErrorNoIo(ParseError::InvalidLength),
        ),
        ParseError::UnexpectedKeywordToken => InternalCompilerError::TubeFormatError(
            TubeFormatError::ExprParseErrorNoIo(ParseError::UnexpectedKeywordToken),
        ),
        ParseError::UnexpectedConstructorEnd => InternalCompilerError::TubeFormatError(
            TubeFormatError::ExprParseErrorNoIo(ParseError::UnexpectedConstructorEnd),
        ),
        ParseError::UnexpectedEndOfFile => InternalCompilerError::TubeFormatError(
            TubeFormatError::ExprParseErrorNoIo(ParseError::UnexpectedEndOfFile),
        ),
        ParseError::InvalidStringPool(err) => InternalCompilerError::TubeFormatError(
            TubeFormatError::ExprParseErrorNoIo(ParseError::InvalidStringPool(err)),
        ),
        ParseError::Utf8Error(err) => InternalCompilerError::TubeFormatError(
            TubeFormatError::ExprParseErrorNoIo(ParseError::Utf8Error(err)),
        ),
    }
}
