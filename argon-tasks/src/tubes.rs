use alloc::sync::Arc;
use argon_compiler::{Context, Tube, TubeCollectionBuilder};
use argon_format::tube::TubeFileEntry;
use argon_io::InputFile;
use argon_util::{InternalCompilerError, TubeFormatError};
use core::iter;
use esexpr::ESExprCodec;
use esexpr_binary::ExprParserSync;
use esexpr_binary::ParseError;

pub fn load_referenced_tube<F>(
    context: Context,
    tube_collection_builder: &TubeCollectionBuilder,
    referenced_tube: &F,
) -> Option<Arc<Tube>>
where
    F: InputFile,
{
    let mut file = match referenced_tube.open() {
        Ok(file) => file,
        Err(e) => {
            context.reporter().report_error(e);
            return None;
        }
    };

    let mut tube_expr_stream = esexpr_binary::parse_sync(&mut file);
    let tube_entries = {
        let context = context.clone();
        iter::from_fn(move || {
            let expr = match tube_expr_stream.try_read_next_expr() {
                Ok(Some(expr)) => expr,
                Ok(None) => return None,
                Err(e) => {
                    context
                        .reporter()
                        .report_error(tube_parse_error(referenced_tube, e));
                    return None;
                }
            };

            let entry = match TubeFileEntry::decode_esexpr(expr) {
                Ok(entry) => entry,
                Err(e) => {
                    context
                        .reporter()
                        .report_error(tube_format_error(referenced_tube, TubeFormatError::from(e)));
                    return None;
                }
            };

            Some(entry)
        })
    };

    Some(argon_tube::decoder::decode_tube(
        context,
        tube_entries,
        tube_collection_builder,
    ))
}

fn tube_format_error<F>(file: &F, error: TubeFormatError) -> InternalCompilerError
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

fn tube_parse_error<F>(file: &F, error: ParseError<InternalCompilerError>) -> InternalCompilerError
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
