use argon_compiler::{Context, Tube, TubeCollectionBuilder};
use argon_format::tube::TubeFileEntry;
use argon_io::{EmbeddedIoRead, InputFile};
use argon_util::{InternalCompilerError, TubeFormatError};
use esexpr::ESExprCodec;
use esexpr_binary::ExprParserSync;
use std::sync::Arc;

pub fn load_referenced_tube<F>(
    context: Context,
    tube_collection_builder: &TubeCollectionBuilder,
    referenced_tube: &F,
) -> Option<Arc<Tube>>
where
    F: InputFile,
{
    let file = match referenced_tube.open() {
        Ok(file) => file,
        Err(e) => {
            context.reporter().report_error(e);
            return None;
        }
    };

    let mut file = EmbeddedIoRead::new(file);
    let mut tube_expr_stream = esexpr_binary::parse_sync(&mut file);
    let tube_entries = std::iter::from_fn(move || {
        let expr = match tube_expr_stream.try_read_next_expr() {
            Ok(Some(expr)) => expr,
            Ok(None) => return None,
            Err(e) => {
                context
                    .reporter()
                    .report_error(tube_format_error(referenced_tube, TubeFormatError::from(e)));
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
    });

    Some(argon_tube::decoder::decode_tube(
        tube_entries,
        tube_collection_builder,
    ))
}

fn tube_format_error<F>(file: &F, error: TubeFormatError) -> InternalCompilerError
where
    F: InputFile,
{
    match error {
        TubeFormatError::FileError(_, err) => InternalCompilerError::TubeFormatError(
            TubeFormatError::FileError(file.path().to_path_buf(), err),
        ),
        error => InternalCompilerError::TubeFormatError(error),
    }
}
