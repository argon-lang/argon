use argon_compiler::{Context, Tube, TubeCollectionBuilder};
use argon_format::tube::TubeFileEntry;
use argon_util::{InternalCompilerError, TubeFormatError};
use esexpr::ESExprCodec;
use esexpr_binary::ExprParserSync;
use std::path::Path;
use std::sync::Arc;

pub fn load_referenced_tube(
    context: Context,
    tube_collection_builder: &TubeCollectionBuilder,
    referenced_tube: &Path,
) -> Option<Arc<Tube>> {
    let mut file = match std::fs::File::open(referenced_tube) {
        Ok(file) => file,
        Err(e) => {
            context
                .reporter()
                .report_error(InternalCompilerError::TubeFormatError(
                    TubeFormatError::FileError(referenced_tube.to_path_buf(), e),
                ));
            return None;
        }
    };

    let mut tube_expr_stream = esexpr_binary::parse_sync(&mut file);
    let tube_entries = std::iter::from_fn(move || {
        let expr = match tube_expr_stream.try_read_next_expr() {
            Ok(Some(expr)) => expr,
            Ok(None) => return None,
            Err(e) => {
                context
                    .reporter()
                    .report_error(InternalCompilerError::TubeFormatError(
                        TubeFormatError::from(e),
                    ));
                return None;
            }
        };

        let entry = match TubeFileEntry::decode_esexpr(expr) {
            Ok(entry) => entry,
            Err(e) => {
                context
                    .reporter()
                    .report_error(InternalCompilerError::TubeFormatError(
                        TubeFormatError::from(e),
                    ));
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
