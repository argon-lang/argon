use parse18_runtime::Location;
use std::path::PathBuf;

pub trait ErrorReporter<E> {
    fn report_error(&self, error: E);
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u16)]
pub enum ErrorCode {
    InvalidToken = 0x0001,
    ParseError = 0x0002,
    UnknownModule = 0x0003,
    InvalidModifier = 0x0004,
    DuplicateModifier = 0x0005,
    TypeMismatch = 0x0006,
    InvalidAssignmentTarget = 0x0007,
    TokenExpressionRequired = 0x0008,
    ErasedMustBePure = 0x0009,
    InvalidOverload = 0x000A,
    AmbiguousOverload = 0x000B,
    PurityError = 0x000C,
    ErasedExpressionNotAllowed = 0x000D,
    LoopLabelNotFound = 0x000E,
    LoopBranchOutsideLoop = 0x000F,
    FunctionTypeRequired = 0x0010,
    UnknownIdentifier = 0x0011,
    CanNotMutate = 0x0012,
    TupleSizeMismatch = 0x0013,
    TupleTypeRequired = 0x0014,
    ImplicitNotFound = 0x0015,
    CouldNotInfer = 0x0016,
    AbstractMethod = 0x0017,
    InvalidOverride = 0x0018,
    OverrideAccessNarrowing = 0x0019,
    TokenFunctionNotInline = 0x001A,
    UnknownExtern = 0x001B,
    TypeParameterIsConcrete = 0x001C,
    DuplicateTubeDefinition = 0x001D,
    UnknownTube = 0x001E,
    InvalidBuiltin = 0x001F,
    CircularReexport = 0x0020,
}

impl ErrorCode {
    pub fn code(self) -> u16 {
        self as u16
    }
}

#[derive(Debug, Clone)]
pub struct CompileError {
    pub code: ErrorCode,
    pub message: String,
    pub location: Option<Location>,
}

impl CompileError {
    fn new(code: ErrorCode, message: impl Into<String>, location: Option<Location>) -> Self {
        Self {
            code,
            message: message.into(),
            location,
        }
    }

    pub fn invalid_token(loc: Location) -> Self {
        Self::new(ErrorCode::InvalidToken, "Invalid token", Some(loc))
    }

    pub fn parse_error(
        loc: Location,
        rule_name: impl AsRef<str>,
        found_token: impl AsRef<str>,
        expected_categories: impl IntoIterator<Item = impl AsRef<str>>,
    ) -> Self {
        Self::new(
            ErrorCode::ParseError,
            format!(
                "Error parsing {}: Found: {}, Expected: {}",
                rule_name.as_ref(),
                found_token.as_ref(),
                join_strings(expected_categories)
            ),
            Some(loc),
        )
    }

    pub fn unknown_module(
        loc: Location,
        tube: impl AsRef<str>,
        module_path: impl AsRef<str>,
    ) -> Self {
        Self::new(
            ErrorCode::UnknownModule,
            format!("Unknown module: {}/{}", tube.as_ref(), module_path.as_ref()),
            Some(loc),
        )
    }

    pub fn invalid_modifier(
        loc: Location,
        modifiers: impl IntoIterator<Item = impl AsRef<str>>,
    ) -> Self {
        Self::new(
            ErrorCode::InvalidModifier,
            format!("Invalid modifier: {}", join_strings(modifiers)),
            Some(loc),
        )
    }

    pub fn duplicate_modifier(loc: Location, modifier: impl AsRef<str>) -> Self {
        Self::new(
            ErrorCode::DuplicateModifier,
            format!("Duplicate modifier: {}", modifier.as_ref()),
            Some(loc),
        )
    }

    pub fn type_mismatch(
        loc: Location,
        expected: impl AsRef<str>,
        actual: impl AsRef<str>,
    ) -> Self {
        Self::new(
            ErrorCode::TypeMismatch,
            format!(
                "Type mismatch: Expected {}, found {}",
                expected.as_ref(),
                actual.as_ref()
            ),
            Some(loc),
        )
    }

    pub fn invalid_assignment_target(loc: Location) -> Self {
        Self::new(
            ErrorCode::InvalidAssignmentTarget,
            "Invalid assignment target",
            Some(loc),
        )
    }

    pub fn token_expression_required(loc: Location) -> Self {
        Self::new(
            ErrorCode::TokenExpressionRequired,
            "Token expression required",
            Some(loc),
        )
    }

    pub fn erased_must_be_pure(loc: Location) -> Self {
        Self::new(
            ErrorCode::ErasedMustBePure,
            "Erased expression must be pure",
            Some(loc),
        )
    }

    pub fn invalid_overload(loc: Location) -> Self {
        Self::new(ErrorCode::InvalidOverload, "Invalid overload", Some(loc))
    }

    pub fn ambiguous_overload(loc: Location) -> Self {
        Self::new(
            ErrorCode::AmbiguousOverload,
            "Ambiguous overload",
            Some(loc),
        )
    }

    pub fn purity_error(loc: Location) -> Self {
        Self::new(ErrorCode::PurityError, "Purity error", Some(loc))
    }

    pub fn erased_expression_not_allowed(loc: Location) -> Self {
        Self::new(
            ErrorCode::ErasedExpressionNotAllowed,
            "Erased expression not allowed",
            Some(loc),
        )
    }

    pub fn loop_label_not_found(loc: Location) -> Self {
        Self::new(
            ErrorCode::LoopLabelNotFound,
            "Loop label not found",
            Some(loc),
        )
    }

    pub fn loop_branch_outside_loop(loc: Location) -> Self {
        Self::new(
            ErrorCode::LoopBranchOutsideLoop,
            "Loop branch outside loop",
            Some(loc),
        )
    }

    pub fn function_type_required(loc: Location, t: impl AsRef<str>) -> Self {
        Self::new(
            ErrorCode::FunctionTypeRequired,
            format!("Function type required. Got: {}", t.as_ref()),
            Some(loc),
        )
    }

    pub fn unknown_identifier(loc: Location, id: impl AsRef<str>) -> Self {
        Self::new(
            ErrorCode::UnknownIdentifier,
            format!("Unknown identifier: {}", id.as_ref()),
            Some(loc),
        )
    }

    pub fn can_not_mutate(loc: Location) -> Self {
        Self::new(ErrorCode::CanNotMutate, "Can not mutate", Some(loc))
    }

    pub fn tuple_size_mismatch(loc: Location, t: usize, actual_size: usize) -> Self {
        Self::new(
            ErrorCode::TupleSizeMismatch,
            format!(
                "Tuple size mismatch. Expected a tuple of size {}, Got: {}",
                actual_size, t
            ),
            Some(loc),
        )
    }

    pub fn tuple_type_required(loc: Location, t: impl AsRef<str>) -> Self {
        Self::new(
            ErrorCode::TupleTypeRequired,
            format!("Tuple type required. Got: {}", t.as_ref()),
            Some(loc),
        )
    }

    pub fn implicit_not_found(loc: Location) -> Self {
        Self::new(ErrorCode::ImplicitNotFound, "Implicit not found", Some(loc))
    }

    pub fn could_not_infer(loc: Location) -> Self {
        Self::new(ErrorCode::CouldNotInfer, "Could not infer type", Some(loc))
    }

    pub fn abstract_method_error(loc: Location) -> Self {
        Self::new(
            ErrorCode::AbstractMethod,
            "Abstract method in non-abstract type",
            Some(loc),
        )
    }

    pub fn invalid_override(loc: Location) -> Self {
        Self::new(ErrorCode::InvalidOverride, "Invalid override", Some(loc))
    }

    pub fn override_access_narrowing(
        loc: Location,
        _expected_access: impl AsRef<str>,
        _actual_access: impl AsRef<str>,
    ) -> Self {
        Self::new(
            ErrorCode::OverrideAccessNarrowing,
            "Override access narrowing",
            Some(loc),
        )
    }

    pub fn token_function_not_inline(loc: Location) -> Self {
        Self::new(
            ErrorCode::TokenFunctionNotInline,
            "Token function is not inline",
            Some(loc),
        )
    }

    pub fn unknown_extern(loc: Location, name: impl AsRef<str>) -> Self {
        Self::new(
            ErrorCode::UnknownExtern,
            format!("Unknown extern: {}", name.as_ref()),
            Some(loc),
        )
    }

    pub fn type_parameter_is_concrete(loc: Location) -> Self {
        Self::new(
            ErrorCode::TypeParameterIsConcrete,
            "Type parameter is concrete",
            Some(loc),
        )
    }

    pub fn duplicate_tube_definition(name: impl AsRef<str>) -> Self {
        Self::new(
            ErrorCode::DuplicateTubeDefinition,
            format!("Duplicate tube definition: {}", name.as_ref()),
            None,
        )
    }

    pub fn unknown_tube(tube: impl AsRef<str>) -> Self {
        Self::new(
            ErrorCode::UnknownTube,
            format!("Unknown tube: {}", tube.as_ref()),
            None,
        )
    }

    pub fn invalid_builtin(loc: Option<Location>, name: impl AsRef<str>) -> Self {
        Self::new(
            ErrorCode::InvalidBuiltin,
            format!("Invalid builtin: {}", name.as_ref()),
            loc,
        )
    }

    pub fn circular_reexport(loc: Location, module_path: impl AsRef<str>) -> Self {
        Self::new(
            ErrorCode::CircularReexport,
            format!(
                "Circular reexport involving module: {}",
                module_path.as_ref()
            ),
            Some(loc),
        )
    }
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[AR{:04X}] ", self.code.code())?;

        if let Some(loc) = &self.location {
            write!(
                f,
                "{}:{}:{}-{}:{} ",
                loc.file.display(),
                loc.start.line,
                loc.start.column,
                loc.end.line,
                loc.end.column
            )?;
        }

        f.write_str(&self.message)
    }
}

impl std::error::Error for CompileError {}

fn join_strings(values: impl IntoIterator<Item = impl AsRef<str>>) -> String {
    let mut iter = values.into_iter();
    let Some(first) = iter.next() else {
        return String::new();
    };

    let mut result = first.as_ref().to_owned();
    for value in iter {
        result.push_str(", ");
        result.push_str(value.as_ref());
    }
    result
}

#[derive(Debug)]
pub enum TubeFormatError {
    FileError(PathBuf, std::io::Error),
    ExprParseError(esexpr_binary::ParseError<std::io::Error>),
    ExprDecodeError(esexpr::DecodeError),
}

impl std::fmt::Display for TubeFormatError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::FileError(path, err) => {
                write!(f, "tube format file error: {}: {err}", path.display())
            }
            Self::ExprParseError(err) => write!(f, "tube format parse error: {err:?}"),
            Self::ExprDecodeError(err) => write!(f, "tube format decode error: {err:?}"),
        }
    }
}

impl From<esexpr_binary::ParseError<std::io::Error>> for TubeFormatError {
    fn from(err: esexpr_binary::ParseError<std::io::Error>) -> Self {
        Self::ExprParseError(err)
    }
}

impl From<esexpr::DecodeError> for TubeFormatError {
    fn from(err: esexpr::DecodeError) -> Self {
        Self::ExprDecodeError(err)
    }
}

#[derive(Debug)]
pub enum TubeEncodingError {
    GeneratorError(esexpr_binary::GeneratorError<std::io::Error>),
}

impl std::fmt::Display for TubeEncodingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::GeneratorError(err) => write!(f, "tube encoding generator error: {err:?}"),
        }
    }
}

#[derive(Debug)]
pub enum InternalCompilerError {
    IoError(PathBuf, std::io::Error),
    WalkDirError(walkdir::Error),
    TubeFormatError(TubeFormatError),
    TubeEncodingError(TubeEncodingError),
}

impl std::fmt::Display for InternalCompilerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::IoError(path, err) => write!(f, "I/O error: {}: {err}", path.display()),
            Self::WalkDirError(err) => write!(f, "directory traversal error: {err}"),
            Self::TubeFormatError(err) => write!(f, "{err}"),
            Self::TubeEncodingError(err) => write!(f, "{err}"),
        }
    }
}

impl std::error::Error for InternalCompilerError {}
