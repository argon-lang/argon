use std::path::PathBuf;

pub fn is_alphabetic(c: char) -> bool {
    c.is_alphabetic()
}

pub fn is_uppercase(c: char) -> bool {
    c.is_uppercase()
}

pub fn is_lowercase(c: char) -> bool {
    c.is_lowercase()
}

pub fn is_whitespace(c: char) -> bool {
    matches!(
        unicode_general_category::get_general_category(c),
        unicode_general_category::GeneralCategory::SpaceSeparator
            | unicode_general_category::GeneralCategory::LineSeparator
            | unicode_general_category::GeneralCategory::ParagraphSeparator
    ) || matches!(c, '\u{0009}'..='\u{000D}' | '\u{0085}')
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexerAcceptance<T> {
    Pending,
    Reject,
    Accept(T),
}

pub trait Lexer {
    type State: Copy;
    type Token;

    const INITIAL_STATE: Self::State;

    fn acceptance(state: Self::State) -> LexerAcceptance<Self::Token>;

    fn step(state: Self::State, c: char) -> Self::State;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FilePosition {
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FilePositionRange {
    pub start: FilePosition,
    pub end: FilePosition,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Location {
    pub file: PathBuf,
    pub start: FilePosition,
    pub end: FilePosition,
}

impl Location {
    #[must_use]
    pub fn merge(&self, other: &Location) -> Location {
        Location {
            file: self.file.clone(),
            start: self.start,
            end: other.end,
        }
    }

    pub fn from_range(file: PathBuf, range: FilePositionRange) -> Location {
        Location {
            file,
            start: range.start,
            end: range.end,
        }
    }
}

#[derive(Debug, Clone)]
pub struct WithRange<T> {
    pub value: T,
    pub range: FilePositionRange,
}

impl<T> WithRange<T> {
    pub fn new(value: T, range: FilePositionRange) -> Self {
        Self { value, range }
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> WithRange<U> {
        WithRange {
            value: f(self.value),
            range: self.range,
        }
    }
}

#[derive(Debug, Clone)]
pub struct WithLocation<T> {
    pub value: T,
    pub location: Location,
}

impl<T> WithLocation<T> {
    pub fn new(value: T, location: Location) -> Self {
        Self { value, location }
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> WithLocation<U> {
        WithLocation {
            value: f(self.value),
            location: self.location,
        }
    }

    pub fn from_range(file_name: PathBuf, range: WithRange<T>) -> WithLocation<T> {
        WithLocation {
            value: range.value,
            location: Location::from_range(file_name, range.range),
        }
    }
}

pub enum ParseResult<T> {
    Success(WithRange<T>),
    Failure,
}

impl<T> ParseResult<T> {
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> ParseResult<U> {
        match self {
            ParseResult::Success(value) => ParseResult::Success(value.map(f)),
            ParseResult::Failure => ParseResult::Failure,
        }
    }

    pub fn flat_map<U>(self, f: impl FnOnce(T) -> ParseResult<U>) -> ParseResult<U> {
        match self {
            ParseResult::Success(value) => match f(value.value) {
                ParseResult::Success(value2) => ParseResult::Success(WithRange {
                    value: value2.value,
                    range: FilePositionRange {
                        start: value.range.start,
                        end: value2.range.end,
                    },
                }),

                ParseResult::Failure => ParseResult::Failure,
            },

            ParseResult::Failure => ParseResult::Failure,
        }
    }
}

pub trait ParserRuntime {
    type Token: Clone;
    type TokenCategory;
    type LexMode;

    fn peek(&mut self) -> ParseResult<Self::Token>;

    fn parser_next(&mut self) -> ParseResult<Self::Token>;

    fn error<T>(&mut self, rule_name: &str, categories: &[Self::TokenCategory]) -> ParseResult<T>;

    fn with_lex_mode<T>(
        &mut self,
        lex_mode: Self::LexMode,
        f: impl FnOnce(&mut Self) -> ParseResult<T>,
    ) -> ParseResult<T>;

    fn recover_with<T>(
        &mut self,
        parse_res: ParseResult<T>,
        fallback: impl FnOnce() -> T,
    ) -> ParseResult<T>;

    fn with_location<T>(&self, parse_res: ParseResult<T>) -> ParseResult<WithLocation<T>>;
}
