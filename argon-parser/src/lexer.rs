use crate::token::{StringTokenType, Token, TokenType};
use alloc::{boxed::Box, collections::VecDeque, string::String};
use argon_util::{CompileError, ErrorReporter};
use num_bigint::BigUint;
use num_traits::Num;
use parse18_runtime::{FilePosition, FilePositionRange, Lexer as Parse18Lexer, LexerAcceptance, Location, LocationFileView, WithRange};
use alloc::borrow::ToOwned;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LexerMode {
    Normal,
    SkipNewLines,
    StringText,
}

pub enum LexedToken {
    Token(WithRange<Token>),
    EndOfFile(FilePosition),
}

pub trait LexerReader {
    fn next_chunk(&mut self, buffer: &mut String);
}

pub trait TokenReader {
    fn next_token(&mut self, mode: LexerMode) -> LexedToken;
}

pub struct Lexer<'a, R, ER: ?Sized> {
    reader: R,
    error_reporter: &'a ER,
    file_name: &'a LocationFileView,

    has_eof: bool,
    token_start_pos: FilePosition,
    acc_text: VecDeque<char>,
    current_pos: FilePosition,
    current_text: String,
    current_text_offset: usize,
    text_buffer: String,
}

impl<'a, R: LexerReader, ER: ErrorReporter<CompileError> + ?Sized> Lexer<'a, R, ER> {
    pub fn new(file_name: &'a LocationFileView, reader: R, error_reporter: &'a ER) -> Self {
        Lexer {
            reader,
            error_reporter,
            file_name,
            has_eof: false,
            token_start_pos: FilePosition { line: 1, column: 1 },
            current_pos: FilePosition { line: 1, column: 1 },
            acc_text: VecDeque::new(),
            current_text: String::with_capacity(2048),
            current_text_offset: 0,
            text_buffer: String::new(),
        }
    }
}

impl<'a, R: LexerReader, ER: ErrorReporter<CompileError> + ?Sized> TokenReader
    for Lexer<'a, R, ER>
{
    fn next_token(&mut self, mode: LexerMode) -> LexedToken {
        match mode {
            LexerMode::Normal => NormalTokenProcessor::next_token(self),
            LexerMode::SkipNewLines => SkipNewLinesTokenProcessor::next_token(self),
            LexerMode::StringText => StringTokenProcessor::next_token(self),
        }
    }
}

trait TokenProcessor {
    type Lexer: Parse18Lexer;
    fn process_token<R: LexerReader, ER: ErrorReporter<CompileError> + ?Sized>(
        lexer: &mut Lexer<R, ER>,
        lex_token: <Self::Lexer as Parse18Lexer>::Token,
    ) -> Option<WithRange<Token>>;

    fn current_token_or_error<R: LexerReader, ER: ErrorReporter<CompileError> + ?Sized>(
        lexer: &mut Lexer<R, ER>,
        state: <Self::Lexer as Parse18Lexer>::State,
    ) -> Option<WithRange<Token>> {
        if let LexerAcceptance::Accept(token) = Self::Lexer::acceptance(state) {
            Self::process_token(lexer, token)
        } else {
            lexer
                .error_reporter
                .report_error(CompileError::invalid_token(Location {
                    file: lexer.file_name.to_owned(),
                    start: lexer.token_start_pos,
                    end: lexer.current_pos,
                }));
            lexer.text_buffer.clear();
            lexer.text_buffer.extend(lexer.acc_text.iter().copied());
            let error_token = Token::Error {
                invalid_text: Box::from(lexer.text_buffer.as_str()),
            };
            let range = FilePositionRange {
                start: lexer.token_start_pos,
                end: lexer.current_pos,
            };

            Some(WithRange::new(error_token, range))
        }
    }

    fn next_token<R: LexerReader, ER: ErrorReporter<CompileError> + ?Sized>(
        lexer: &mut Lexer<R, ER>,
    ) -> LexedToken {
        let mut state = Self::Lexer::INITIAL_STATE;

        loop {
            if lexer.current_text_offset < lexer.current_text.len() {
                #[allow(
                    clippy::unwrap_used,
                    reason = "Safe because we just checked that the offset is within the string length and sliced it to that point"
                )]
                let c = lexer.current_text[lexer.current_text_offset..]
                    .chars()
                    .next()
                    .unwrap();

                let next_state = Self::Lexer::step(state, c);

                if let LexerAcceptance::Reject = Self::Lexer::acceptance(next_state) {
                    let token = Self::current_token_or_error(lexer, state);

                    lexer.acc_text.clear();

                    if let Some(token) = token {
                        return LexedToken::Token(token);
                    }

                    state = Self::Lexer::INITIAL_STATE;
                } else {
                    lexer.current_text_offset += c.len_utf8();

                    state = next_state;
                    lexer.acc_text.push_back(c);

                    if c == '\n' {
                        lexer.current_pos.line += 1;
                        lexer.current_pos.column = 1;
                    } else {
                        lexer.current_pos.column += 1;
                    }
                }
            } else if lexer.has_eof {
                return LexedToken::EndOfFile(lexer.current_pos);
            } else {
                lexer.reader.next_chunk(&mut lexer.current_text);
                if lexer.current_text.is_empty() {
                    lexer.has_eof = true;

                    if !lexer.acc_text.is_empty()
                        && let Some(token) = Self::current_token_or_error(lexer, state)
                    {
                        return LexedToken::Token(token);
                    }
                }
            }
        }
    }
}

struct NormalTokenProcessor;

impl TokenProcessor for NormalTokenProcessor {
    type Lexer = crate::token_lexer::TokenLexer;

    fn process_token<R: LexerReader, ER: ErrorReporter<CompileError> + ?Sized>(
        lexer: &mut Lexer<R, ER>,
        lex_token: <Self::Lexer as Parse18Lexer>::Token,
    ) -> Option<WithRange<Token>> {
        let token_opt = match lex_token {
            TokenType::Token(t) => Some(t),
            TokenType::Whitespace => None,

            TokenType::DecInteger => {
                lexer.text_buffer.clear();
                lexer.text_buffer.extend(lexer.acc_text.iter().copied());
                let n = BigUint::from_str_radix(&lexer.text_buffer, 10)
                    .expect("Lexer produced token with invalid decimal integer value");
                Some(Token::IntToken { value: n })
            }
            TokenType::BinInteger => {
                lexer.text_buffer.clear();
                lexer
                    .text_buffer
                    .extend(lexer.acc_text.iter().copied().skip(2));
                let n = BigUint::from_str_radix(&lexer.text_buffer, 2)
                    .expect("Lexer produced token with invalid binary integer value");
                Some(Token::IntToken { value: n })
            }
            TokenType::OctInteger => {
                lexer.text_buffer.clear();
                lexer
                    .text_buffer
                    .extend(lexer.acc_text.iter().copied().skip(2));
                let n = BigUint::from_str_radix(&lexer.text_buffer, 8)
                    .expect("Lexer produced token with invalid octal integer value");
                Some(Token::IntToken { value: n })
            }
            TokenType::HexInteger => {
                lexer.text_buffer.clear();
                lexer
                    .text_buffer
                    .extend(lexer.acc_text.iter().copied().skip(2));
                let n = BigUint::from_str_radix(&lexer.text_buffer, 16)
                    .expect("Lexer produced token with invalid hexadecimal integer value");
                Some(Token::IntToken { value: n })
            }
            TokenType::Identifier => {
                lexer.text_buffer.clear();
                lexer.text_buffer.extend(lexer.acc_text.iter().copied());
                Some(Token::Identifier {
                    name: Box::from(lexer.text_buffer.as_str()),
                })
            }
            TokenType::InvalidInteger => {
                lexer.text_buffer.clear();
                lexer.text_buffer.extend(lexer.acc_text.iter().copied());
                lexer
                    .error_reporter
                    .report_error(CompileError::invalid_token(Location {
                        file: lexer.file_name.to_owned(),
                        start: lexer.token_start_pos,
                        end: lexer.current_pos,
                    }));
                Some(Token::Error {
                    invalid_text: Box::from(lexer.text_buffer.as_str()),
                })
            }
        };

        let pos = lexer.current_pos;
        let token_with_loc = token_opt.map(|token| {
            WithRange::new(
                token,
                FilePositionRange {
                    start: lexer.token_start_pos,
                    end: pos,
                },
            )
        });

        lexer.token_start_pos = lexer.current_pos;

        token_with_loc
    }
}

struct SkipNewLinesTokenProcessor;

impl TokenProcessor for SkipNewLinesTokenProcessor {
    type Lexer = crate::token_lexer::TokenLexer;

    fn process_token<R: LexerReader, ER: ErrorReporter<CompileError> + ?Sized>(
        lexer: &mut Lexer<R, ER>,
        lex_token: <Self::Lexer as Parse18Lexer>::Token,
    ) -> Option<WithRange<Token>> {
        match NormalTokenProcessor::process_token(lexer, lex_token) {
            Some(WithRange {
                value: Token::NewLine,
                range: _,
            }) => None,
            token => token,
        }
    }
}

struct StringTokenProcessor;

impl TokenProcessor for StringTokenProcessor {
    type Lexer = crate::double_quote_string_lexer::StringLexer;

    fn process_token<R: LexerReader, ER: ErrorReporter<CompileError> + ?Sized>(
        lexer: &mut Lexer<R, ER>,
        lex_token: <Self::Lexer as Parse18Lexer>::Token,
    ) -> Option<WithRange<Token>> {
        let pos = lexer.current_pos;

        let string_token = |s: &str| {
            WithRange::new(
                Token::StringText { text: Box::from(s) },
                FilePositionRange {
                    start: lexer.token_start_pos,
                    end: pos,
                },
            )
        };

        let token = match lex_token {
            StringTokenType::LiteralText => {
                lexer.text_buffer.clear();
                lexer.text_buffer.extend(lexer.acc_text.iter().copied());

                string_token(lexer.text_buffer.as_str())
            }
            StringTokenType::EscapedBackspace => string_token("\x08"),
            StringTokenType::EscapedFormFeed => string_token("\x0C"),
            StringTokenType::EscapedNewLine => string_token("\n"),
            StringTokenType::EscapedCarriageReturn => string_token("\r"),
            StringTokenType::EscapedTab => string_token("\t"),
            StringTokenType::EscapedVerticalTab => string_token("\x0B"),
            StringTokenType::EscapedBackslash => string_token("\\"),
            StringTokenType::EscapedSingleQuote => string_token("'"),
            StringTokenType::EscapedDoubleQuote => string_token("\""),
            StringTokenType::EscapedHash => string_token("#"),
            StringTokenType::EscapedOpenBracket => string_token("["),
            StringTokenType::EscapedCloseBracket => string_token("]"),
            StringTokenType::EscapedOpenCurly => string_token("{"),
            StringTokenType::EscapedCloseCurly => string_token("}"),
            StringTokenType::EscapedUnicodeCodePoint => {
                let num_chars = lexer.acc_text.len();
                lexer.text_buffer.clear();
                lexer
                    .text_buffer
                    .extend(lexer.acc_text.iter().copied().take(num_chars - 1).skip(3));
                let escaped_char_str = parse_unicode_hex_str(&mut lexer.text_buffer)?;
                string_token(escaped_char_str)
            }
            StringTokenType::InterpolationStart => WithRange::new(
                Token::StringInterpolationStart,
                FilePositionRange {
                    start: lexer.token_start_pos,
                    end: pos,
                },
            ),
            StringTokenType::EndOfString => WithRange::new(
                Token::StringEnd,
                FilePositionRange {
                    start: lexer.token_start_pos,
                    end: pos,
                },
            ),
        };

        lexer.token_start_pos = pos;

        Some(token)
    }
}

fn parse_unicode_hex_str(buffer: &mut String) -> Option<&str> {
    let code_point = u32::from_str_radix(buffer, 16).ok()?;
    let code_point = char::from_u32(code_point)?;
    buffer.clear();
    buffer.push(code_point);
    Some(buffer.as_str())
}

#[cfg(test)]
mod tests {
    use crate::lexer::{LexedToken, Lexer, LexerMode, LexerReader, TokenReader};
    use crate::token::Token;
    use alloc::{borrow::ToOwned, boxed::Box, string::String, vec, vec::Vec};
    use argon_util::{CompileError, ErrorReporter};
    use num_bigint::BigUint;
    use num_traits::Num;
    use std::cell::RefCell;
    use std::path::PathBuf;
    use std::rc::Rc;

    #[derive(Clone, Default)]
    struct TestErrorReporter {
        errors: Rc<RefCell<Vec<CompileError>>>,
    }

    impl TestErrorReporter {
        fn errors(&self) -> Vec<CompileError> {
            self.errors.borrow().clone()
        }
    }

    impl ErrorReporter<CompileError> for TestErrorReporter {
        fn report_error(&self, error: CompileError) {
            self.errors.borrow_mut().push(error);
        }
    }

    struct TestReader {
        text: String,
        consumed: bool,
    }

    impl TestReader {
        fn new(text: &str) -> Self {
            Self {
                text: text.to_owned(),
                consumed: false,
            }
        }
    }

    impl LexerReader for TestReader {
        fn next_chunk(&mut self, buffer: &mut String) {
            buffer.clear();
            if !self.consumed {
                self.consumed = true;
                buffer.push_str(&self.text);
            }
        }
    }

    fn lex_with_errors(mode: LexerMode, text: &str) -> (Vec<Token>, Vec<CompileError>) {
        let file_name = PathBuf::from("<test>");
        let reader = TestReader::new(text);
        let error_reporter = TestErrorReporter::default();
        let mut lexer = Lexer::new(&file_name, reader, &error_reporter);
        let mut tokens = Vec::new();

        loop {
            match lexer.next_token(mode) {
                LexedToken::Token(token) => tokens.push(token.value),
                LexedToken::EndOfFile(_) => break,
            }
        }

        (tokens, error_reporter.errors())
    }

    fn lex(mode: LexerMode, text: &str) -> Vec<Token> {
        let (tokens, errors) = lex_with_errors(mode, text);
        assert!(
            errors.is_empty(),
            "expected no lexer errors, got {errors:?}"
        );
        tokens
    }

    fn lex_normal(text: &str) -> Vec<Token> {
        lex(LexerMode::Normal, text)
    }

    fn lex_string(text: &str) -> Vec<Token> {
        lex(LexerMode::StringText, text)
    }

    #[test]
    fn let_keyword() {
        assert_eq!(lex_normal("let"), vec![Token::KwLet]);
    }

    #[test]
    fn let_prefix_letter() {
        assert_eq!(
            lex_normal("lets"),
            vec![Token::Identifier {
                name: Box::from("lets")
            }]
        );
    }

    #[test]
    fn let_prefix_bang() {
        assert_eq!(
            lex_normal("let!"),
            vec![Token::Identifier {
                name: Box::from("let!")
            }]
        );
    }

    #[test]
    fn let_keyword_identifier() {
        assert_eq!(
            lex_normal("let s"),
            vec![
                Token::KwLet,
                Token::Identifier {
                    name: Box::from("s")
                }
            ]
        );
    }

    #[test]
    fn box_keyword() {
        assert_eq!(lex_normal("box"), vec![Token::KwBox]);
    }

    #[test]
    fn boxed_keyword() {
        assert_eq!(lex_normal("boxed"), vec![Token::KwBoxed]);
    }

    #[test]
    fn plus() {
        assert_eq!(lex_normal("+"), vec![Token::OpPlus]);
    }

    #[test]
    fn minus() {
        assert_eq!(lex_normal("-"), vec![Token::OpMinus]);
    }

    #[test]
    fn concat() {
        assert_eq!(lex_normal("++"), vec![Token::OpConcat]);
    }

    #[test]
    fn double_plus() {
        assert_eq!(lex_normal("+ +"), vec![Token::OpPlus, Token::OpPlus]);
    }

    #[test]
    fn simple_decimal_integer() {
        assert_eq!(
            lex_normal("123"),
            vec![Token::IntToken {
                value: BigUint::from(123u32)
            }]
        );
    }

    #[test]
    fn decimal_integer_with_leading_zero() {
        assert_eq!(
            lex_normal("0123"),
            vec![Token::IntToken {
                value: BigUint::from(123u32)
            }]
        );
    }

    #[test]
    fn negative_decimal_integer() {
        assert_eq!(
            lex_normal("-123"),
            vec![
                Token::OpMinus,
                Token::IntToken {
                    value: BigUint::from(123u32)
                }
            ]
        );
    }

    #[test]
    fn invalid_decimal_integer() {
        let (tokens, errors) = lex_with_errors(LexerMode::Normal, "123A");
        assert_eq!(
            tokens,
            vec![Token::Error {
                invalid_text: Box::from("123A")
            }]
        );
        assert_eq!(errors.len(), 1);
    }

    #[test]
    fn binary_integer_lowercase() {
        assert_eq!(
            lex_normal("0b101010"),
            vec![Token::IntToken {
                value: BigUint::from(42u32)
            }]
        );
    }

    #[test]
    fn binary_integer_uppercase_b() {
        assert_eq!(
            lex_normal("0B101010"),
            vec![Token::IntToken {
                value: BigUint::from(42u32)
            }]
        );
    }

    #[test]
    fn negative_binary_integer() {
        assert_eq!(
            lex_normal("-0b101010"),
            vec![
                Token::OpMinus,
                Token::IntToken {
                    value: BigUint::from(42u32)
                }
            ]
        );
    }

    #[test]
    fn invalid_binary_integer() {
        let (tokens, errors) = lex_with_errors(LexerMode::Normal, "0b101010A");
        assert_eq!(
            tokens,
            vec![Token::Error {
                invalid_text: Box::from("0b101010A")
            }]
        );
        assert_eq!(errors.len(), 1);
    }

    #[test]
    fn octal_integer() {
        assert_eq!(
            lex_normal("0o1234567"),
            vec![Token::IntToken {
                value: BigUint::from_str_radix("1234567", 8).unwrap()
            }]
        );
    }

    #[test]
    fn negative_octal_integer() {
        assert_eq!(
            lex_normal("-0o1234567"),
            vec![
                Token::OpMinus,
                Token::IntToken {
                    value: BigUint::from_str_radix("1234567", 8).unwrap()
                }
            ]
        );
    }

    #[test]
    fn invalid_octal_integer() {
        let (tokens, errors) = lex_with_errors(LexerMode::Normal, "0o1234567A");
        assert_eq!(
            tokens,
            vec![Token::Error {
                invalid_text: Box::from("0o1234567A")
            }]
        );
        assert_eq!(errors.len(), 1);
    }

    #[test]
    fn hex_integer_lowercase() {
        assert_eq!(
            lex_normal("0x123456789abcdef"),
            vec![Token::IntToken {
                value: BigUint::from_str_radix("123456789abcdef", 16).unwrap()
            }]
        );
    }

    #[test]
    fn hex_integer_uppercase_x() {
        assert_eq!(
            lex_normal("0X123456789abcdef"),
            vec![Token::IntToken {
                value: BigUint::from_str_radix("123456789abcdef", 16).unwrap()
            }]
        );
    }

    #[test]
    fn hex_integer_uppercase_digits() {
        assert_eq!(
            lex_normal("0x123456789ABCDEF"),
            vec![Token::IntToken {
                value: BigUint::from_str_radix("123456789ABCDEF", 16).unwrap()
            }]
        );
    }

    #[test]
    fn hex_integer_uppercase_x_and_digits() {
        assert_eq!(
            lex_normal("0X123456789ABCDEF"),
            vec![Token::IntToken {
                value: BigUint::from_str_radix("123456789ABCDEF", 16).unwrap()
            }]
        );
    }

    #[test]
    fn negative_hex_integer() {
        assert_eq!(
            lex_normal("-0x123456789abcdef"),
            vec![
                Token::OpMinus,
                Token::IntToken {
                    value: BigUint::from_str_radix("123456789abcdef", 16).unwrap()
                }
            ]
        );
    }

    #[test]
    fn invalid_hex_integer() {
        let (tokens, errors) = lex_with_errors(LexerMode::Normal, "0x123456789abcdefG");
        assert_eq!(
            tokens,
            vec![Token::Error {
                invalid_text: Box::from("0x123456789abcdefG")
            }]
        );
        assert_eq!(errors.len(), 1);
    }

    #[test]
    fn simple_identifier() {
        assert_eq!(
            lex_normal("s"),
            vec![Token::Identifier {
                name: Box::from("s")
            }]
        );
    }

    #[test]
    fn identifier_with_digits() {
        assert_eq!(
            lex_normal("s1"),
            vec![Token::Identifier {
                name: Box::from("s1")
            }]
        );
    }

    #[test]
    fn identifier_with_underscore_in_middle() {
        assert_eq!(
            lex_normal("some_name"),
            vec![Token::Identifier {
                name: Box::from("some_name")
            }]
        );
    }

    #[test]
    fn identifier_starting_with_underscore_not_lone_underscore() {
        assert_eq!(
            lex_normal("_value1"),
            vec![Token::Identifier {
                name: Box::from("_value1")
            }]
        );
    }

    #[test]
    fn lone_underscore_is_keyword() {
        assert_eq!(lex_normal("_"), vec![Token::KwUnderscore]);
    }

    #[test]
    fn identifier_ending_with_question_mark() {
        assert_eq!(
            lex_normal("isEmpty?"),
            vec![Token::Identifier {
                name: Box::from("isEmpty?")
            }]
        );
    }

    #[test]
    fn identifier_ending_with_exclamation_mark() {
        assert_eq!(
            lex_normal("bang!"),
            vec![Token::Identifier {
                name: Box::from("bang!")
            }]
        );
    }

    #[test]
    fn only_one_trailing_terminator_following_bang_is_operator() {
        assert_eq!(
            lex_normal("a?!"),
            vec![
                Token::Identifier {
                    name: Box::from("a?")
                },
                Token::OpLogicalNot
            ]
        );
    }

    #[test]
    fn identifier_in_pipes() {
        assert_eq!(
            lex_normal("|a|"),
            vec![
                Token::SymPipe,
                Token::Identifier {
                    name: Box::from("a")
                },
                Token::SymPipe
            ]
        );
    }

    #[test]
    fn string_start() {
        assert_eq!(lex_normal("\""), vec![Token::StringStart]);
    }

    #[test]
    fn simple_string() {
        assert_eq!(
            lex_string("abc\""),
            vec![
                Token::StringText {
                    text: Box::from("abc")
                },
                Token::StringEnd
            ]
        );
    }

    #[test]
    fn string_with_escaped_characters() {
        assert_eq!(
            lex_string("abc\\ndef\""),
            vec![
                Token::StringText {
                    text: Box::from("abc")
                },
                Token::StringText {
                    text: Box::from("\n")
                },
                Token::StringText {
                    text: Box::from("def")
                },
                Token::StringEnd,
            ]
        );
    }

    #[test]
    fn string_interpolation_with_variable() {
        assert_eq!(
            lex_string("a #{"),
            vec![
                Token::StringText {
                    text: Box::from("a ")
                },
                Token::StringInterpolationStart,
            ]
        );
    }

    #[test]
    fn simple_new_line() {
        assert_eq!(lex_normal("\n"), vec![Token::NewLine]);
    }
}
