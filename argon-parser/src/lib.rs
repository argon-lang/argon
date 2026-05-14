use crate::ast::ModuleDeclaration;
use crate::lexer::{Lexer, LexerReader};
use argon_util::ErrorReporter;
use std::io::Read;
use std::path::PathBuf;

pub mod argon_parser;
pub mod ast;
mod double_quote_string_lexer;
mod lexer;
pub mod token;
mod token_lexer;

use crate::argon_parser::ArgonParser;
use crate::token::TokenCategory;
use parse18_runtime::{Location, ParseResult};

#[derive(Debug, Clone)]
pub enum ParseError {
    LexerError(LexerError),
    ParserError(ParserError),
}

#[derive(Debug, Clone)]
pub struct LexerError {
    pub location: Location,
}

#[derive(Debug, Clone)]
pub struct ParserError {
    pub location: Location,
    pub rule_name: String,
    pub found_token: String,
    pub expected_categories: Vec<TokenCategory>,
}

pub fn parse<R: Read, ER: ErrorReporter<std::io::Error> + ErrorReporter<ParseError>>(
    reader: R,
    file_name: &PathBuf,
    error_reporter: ER,
) -> ModuleDeclaration {
    let lexer_reader = ReadLexerReader {
        reader,
        error_reporter: error_reporter.clone(),
        eof: false,
        decoder_done: false,
        decoder: encoding_rs::UTF_8.new_decoder_with_bom_removal(),
        buffer: [0; 4096],
        pending_start: 0,
        pending_end: 0,
    };
    let lexer = Lexer::new(file_name, lexer_reader, error_reporter.clone());
    let mut parser = ArgonParser::new(file_name, lexer, error_reporter);
    let ParseResult::Success(module_decl) = parser.start_0() else {
        return ModuleDeclaration {
            module_path: vec![],
            stmts: vec![],
        };
    };

    module_decl.value
}

struct ReadLexerReader<R, ER> {
    reader: R,
    error_reporter: ER,
    eof: bool,
    decoder_done: bool,
    decoder: encoding_rs::Decoder,
    buffer: [u8; 4096],
    pending_start: usize,
    pending_end: usize,
}

impl<R: Read, ER: ErrorReporter<std::io::Error>> LexerReader for ReadLexerReader<R, ER> {
    fn next_chunk(&mut self, chars: &mut String) {
        chars.clear();

        if self.decoder_done {
            return;
        }

        if self.pending_start == self.pending_end && !self.eof {
            let bytes_read = match self.reader.read(&mut self.buffer) {
                Ok(count) => count,
                Err(err) => {
                    self.error_reporter.report_error(err);
                    self.eof = true;
                    self.decoder_done = true;
                    return;
                }
            };

            self.pending_start = 0;
            self.pending_end = bytes_read;

            if bytes_read == 0 {
                self.eof = true;
            }
        }

        let input = &self.buffer[self.pending_start..self.pending_end];
        let (status, read, had_errors) = self.decoder.decode_to_string(input, chars, self.eof);

        self.pending_start += read;
        if self.pending_start >= self.pending_end {
            self.pending_start = 0;
            self.pending_end = 0;
        }

        if had_errors {
            self.error_reporter.report_error(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                "input was not valid UTF-8",
            ));
            self.eof = true;
            self.pending_start = 0;
            self.pending_end = 0;
            self.decoder_done = true;
            return;
        }

        if self.eof
            && self.pending_start == self.pending_end
            && matches!(status, encoding_rs::CoderResult::InputEmpty)
        {
            self.decoder_done = true;
        }
    }
}
