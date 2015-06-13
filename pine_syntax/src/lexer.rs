//! The "lexer" module provides a token stream for Pine programs.
//! The chief exports of this module are token definitions and the Lexer
//! struct, which is an iterator that yields Pine tokens.
//!
//! ## Tokens
//! Tokens are the building blocks upon which the parser builds an
//! abstract syntax tree. A token represents a single syntactical
//! item within the language - a left parenthesis, an integer literal,
//! the `def` keyword, and so on. The `Token` struct (and `TokenType` enum)
//! together represent the tokens that the lexer generates, and that the parser
//! ultimately consumes.
//!
//! A token consists of a span and a variant. The span indicates the position
//! of the token in the source file - it is composed of both a start position
//! and a stop position, each of which are composed of a column and a line number.
//! The variant is a member of the TokenType enum that indicates which type of token
//! this token is and, if applicable, contains some data as to what the token represents.
//! This is used for literals and identifiers, which contain the scanned value of the data
//! type that each represents.
//!
//! ## The Lexer
//! The lexer is a glorified state machine, where each function roughly corresponds to a state.
//! This state machine is exposed as the Lexer struct, which exposes no public methods but instead
//! implements the Iterator trait. Every call to `next` runs the state machine on the input until
//! it reaches an accept state, reaches an EOF, or has some other sort of error. `next` returns
//! either a Token upon success, or a CompileDiagnostic on failure. The parser is expected to
//! abort parsing immediately if it encounters a lexer error in this way.
//!
//! ## Lexical specification
//! There is no official lexical specification for Pine yet, but when I have solidified the syntax
//! of Pine more I will add one here.
use pine_common::{Span, CompileDiagnostic, Position, Severity};

use std::iter::Peekable;
use std::str::FromStr;

use self::TokenType::*;

#[derive(PartialEq, Clone, Debug)]
pub struct Token {
    pub span: Span,
    pub variant: TokenType
}

impl Token {
    pub fn new(start: Position, stop: Position, variant: TokenType) -> Token {
        Token {
            span: Span(start, stop),
            variant: variant
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct LexerError {
    filename: String,
    span: Span,
    severity: Severity,
    kind: LexerErrorKind
}

#[derive(PartialEq, Eq, Copy, Clone, Debug)]
pub enum LexerErrorKind {
    LiteralOverflow,
    UnknownCharacter,
    InvalidNumericLiteral,
    UnterminatedStringLiteral,
    InvalidEscapeCharacter
}

impl From<LexerError> for CompileDiagnostic {
    fn from(err: LexerError) -> CompileDiagnostic {
        let message = match err.kind {
            LexerErrorKind::LiteralOverflow => "value is too big for literal",
            LexerErrorKind::UnknownCharacter => "unknown character",
            LexerErrorKind::InvalidNumericLiteral => "invalid numeric literal",
            LexerErrorKind::UnterminatedStringLiteral => "unexpected EOF while scanning string literal",
            LexerErrorKind::InvalidEscapeCharacter => "invalid escape character"
        };
        CompileDiagnostic {
            filename: err.filename,
            span: err.span,
            severity: err.severity,
            message: message.to_string()
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum TokenType {
    BooleanLiteral(bool),
    IntLiteral(i32),
    FloatLiteral(f32),
    Identifier(String),
    StringLiteral(String),
    UnitLiteral,
    Def,
    Comma,
    Ref,
    If,
    Then,
    Else,
    End,
    Do,
    VBar,
    Let,
    SingleEq,
    In,
    Dot,
    LeftArrow,
    LParen,
    RParen,
    DoubleEq,
    LessThanEq,
    LessThan,
    GreaterThanEq,
    GreaterThan,
    Plus,
    Minus,
    Times,
    Div,
    NotEqual,
    Not,
    Deref,
    And,
    Or
}

/// The Lexer struct wraps an iterator of chars to provide an iterator
/// over Pine tokens. It implements the Iterator trait itself as an entry
/// point to the Lexer state machine.
pub struct Lexer<I: Iterator<Item=char>> {
    filename: String,
    iter: Peekable<I>,
    current_position: Position
}

impl<I: Iterator<Item=char>> Iterator for Lexer<I> {
    type Item = Result<Token, CompileDiagnostic>;

    fn next(&mut self) -> Option<Result<Token, CompileDiagnostic>> {
        if let None = self.iter.peek() {
            None
        } else {
            loop {
                // TODO this is not particularly effective
                self.skip_to_next_non_whitespace_char();
                self.skip_comments();
                if let Some(c) = self.iter.peek() {
                    if !c.is_whitespace() {
                        break;
                    }
                } else {
                    break;
                }
            }
            if let None = self.iter.peek() {
                None
            } else {
                Some(self.initial_state())
            }
        }
    }
}

impl<I: Iterator<Item=char>> Lexer<I> {
    pub fn new(filename: String, iter: I) -> Lexer<I> {
        Lexer {
            filename: filename,
            iter: iter.peekable(),
            current_position: Position(0, 0)
        }
    }

    pub fn filename(&self) -> &str {
        &self.filename
    }

    fn skip_to_next_non_whitespace_char(&mut self) {
        loop {
            match self.iter.peek() {
                Some(c) if c.is_whitespace() => {},
                _ => break
            }
            let c = self.iter.next().unwrap();
            if c == '\n' {
                self.current_position.0 += 1;
                self.current_position.1 = 0;
            } else {
                self.current_position.1 += 1;
            }
        }
    }

    fn skip_comments(&mut self) {
        match self.iter.peek() {
            Some(&'#') => loop {
                match self.iter.next() {
                    Some('\n') => {
                        self.current_position.0 += 1;
                        self.current_position.1 = 0;
                    }
                    Some(_) => {
                        self.current_position.1 += 1;
                    },
                    None => break
                }
            },
            _ => {}
        }
    }

    fn next_char(&mut self) -> Option<char> {
        match self.iter.next() {
            Some(c) => {
                self.current_position.1 += 1;
                Some(c)
            },
            None => None
        }
    }

    fn initial_state(&mut self) -> Result<Token, CompileDiagnostic> {
        // single character tokens can be matched immediately
        let starting_position = self.current_position;
        match self.next_char().unwrap() {
            ',' => Ok(Token::new(starting_position, self.current_position, Comma)),
            '|' => Ok(Token::new(starting_position, self.current_position, VBar)),
            '.' => Ok(Token::new(starting_position, self.current_position, Dot)),
            '(' => Ok(Token::new(starting_position, self.current_position, LParen)),
            ')' => Ok(Token::new(starting_position, self.current_position, RParen)),
            '+' => Ok(Token::new(starting_position, self.current_position, Plus)),
            '-' => Ok(Token::new(starting_position, self.current_position, Minus)),
            '*' => Ok(Token::new(starting_position, self.current_position, Times)),
            '/' => Ok(Token::new(starting_position, self.current_position, Div)),
            '=' => self.single_or_double_eq(starting_position),
            '<' => self.less_operator_or_arrow(starting_position),
            '>' => self.greater_or_geq_operator(starting_position),
            '!' => self.deref_or_not_equal(starting_position),
            '"' => self.string_literal(starting_position),
            c if c.is_digit(10) => self.number(c, starting_position),
            c if c.is_alphabetic() => self.identifier(c, starting_position),
            _ => self.span_err(starting_position, self.current_position, LexerErrorKind::UnknownCharacter)
        }
    }

    fn single_or_double_eq(&mut self, start: Position) -> Result<Token, CompileDiagnostic> {
        // try to scan either == or =, when looking at the first =
        if let Some(&'=') = self.iter.peek() {
            // it's a double equal.
            let _ = self.next_char();
            Ok(Token::new(start, self.current_position, DoubleEq))
        } else {
            // it's a single equal.
            Ok(Token::new(start, self.current_position, SingleEq))
        }
    }

    fn less_operator_or_arrow(&mut self, start: Position) -> Result<Token, CompileDiagnostic> {
        // try to scan either: <-, <=, or < when looking at the first <
        match self.iter.peek() {
            Some(&'-') => {
                let _ = self.next_char();
                Ok(Token::new(start, self.current_position, LeftArrow))
            },
            Some(&'=') => {
                let _ = self.next_char();
                Ok(Token::new(start, self.current_position, LessThanEq))
            },
            _ => {
                Ok(Token::new(start, self.current_position, LessThan))
            }
        }
    }

    fn greater_or_geq_operator(&mut self, start: Position) -> Result<Token, CompileDiagnostic> {
        // try to scan either >= or >
        if let Some(&'=') = self.iter.peek() {
            let _ = self.next_char();
            Ok(Token::new(start, self.current_position, GreaterThanEq))
        } else {
            Ok(Token::new(start, self.current_position, GreaterThan))
        }
    }

    fn deref_or_not_equal(&mut self, start: Position) -> Result<Token, CompileDiagnostic> {
        if let Some(&'=') = self.iter.peek() {
            let _ = self.next_char();
            Ok(Token::new(start, self.current_position, NotEqual))
        } else {
            Ok(Token::new(start, self.current_position, Deref))
        }
    }

    fn number(&mut self, c: char, start: Position) -> Result<Token, CompileDiagnostic> {
        let mut buf = String::new();
        // the iter currently points to a base-10 digit. consume it.
        buf.push(c);
        // while the thing that we're looking at is a digit, grab it
        // and stick it on the buffer.
        let mut seen_dot = false;
        loop {
            let matched = match self.iter.peek() {
                Some(c) => *c,
                None => break
            };
            match matched {
                c if c.is_digit(10) => {
                    let _ = self.next_char();
                    buf.push(c);
                },
                '.' if !seen_dot => {
                    let _ = self.next_char();
                    seen_dot = true;
                    buf.push('.');
                },
                '.' if seen_dot => {
                    return self.span_err(start, self.current_position, LexerErrorKind::InvalidNumericLiteral);
                },
                _ => break
            }
        }
        let tok = if seen_dot {
            // this is a float literal.
            let float_val : Option<f32> = FromStr::from_str(&buf).ok();
            if let Some(value) = float_val {
                Token::new(start, self.current_position, FloatLiteral(value))
            } else {
                return self.span_err(start, self.current_position, LexerErrorKind::InvalidNumericLiteral);
            }
        } else {
            // otherwise, it's a integer literal
            let int_val : Option<i32> = FromStr::from_str(&buf).ok();
            if let Some(value) = int_val {
                Token::new(start, self.current_position, IntLiteral(value))
            } else {
                return self.span_err(start, self.current_position, LexerErrorKind::InvalidNumericLiteral);
            }
        };
        Ok(tok)
    }

    fn identifier(&mut self, c: char, start: Position) -> Result<Token, CompileDiagnostic> {
        let mut buf = String::new();
        // the iterator points to an alphabetical character. consume it.
        buf.push(c);
        loop {
            let matched = match self.iter.peek() {
                Some(c) => *c,
                None => break
            };
            if matched.is_alphanumeric() || matched == '_' {
                let _ = self.next_char();
                buf.push(matched);
            } else {
                break;
            }
        }
        let tok = match extract_keyword(&buf) {
            Some(keyword) => keyword,
            None => TokenType::Identifier(buf)
        };
        Ok(Token::new(start, self.current_position, tok))
    }

    fn string_literal(&mut self, start: Position) -> Result<Token, CompileDiagnostic> {
        // the iterator just scanned a double quote.
        let mut buf = String::new();
        loop {
            let matched = self.iter.peek().map(|x| *x);
            let c = match matched {
                Some(c) => c,
                None => return self.span_err(start, self.current_position, LexerErrorKind::UnterminatedStringLiteral)
            };
            // within the string literal, allow escape characters and
            // things that are not end quotes.
            match c {
                '"' => {
                    let _ = self.next_char().unwrap();
                    break;
                },
                '\\' => {
                    let _ = self.next_char().unwrap();
                    let actual = try!(self.escape_char(start));
                    buf.push(actual);
                },
                c => {
                    let _ = self.next_char().unwrap();
                    buf.push(c);
                }
            }
        }
        // if we've made it here, we scanned a closing double quote.
        Ok(Token::new(start, self.current_position, StringLiteral(buf)))
    }

    fn escape_char(&mut self, start: Position) -> Result<char, CompileDiagnostic> {
        match self.next_char() {
            Some(c) => match c {
                't' => Ok('\t'),
                'n' => Ok('\n'),
                'r' => Ok('\r'),
                '\\' => Ok('\\'),
                '"' => Ok('"'),
                _ => self.span_err(start, self.current_position, LexerErrorKind::InvalidEscapeCharacter)
            },
            None => self.span_err(start, self.current_position, LexerErrorKind::UnterminatedStringLiteral)
        }
    }

    fn span_err<T>(&self, start: Position, stop: Position, kind: LexerErrorKind) -> Result<T, CompileDiagnostic> {
        Err(From::from(LexerError {
            filename: self.filename.clone(),
            span: Span(start, stop),
            severity: Severity::Error,
            kind: kind
        }))
    }
}

fn extract_keyword(string: &str) -> Option<TokenType> {
    match string {
        "def" => Some(Def),
        "ref" => Some(Ref),
        "if"  => Some(If),
        "then" => Some(Then),
        "else" => Some(Else),
        "end" => Some(End),
        "do" => Some(Do),
        "let" => Some(Let),
        "in" => Some(In),
        "not" => Some(Not),
        "true" => Some(BooleanLiteral(true)),
        "false" => Some(BooleanLiteral(false)),
        "and" => Some(And),
        "or" => Some(Or),
        "unit" => Some(UnitLiteral),
        _ => None
    }
}
