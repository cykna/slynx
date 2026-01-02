pub mod error;
use std::{collections::VecDeque, ops::Index};

use crate::parser::{
    ast::Span,
    lexer::{
        error::LexerError,
        tokens::{Token, TokenKind},
    },
};

pub mod tokens;
#[derive(Debug)]
///A stream of tokens to be used when parsing the content
pub struct TokenStream {
    pub stream: VecDeque<Token>,
    pub new_lines: Vec<usize>,
}

impl Iterator for TokenStream {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        self.stream.pop_front()
    }
}

impl Index<usize> for TokenStream {
    type Output = Token;

    fn index(&self, index: usize) -> &Self::Output {
        &self.stream[index]
    }
}
pub struct Lexer;

impl Lexer {
    pub fn tokenize(source: &str) -> Result<TokenStream, LexerError> {
        let mut out = VecDeque::new();
        let mut lines = Vec::new();
        let chars = source.chars().collect::<Vec<_>>();
        let mut idx = 0;
        while idx < chars.len() {
            let tk = match chars[idx] {
                '.' => Token::dot(idx),
                '(' => Token::lparen(idx),
                ')' => Token::rparen(idx),
                '{' => Token::lbrace(idx),
                '}' => Token::rbrace(idx),
                ';' => Token::semicolon(idx),
                '<' => {
                    if chars[idx + 1] == '=' {
                        idx += 1;
                        Token::lte(idx)
                    } else {
                        Token::lt(idx)
                    }
                }
                '>' => {
                    if chars[idx + 1] == '=' {
                        idx += 1;
                        Token::gte(idx)
                    } else {
                        Token::gt(idx)
                    }
                }
                '=' => {
                    if chars[idx + 1] == '=' {
                        idx += 1;
                        Token::eqeq(idx)
                    } else {
                        Token::eq(idx)
                    }
                }
                ':' => Token::colon(idx),
                ',' => Token::comma(idx),
                '+' => {
                    if chars[idx + 1] == '=' {
                        idx += 1;
                        Token::pluseq(idx)
                    } else {
                        Token::plus(idx)
                    }
                }
                '-' => {
                    if chars[idx + 1] == '=' {
                        idx += 1;
                        Token::subeq(idx)
                    } else if chars[idx + 1] == '>' {
                        let out = Token::arrow(idx);
                        idx += 1;
                        out
                    } else {
                        Token::sub(idx)
                    }
                }
                '*' => {
                    if chars[idx + 1] == '=' {
                        idx += 1;
                        Token::stareq(idx)
                    } else {
                        Token::star(idx)
                    }
                }
                '/' => {
                    if chars[idx + 1] == '=' {
                        idx += 1;
                        Token::slasheq(idx)
                    } else {
                        Token::slash(idx)
                    }
                }
                '"' => {
                    let mut buffer = String::new();
                    idx += 1;
                    let start = idx;
                    while idx < chars.len() && chars[idx] != '"' {
                        if chars[idx] == '\\' {
                            idx += 1;
                            match chars[idx] {
                                'n' => buffer.push('\n'),
                                't' => buffer.push('\t'),
                                'r' => buffer.push('\r'),
                                c @ '0'..='9' => buffer.push(c),
                                '\\' => buffer.push('\\'),
                                '"' => buffer.push('"'),
                                _ => buffer.push('\\'),
                            }
                        } else {
                            buffer.push(chars[idx]);
                        }

                        idx += 1;
                    }
                    Token::string(buffer, start, idx)
                }
                c if c.is_whitespace() => {
                    if c == '\n' {
                        lines.push(idx);
                    }
                    idx += 1;
                    continue;
                }
                c if c.is_ascii_digit() => {
                    let mut buffer = String::new();
                    let start = idx;
                    let mut float_value = false;
                    while idx < chars.len() && (chars[idx].is_ascii_digit() || chars[idx] == '.') {
                        if chars[idx] == '.' {
                            float_value = true;
                        }
                        buffer.push(chars[idx]);
                        idx += 1;
                    }
                    idx -= 1;
                    if float_value {
                        Token::float(buffer.parse::<f32>().unwrap(), start, idx)
                    } else {
                        Token::int(buffer.parse::<i32>().unwrap(), start, idx)
                    }
                }
                c if c.is_alphabetic() || c == '_' => {
                    let mut buffer = String::new();
                    let start = idx;
                    while idx < chars.len() && (chars[idx].is_alphanumeric() || chars[idx] == '_') {
                        buffer.push(chars[idx]);
                        idx += 1;
                    }

                    idx -= 1;
                    let span = Span { start, end: idx };
                    match buffer.as_str() {
                        "object" => Token {
                            kind: TokenKind::Object,
                            span,
                        },
                        "component" => Token {
                            kind: TokenKind::Component,
                            span,
                        },
                        "func" => Token {
                            kind: TokenKind::Func,
                            span,
                        },
                        "pub" => Token {
                            kind: TokenKind::Pub,
                            span,
                        },
                        "prop" => Token {
                            kind: TokenKind::Prop,
                            span,
                        },
                        _ => Token::identifier(&buffer, start, idx),
                    }
                }
                c => {
                    return Err(LexerError::UnrecognizedChar {
                        char: c,
                        index: idx,
                    });
                }
            };
            out.push_back(tk);
            idx += 1;
        }
        Ok(TokenStream {
            stream: out,
            new_lines: lines,
        })
    }
}
