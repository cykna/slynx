use crate::parser::ast::Span;

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub enum TokenKind {
    LParen,
    RParen,
    LBrace,
    RBrace,
    SemiColon,
    Lt,
    LtEq,
    Gt,
    GtEq,
    Eq,
    EqEq,
    Plus,
    PlusEq,
    Sub,
    SubEq,
    Star,
    StarEq,
    Slash,
    SlashEq,
    Arrow,
    Comma,
    Colon,
    Float(f32),
    Int(i32),
    String(String),
    Identifier(String),
    MacroName(String),
    Component,
    Func,
    Pub,
    Prop,
    Object,
}

impl Token {
    pub fn lparen(pos: usize) -> Self {
        Self {
            kind: TokenKind::LParen,
            span: Span {
                end: pos,
                start: pos,
            },
        }
    }
    pub fn rparen(pos: usize) -> Self {
        Self {
            kind: TokenKind::RParen,
            span: Span {
                end: pos,
                start: pos,
            },
        }
    }
    pub fn lbrace(pos: usize) -> Self {
        Self {
            kind: TokenKind::LBrace,
            span: Span {
                end: pos,
                start: pos,
            },
        }
    }
    pub fn rbrace(pos: usize) -> Self {
        Self {
            kind: TokenKind::RBrace,
            span: Span {
                end: pos,
                start: pos,
            },
        }
    }
    pub fn semicolon(pos: usize) -> Self {
        Self {
            kind: TokenKind::SemiColon,
            span: Span {
                end: pos,
                start: pos,
            },
        }
    }
    pub fn lt(pos: usize) -> Self {
        Self {
            kind: TokenKind::Lt,
            span: Span {
                end: pos,
                start: pos,
            },
        }
    }
    pub fn gt(pos: usize) -> Self {
        Self {
            kind: TokenKind::Gt,
            span: Span {
                end: pos,
                start: pos,
            },
        }
    }
    pub fn eq(pos: usize) -> Self {
        Self {
            kind: TokenKind::Eq,
            span: Span {
                end: pos,
                start: pos,
            },
        }
    }
    pub fn colon(pos: usize) -> Self {
        Self {
            kind: TokenKind::Colon,
            span: Span {
                end: pos,
                start: pos,
            },
        }
    }
    pub fn comma(pos: usize) -> Self {
        Self {
            kind: TokenKind::Comma,
            span: Span {
                end: pos,
                start: pos,
            },
        }
    }
    pub fn plus(pos: usize) -> Self {
        Self {
            kind: TokenKind::Plus,
            span: Span {
                end: pos,
                start: pos,
            },
        }
    }
    pub fn sub(pos: usize) -> Self {
        Self {
            kind: TokenKind::Sub,
            span: Span {
                end: pos,
                start: pos,
            },
        }
    }
    pub fn star(pos: usize) -> Self {
        Self {
            kind: TokenKind::Star,
            span: Span {
                end: pos,
                start: pos,
            },
        }
    }
    pub fn slash(pos: usize) -> Self {
        Self {
            kind: TokenKind::Slash,
            span: Span {
                end: pos,
                start: pos,
            },
        }
    }
    pub fn lte(pos: usize) -> Self {
        Self {
            kind: TokenKind::LtEq,
            span: Span {
                end: pos,
                start: pos,
            },
        }
    }
    pub fn gte(pos: usize) -> Self {
        Self {
            kind: TokenKind::GtEq,
            span: Span {
                end: pos,
                start: pos,
            },
        }
    }
    pub fn pluseq(pos: usize) -> Self {
        Self {
            kind: TokenKind::PlusEq,
            span: Span {
                end: pos,
                start: pos,
            },
        }
    }
    pub fn subeq(pos: usize) -> Self {
        Self {
            kind: TokenKind::SubEq,
            span: Span {
                end: pos,
                start: pos,
            },
        }
    }
    pub fn stareq(pos: usize) -> Self {
        Self {
            kind: TokenKind::StarEq,
            span: Span {
                end: pos,
                start: pos,
            },
        }
    }
    pub fn slasheq(pos: usize) -> Self {
        Self {
            kind: TokenKind::SlashEq,
            span: Span {
                end: pos,
                start: pos,
            },
        }
    }
    pub fn eqeq(pos: usize) -> Self {
        Self {
            kind: TokenKind::EqEq,
            span: Span {
                end: pos,
                start: pos,
            },
        }
    }
    pub fn float(value: f32, start: usize, end: usize) -> Self {
        Self {
            kind: TokenKind::Float(value),
            span: Span { end, start },
        }
    }
    pub fn int(value: i32, start: usize, end: usize) -> Self {
        Self {
            kind: TokenKind::Int(value),
            span: Span { end, start },
        }
    }
    pub fn macro_name(buffer: &str, start: usize, end: usize) -> Self {
        Self {
            kind: TokenKind::MacroName(buffer.to_string()),
            span: Span { end, start },
        }
    }
    pub fn identifier(buffer: &str, start: usize, end: usize) -> Self {
        Self {
            kind: TokenKind::Identifier(buffer.to_string()),
            span: Span { end, start },
        }
    }
    pub fn component(start: usize, end: usize) -> Self {
        Self {
            kind: TokenKind::Component,
            span: Span { end, start },
        }
    }
    pub fn arrow(start: usize) -> Self {
        Self {
            kind: TokenKind::Arrow,
            span: Span {
                start,
                end: start + 1,
            },
        }
    }
    pub fn string(str: String, start: usize, end: usize) -> Self {
        Self {
            kind: TokenKind::String(str),
            span: Span { end, start },
        }
    }
}
