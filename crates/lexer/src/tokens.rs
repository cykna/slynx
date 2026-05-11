use common::ast::Span;
use logos::Logos;

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"[ \t\r\f]+")]
pub enum TokenKind {
    // Comments (skip)
    #[regex(r"//[^\n]*", logos::skip, allow_greedy = true)]
    #[regex(r"/\*[^*]*\*+(?:[^/*][^*]*\*+)*/", logos::skip)]
    CommonComent,

    // Keywords
    #[token("while")]
    While,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("component")]
    Component,
    #[token("func")]
    Func,
    #[token("pub")]
    Pub,
    #[token("prop")]
    Prop,
    #[token("alias")]
    Alias,
    #[token("object")]
    Object,
    #[token("let")]
    Let,
    #[token("mut")]
    Mut,
    #[token("true")]
    True,
    #[token("false")]
    False,

    // Multi-char operators (must come before single-char)
    #[token("&&")]
    And,
    #[token("||")]
    Or,
    #[token("<<")]
    ShiftLeft,
    #[token(">>")]
    ShiftRight,
    #[token("<=")]
    LtEq,
    #[token(">=")]
    GtEq,
    #[token("==")]
    EqEq,
    #[token("+=")]
    PlusEq,
    #[token("-=")]
    SubEq,
    #[token("*=")]
    StarEq,
    #[token("/=")]
    SlashEq,
    #[token("->")]
    Arrow,

    // Single-char operators
    #[token("&")]
    BitAnd,
    #[token("|")]
    BitOr,
    #[token("^")]
    Xor,
    #[token("~")]
    BitNot,
    #[token(".")]
    Dot,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token(";")]
    SemiColon,
    #[token("<")]
    Lt,
    #[token(">")]
    Gt,
    #[token("=")]
    Eq,
    #[token("+")]
    Plus,
    #[token("-")]
    Sub,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,

    // Literals
    #[regex(r"0x[0-9a-fA-F]+", |lex| i32::from_str_radix(&lex.slice()[2..], 16).ok())]
    #[regex(r"0b[01]+", |lex| i32::from_str_radix(&lex.slice()[2..], 2).ok())]
    #[regex(r"0o[0-7]+", |lex| i32::from_str_radix(&lex.slice()[2..], 8).ok())]
    #[regex(r"[0-9][0-9_]*", |lex| lex.slice().replace('_', "").parse::<i32>().ok())]
    Int(i32),

    // No underscore immediately before or after the decimal point
    #[regex(r"[0-9]([0-9]|_[0-9])*\.[0-9]([0-9]|_[0-9])*", |lex| lex.slice().replace('_', "").parse::<f32>().ok())]
    Float(f32),

    #[regex(r#""([^"\\]|\\.)*""#, |lex| {
        let s = lex.slice();
        // strip surrounding quotes and process escapes
        let inner = &s[1..s.len()-1];
        let mut out = String::with_capacity(inner.len());
        let mut chars = inner.chars();
        while let Some(c) = chars.next() {
            if c == '\\' {
                match chars.next() {
                    Some('n') => out.push('\n'),
                    Some('t') => out.push('\t'),
                    Some('r') => out.push('\r'),
                    Some('\\') => out.push('\\'),
                    Some('"') => out.push('"'),
                    Some(c) => { out.push('\\'); out.push(c); }
                    None => out.push('\\'),
                }
            } else {
                out.push(c);
            }
        }
        Some(out)
    })]
    String(String),

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Identifier(String),

    // Newline (tracked for line info)
    #[token("\n")]
    Newline,
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::While => "while",
            Self::CommonComent => "//",
            Self::And => "&&",
            Self::Or => "||",
            Self::If => "if",
            Self::Else => "else",
            Self::Dot => ".",
            Self::LParen => "(",
            Self::RParen => ")",
            Self::LBrace => "{",
            Self::RBrace => "}",
            Self::SemiColon => ";",
            Self::Lt => "<",
            Self::LtEq => "<=",
            Self::Gt => ">",
            Self::GtEq => ">=",
            Self::Eq => "=",
            Self::EqEq => "==",
            Self::Plus => "+",
            Self::PlusEq => "+=",
            Self::Sub => "-",
            Self::SubEq => "-=",
            Self::Star => "*",
            Self::StarEq => "*=",
            Self::Slash => "/",
            Self::SlashEq => "/=",
            Self::Arrow => "->",
            Self::Comma => ",",
            Self::Colon => ":",
            Self::Component => "component",
            Self::Func => "func",
            Self::Pub => "pub",
            Self::Prop => "prop",
            Self::Alias => "alias",
            Self::Object => "object",
            Self::Let => "let",
            Self::Mut => "mut",
            Self::True => "true",
            Self::False => "false",
            Self::ShiftRight => ">>",
            Self::ShiftLeft => "<<",
            Self::Xor => "^",
            Self::BitAnd => "&",
            Self::BitOr => "|",
            Self::BitNot => "~",
            Self::Newline => "\n",
            Self::Float(v) => return write!(f, "{v}"),
            Self::Int(v) => return write!(f, "{v}"),
            Self::String(v) => return write!(f, "{v}"),
            Self::Identifier(v) => return write!(f, "{v}"),
        };
        write!(f, "{s}")
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "'{}'", self.kind)
    }
}
