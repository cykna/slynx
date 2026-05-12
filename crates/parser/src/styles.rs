use common::Span;
use slynx_lexer::tokens::TokenKind;

use crate::{
    ASTDeclaration, ASTDeclarationKind, ASTExpression, Parser, StyleBlock, StyleSheetStatement,
    error::ParseError,
};

impl Parser {
    pub fn parse_style_block(&mut self) -> Result<StyleBlock, ParseError> {
        let block_start = self.peek()?.span;
        let event = match self.peek()?.kind {
            TokenKind::Identifier(_) => {
                let ident = self.expect_identifier()?;
                match ident.kind {
                    TokenKind::Identifier(name) => Some(name),
                    _ => unreachable!(),
                }
            }
            _ => None,
        };
        let duration = if let TokenKind::LParen = self.peek()?.kind {
            self.expect(&TokenKind::LParen)?;
            let expr = self.parse_expression()?;
            self.expect(&TokenKind::RParen)?;
            Some(expr)
        } else {
            None
        };
        self.expect(&TokenKind::LBrace)?;
        let mut properties = Vec::new();
        loop {
            if let TokenKind::RBrace = self.peek()?.kind {
                break;
            }
            let stmt = self.parse_named_expr()?;
            if let TokenKind::Comma | TokenKind::SemiColon = self.peek()?.kind {
                self.eat()?;
            }
            properties.push(stmt);
        }
        let block_end = self.expect(&TokenKind::RBrace)?.span;
        Ok(StyleBlock {
            event,
            duration,
            properties,
            span: block_start.merge_with(block_end),
        })
    }

    pub fn parse_styles_statement(&mut self) -> Result<StyleSheetStatement, ParseError> {
        let styles_span = self.expect(&TokenKind::Styles)?.span;
        self.expect(&TokenKind::LBrace)?;
        let mut styles = Vec::new();
        loop {
            if let TokenKind::RBrace = self.peek()?.kind {
                break;
            }
            styles.push(self.parse_style_block()?);
        }
        let end_span = self.expect(&TokenKind::RBrace)?.span;
        Ok(StyleSheetStatement::Styles {
            styles,
            span: styles_span.merge_with(end_span),
        })
    }

    pub fn parse_stylesheet_statement(&mut self) -> Result<StyleSheetStatement, ParseError> {
        match self.peek()?.kind {
            TokenKind::Styles => self.parse_styles_statement(),
            _ => self.parse_statement().map(StyleSheetStatement::Statement),
        }
    }

    pub fn parse_stylesheet_body(&mut self) -> Result<Vec<StyleSheetStatement>, ParseError> {
        let mut statements = Vec::new();
        loop {
            if let TokenKind::RBrace = self.peek()?.kind {
                break Ok(statements);
            }
            let stmt = self.parse_stylesheet_statement()?;
            if let TokenKind::Comma = self.peek()?.kind {
                self.eat()?;
            }
            statements.push(stmt);
        }
    }

    ///Parses the usages of the stylesheet. Thus, the `uses` until the `{` without consuming the `{`. The given `span` is the `uses` identifier span
    pub fn parse_usages(&mut self, _: Span) -> Result<Vec<ASTExpression>, ParseError> {
        let mut exprs = vec![];
        loop {
            if let TokenKind::LBrace = self.peek()?.kind {
                break {
                    if exprs.len() == 0 {
                        Err(ParseError::UnexpectedEndOfInput)
                    } else {
                        Ok(exprs)
                    }
                };
            }
            let usage = self.parse_funcall()?;
            if let TokenKind::Comma = self.peek()?.kind {
                self.eat()?;
                exprs.push(usage);
            }
        }
    }

    ///Parses a stylesheet. Thus the syntax `stylesheet Name(p1: T) uses Name2(f), F {...}`. The given `span` is the `stylesheet` keyword span
    pub fn parse_stylesheet(&mut self, span: Span) -> Result<ASTDeclaration, ParseError> {
        let name = self.parse_type()?;
        self.expect(&TokenKind::LParen)?;
        let args = {
            let mut out = Vec::new();
            loop {
                if let TokenKind::RParen = self.peek()?.kind {
                    break out;
                }
                let arg = self.parse_typedname()?;
                if let TokenKind::Comma = self.peek()?.kind {
                    self.eat()?;
                    out.push(arg);
                }
            }
        };

        self.expect(&TokenKind::RParen)?;
        let usages = if let TokenKind::Identifier(ref name) = self.peek()?.kind
            && name == "uses"
        {
            let span = self.eat()?.span;
            self.parse_usages(span)?
        } else {
            vec![]
        };
        self.expect(&TokenKind::LBrace)?;
        let body = self.parse_stylesheet_body()?;
        let out = ASTDeclaration {
            kind: ASTDeclarationKind::StyleSheet {
                name,
                args,
                usages,
                body,
            },
            span,
        };
        self.expect(&TokenKind::RBrace)?;
        Ok(out)
    }
}
