use crate::{
    hir::{
        SlynxHir,
        error::{HIRError, HIRErrorKind},
    },
    parser::ast::{
        ASTDeclaration, ASTDeclarationKind, ASTStatment, ASTStatmentKind, ElementDeffinition,
        ElementDeffinitionKind, Span,
    },
};

impl SlynxHir {
    ///Expands, or atleast, tries to, the given AST if it's a macro call declaration
    pub fn expand_decl(
        &mut self,
        idx: usize,
        ast: &ASTDeclaration,
    ) -> Result<Option<Vec<ASTDeclaration>>, HIRError> {
        match &ast.kind {
            ASTDeclarationKind::MacroCall(call_data) => {
                let macr = self
                    .decl_macros
                    .get::<&str>(&call_data.name.as_ref())
                    .ok_or(HIRError {
                        kind: HIRErrorKind::NameNotRecognized(call_data.name.clone()),
                        span: ast.span.clone(),
                    })?;
                Ok(Some(macr.execute(&call_data.args, idx)))
            }
            _ => Ok(None),
        }
    }
    ///Expands, or atleast, tries to, the given AST if it's a macro call statment
    pub fn expand_element(
        &mut self,
        idx: usize,
        ast: &ElementDeffinition,
    ) -> Result<Option<Vec<ElementDeffinition>>, HIRError> {
        match &ast.kind {
            ElementDeffinitionKind::MacroCall { name, args } => {
                let macr = self
                    .elmt_macros
                    .get::<&str>(&name.as_ref())
                    .ok_or(HIRError {
                        kind: HIRErrorKind::NameNotRecognized(name.clone()),
                        span: Span {
                            start: ast.span.start,
                            end: ast.span.start + name.len(),
                        },
                    })?;
                Ok(Some(macr.execute(&args, idx)))
            }
            _ => Ok(None),
        }
    }
    ///Expands, or atleast, tries to, the given AST if it's a macro call statment
    pub fn expand_stmt(
        &mut self,
        idx: usize,
        ast: &ASTStatment,
    ) -> Result<Option<Vec<ASTStatment>>, HIRError> {
        match &ast.kind {
            ASTStatmentKind::MacroCall(call_data) => {
                let macr = self
                    .stmt_macros
                    .get::<&str>(&call_data.name.as_ref())
                    .ok_or(HIRError {
                        kind: HIRErrorKind::NameNotRecognized(call_data.name.clone()),
                        span: ast.span.clone(),
                    })?;
                Ok(Some(macr.execute(&call_data.args, idx)))
            }
            _ => Ok(None),
        }
    }
}
