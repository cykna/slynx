use crate::{
    HIRError, Result, SlynxHir, TypeId,
    model::{
        HirStatement, HirStatementKind, HirStyleBlock, HirStyleBlockKind, HirStyleStatement,
        StylesDefinition,
    },
};
use common::Span;
use slynx_parser::{ASTStatement, ASTStatementKind, NamedExpr, StyleBlock, StyleSheetStatement};

impl SlynxHir {
    /// Resolves an AST statement into a typed [`HirStatement`].
    pub(crate) fn resolve_statement(&mut self, statement: &ASTStatement) -> Result<HirStatement> {
        match &statement.kind {
            ASTStatementKind::Expression(expr) => {
                let expr = self.generate_expression(expr, None)?;
                Ok(HirStatement::new_expression(expr))
            }
            ASTStatementKind::Assign { lhs, rhs } => {
                let lhs = self.generate_expression(lhs, None)?;
                let rhs = self.generate_expression(rhs, None)?;

                Ok(HirStatement {
                    kind: HirStatementKind::Assign { lhs, value: rhs },
                    span: statement.span,
                })
            }
            ASTStatementKind::MutableVar { name, ty, rhs } => {
                let typeid = ty.as_ref().and_then(|t| {
                    self.retrieve_information_of_type(&t.identifier, &statement.span)
                        .ok()
                        .map(|inner| inner.0)
                });
                let rhs = self.generate_expression(rhs, typeid)?;
                let name = self.modules.intern_name(name);
                let id = self.create_mutable_variable(name, rhs.ty, &statement.span)?;

                Ok(HirStatement::new_variable(id, rhs, statement.span))
            }
            ASTStatementKind::Var { name, ty, rhs } => {
                let typeid = ty.as_ref().and_then(|t| {
                    self.retrieve_information_of_type(&t.identifier, &statement.span)
                        .ok()
                        .map(|inner| inner.0)
                });
                let rhs = self.generate_expression(rhs, typeid)?;
                let name = self.modules.intern_name(name);
                let id = self.create_variable(name, rhs.ty, &statement.span)?;

                Ok(HirStatement::new_variable(id, rhs, statement.span))
            }

            ASTStatementKind::While { condition, body } => {
                let condition = self.generate_expression(condition, None)?;
                let body = body
                    .iter()
                    .map(|stmt| self.resolve_statement(stmt))
                    .collect::<Result<Vec<_>>>()?;
                Ok(HirStatement::new_while(condition, body, statement.span))
            }
        }
    }

    ///Transforms the given `statement` into an HIR style statement
    pub(crate) fn resolve_stylesheet_statement(
        &mut self,
        statement: &StyleSheetStatement,
    ) -> Result<HirStyleStatement> {
        match statement {
            StyleSheetStatement::Statement(s) => self
                .resolve_statement(s)
                .map(|s| HirStyleStatement::Statement(Box::new(s))),
            StyleSheetStatement::Styles { styles, .. } => self
                .resolve_stylesblock(styles)
                .map(HirStyleStatement::Styles),
        }
    }

    ///Type of styles is made by its name.
    pub(crate) fn resolve_style_type(&mut self, name: &str, span: Span) -> Result<TypeId> {
        let ty = match name {
            "backgroundColor" | "foregroundColor" => self.int32_type(),
            _ => {
                let name = self.modules.intern_name(name);
                return Err(HIRError::invalid_style_definition(name, span));
            }
        };
        Ok(ty)
    }

    ///Transforms the given `definitions` in a vector of `StyleDefinition`
    pub(crate) fn resolve_style_definitions(
        &mut self,
        definitions: &[NamedExpr],
    ) -> Result<Vec<StylesDefinition>> {
        definitions
            .iter()
            .map(|def| {
                let expr = self.generate_expression(&def.expr, None)?;
                let expected_type = self.resolve_style_type(&def.name, def.span)?;
                let symbol = self.modules.intern_name(&def.name);
                Ok(StylesDefinition::new(symbol, expr, expected_type, def.span))
            })
            .collect::<Result<Vec<_>>>()
    }

    ///Resolves the given `styles` blocks, and creates `HirStyleBlock`s based on them
    pub(crate) fn resolve_stylesblock(
        &mut self,
        styles: &[StyleBlock],
    ) -> Result<Vec<HirStyleBlock>> {
        let mut out = Vec::new();
        for style in styles {
            let kind = {
                let states = &style.state.states;
                if states.iter().any(|s| s == "hover") {
                    HirStyleBlockKind::Hover
                } else if states.is_empty() || states.iter().any(|s| s == "default") {
                    HirStyleBlockKind::Default
                } else {
                    let event = self.modules.intern_name(&states[0]);
                    return Err(HIRError::invalid_style_event(event, style.span));
                }
            };
            let definitions = self.resolve_style_definitions(&style.properties)?;
            out.push(HirStyleBlock { kind, definitions });
        }
        Ok(out)
    }
}
