use crate::hir::{
    SlynxHir, TypeId, VariableId,
    definitions::{
        ComponentMemberDeclaration, HirDeclaration, HirDeclarationKind, HirExpression,
        HirExpressionKind, HirStatement, HirStatementKind, SpecializedComponent,
    },
    types::{FieldMethod, HirType},
};
use common::ast::VisibilityModifier;

impl SlynxHir {
    pub fn render_text(&self) -> String {
        let mut out = String::from("hir {\n");
        for decl in &self.declarations {
            self.render_declaration(decl, 1, &mut out);
            out.push('\n');
        }
        out.push_str("}\n");
        out
    }

    fn render_declaration(&self, decl: &HirDeclaration, indent: usize, out: &mut String) {
        let padding = "  ".repeat(indent);
        let name = self
            .types_module
            .type_name(&decl.ty)
            .map(|sym| self.symbols_module[sym].to_string())
            .unwrap_or_else(|| format!("decl{}", decl.id.as_raw()));

        match &decl.kind {
            HirDeclarationKind::Object { name } => {
                out.push_str(&format!(
                    "{padding}object {} : {}\n",
                    name,
                    self.render_type_id(&decl.ty)
                ));
                out.push_str(&format!("{padding}{{\n"));
                if let Some(fields) = self.declarations_module.retrieve_object_body(decl.ty)
                    && let HirType::Reference { rf, .. } = self.types_module.get_type(&decl.ty)
                    && let HirType::Struct { fields: tys } = self.types_module.get_type(rf)
                {
                    for (idx, field_name) in fields.iter().enumerate() {
                        let field_ty = tys
                            .get(idx)
                            .map(|ty| self.render_type_id(ty))
                            .unwrap_or_else(|| "<missing>".to_string());
                        out.push_str(&format!(
                            "{}  {}: {}\n",
                            padding, &self.symbols_module[*field_name], field_ty
                        ));
                    }
                }
                out.push_str(&format!("{padding}}}\n"));
            }
            HirDeclarationKind::Function {
                args, statements, ..
            } => {
                let HirType::Function {
                    args: arg_tys,
                    return_type,
                } = self.types_module.get_type(&decl.ty)
                else {
                    unreachable!("function declaration should keep a function type");
                };
                let args_text = args
                    .iter()
                    .zip(arg_tys.iter())
                    .map(|(var, ty)| {
                        format!(
                            "{}: {}",
                            self.render_variable(*var),
                            self.render_type_id(ty)
                        )
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                out.push_str(&format!(
                    "{padding}func {}({}) -> {}\n",
                    name,
                    args_text,
                    self.render_type_id(return_type)
                ));
                out.push_str(&format!("{padding}{{\n"));
                for statement in statements {
                    self.render_statement(statement, indent + 1, out);
                }
                out.push_str(&format!("{padding}}}\n"));
            }
            HirDeclarationKind::ComponentDeclaration { name, props } => {
                out.push_str(&format!(
                    "{padding}component {} : {}\n",
                    name,
                    self.render_type_id(&decl.ty)
                ));
                out.push_str(&format!("{padding}{{\n"));
                for member in props {
                    self.render_component_member(member, &decl.ty, indent + 1, out);
                }
                out.push_str(&format!("{padding}}}\n"));
            }
        }
    }

    fn render_statement(&self, statement: &HirStatement, indent: usize, out: &mut String) {
        let padding = "  ".repeat(indent);
        match &statement.kind {
            HirStatementKind::Assign { lhs, value } => {
                out.push_str(&format!(
                    "{padding}assign {} = {};\n",
                    self.render_expression(lhs),
                    self.render_expression(value)
                ));
            }
            HirStatementKind::Variable { name, value } => {
                let value_ty = self
                    .types_module
                    .get_variable(name)
                    .map(|ty| self.render_type_id(ty))
                    .unwrap_or_else(|| "<unknown>".to_string());
                out.push_str(&format!(
                    "{padding}let {}: {} = {};\n",
                    self.render_variable(*name),
                    value_ty,
                    self.render_expression(value)
                ));
            }
            HirStatementKind::Expression { expr } => {
                out.push_str(&format!(
                    "{padding}expr {};\n",
                    self.render_expression(expr)
                ));
            }
            HirStatementKind::Return { expr } => {
                out.push_str(&format!("{padding}ret {};\n", self.render_expression(expr)));
            }
        }
    }

    fn render_component_member(
        &self,
        member: &ComponentMemberDeclaration,
        component_ty: &TypeId,
        indent: usize,
        out: &mut String,
    ) {
        let padding = "  ".repeat(indent);
        match member {
            ComponentMemberDeclaration::Property {
                id: _,
                index,
                value,
                ..
            } => {
                let (modifier, name, ty) = self
                    .component_prop_metadata(component_ty, *index)
                    .unwrap_or((
                        VisibilityModifier::Private,
                        format!("prop{index}"),
                        self.types_module.infer_id(),
                    ));
                let modifier = self.render_visibility(&modifier);
                match value {
                    Some(value) => out.push_str(&format!(
                        "{padding}prop {}{}: {} = {};\n",
                        modifier,
                        name,
                        self.render_type_id(&ty),
                        self.render_expression(value)
                    )),
                    None => out.push_str(&format!(
                        "{padding}prop {}{}: {};\n",
                        modifier,
                        name,
                        self.render_type_id(&ty)
                    )),
                }
            }
            ComponentMemberDeclaration::Child { name, values, .. } => {
                out.push_str(&format!(
                    "{padding}child {} {{\n",
                    self.render_type_id(name)
                ));
                for value in values {
                    self.render_component_member(value, name, indent + 1, out);
                }
                out.push_str(&format!("{padding}}}\n"));
            }
            ComponentMemberDeclaration::Specialized(spec) => {
                out.push_str(&format!(
                    "{padding}specialized {};\n",
                    self.render_specialized(spec)
                ));
            }
        }
    }

    fn component_prop_metadata(
        &self,
        component_ty: &TypeId,
        index: usize,
    ) -> Option<(VisibilityModifier, String, TypeId)> {
        let HirType::Component { props } = self.types_module.get_type(component_ty) else {
            return None;
        };
        props
            .get(index)
            .map(|(modifier, name, ty)| (modifier.clone(), name.clone(), *ty))
    }

    fn render_expression(&self, expr: &HirExpression) -> String {
        let head = match &expr.kind {
            HirExpressionKind::Int(value) => value.to_string(),
            HirExpressionKind::StringLiteral(value) => format!("{value:?}"),
            HirExpressionKind::Float(value) => value.to_string(),
            HirExpressionKind::Bool(value) => value.to_string(),
            HirExpressionKind::Binary { lhs, op, rhs } => format!(
                "({} {:?} {})",
                self.render_expression(lhs),
                op,
                self.render_expression(rhs)
            ),
            HirExpressionKind::Identifier(id) => self.render_variable(*id),
            HirExpressionKind::Specialized(spec) => self.render_specialized(spec),
            HirExpressionKind::Component { name, values } => {
                let values = values
                    .iter()
                    .map(|value| self.render_inline_component_member(value, name))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{} {{ {} }}", self.render_type_id(name), values)
            }
            HirExpressionKind::Object { name, fields } => {
                let values = fields
                    .iter()
                    .map(|field| self.render_expression(field))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}({values})", self.render_type_id(name))
            }
            HirExpressionKind::FieldAccess { expr, field_index } => {
                format!("{}.f{}", self.render_expression(expr), field_index)
            }
            HirExpressionKind::FunctionCall { name, args } => {
                let args = args
                    .iter()
                    .map(|arg| self.render_expression(arg))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}({args})", self.render_declaration_name(*name))
            }
        };
        format!("{head}: {}", self.render_type_id(&expr.ty))
    }

    fn render_inline_component_member(
        &self,
        member: &ComponentMemberDeclaration,
        component_ty: &TypeId,
    ) -> String {
        match member {
            ComponentMemberDeclaration::Property { index, value, .. } => {
                let name = self
                    .component_prop_metadata(component_ty, *index)
                    .map(|(_, name, _)| name)
                    .unwrap_or_else(|| format!("prop{index}"));
                match value {
                    Some(value) => format!("{name}: {}", self.render_expression(value)),
                    None => name,
                }
            }
            ComponentMemberDeclaration::Child { name, values, .. } => {
                let values = values
                    .iter()
                    .map(|value| self.render_inline_component_member(value, name))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{} {{ {values} }}", self.render_type_id(name))
            }
            ComponentMemberDeclaration::Specialized(spec) => self.render_specialized(spec),
        }
    }

    fn render_specialized(&self, spec: &SpecializedComponent) -> String {
        match spec {
            SpecializedComponent::Text { text } => {
                format!("Text {{ text: {} }}", self.render_expression(text))
            }
            SpecializedComponent::Div { children } => {
                let values = children
                    .iter()
                    .map(|child| {
                        self.render_inline_component_member(
                            child,
                            &self.types_module.generic_component_id(),
                        )
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("Div {{ {values} }}")
            }
        }
    }

    fn render_variable(&self, id: VariableId) -> String {
        self.variable_names
            .get(&id)
            .map(|symbol| self.symbols_module[*symbol].to_string())
            .unwrap_or_else(|| format!("var{}", self.variable_ordinal(id)))
    }

    fn render_declaration_name(&self, id: crate::hir::DeclarationId) -> String {
        let ty = self.declarations_module.retrieve_declaration_type(id);
        self.types_module
            .type_name(&ty)
            .map(|symbol| self.symbols_module[symbol].to_string())
            .unwrap_or_else(|| format!("decl{}", id.as_raw()))
    }

    fn render_type_id(&self, id: &TypeId) -> String {
        if let Some(symbol) = self.types_module.type_name(id) {
            return self.symbols_module[symbol].to_string();
        }

        match self.types_module.get_type(id) {
            HirType::Struct { fields } => format!(
                "struct{{{}}}",
                fields
                    .iter()
                    .map(|field| self.render_type_id(field))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            HirType::Vector { ty } => format!("vec<{}>", self.render_type_id(ty)),
            HirType::Reference { rf, generics } => {
                if generics.is_empty() {
                    format!("ref {}", self.render_type_id(rf))
                } else {
                    format!(
                        "ref {}<{}>",
                        self.render_type_id(rf),
                        generics
                            .iter()
                            .map(|generic| self.render_type_id(generic))
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                }
            }
            HirType::VarReference(id) => format!("typeof {}", self.render_variable(*id)),
            HirType::Field(FieldMethod::Type(parent, idx)) => {
                format!("field {}.{}", self.render_type_id(parent), idx)
            }
            HirType::Field(FieldMethod::Variable(id, field)) => {
                format!(
                    "field {}.{}",
                    self.render_variable(*id),
                    &self.symbols_module[*field]
                )
            }
            HirType::Function { args, return_type } => format!(
                "fn({}) -> {}",
                args.iter()
                    .map(|arg| self.render_type_id(arg))
                    .collect::<Vec<_>>()
                    .join(", "),
                self.render_type_id(return_type)
            ),
            HirType::Bool => "bool".to_string(),
            HirType::Float => "float".to_string(),
            HirType::Int => "int".to_string(),
            HirType::Str => "str".to_string(),
            HirType::GenericComponent => "Component".to_string(),
            HirType::Component { props } => format!(
                "component{{{}}}",
                props
                    .iter()
                    .map(|(modifier, name, ty)| {
                        format!(
                            "{}{}: {}",
                            self.render_visibility(modifier),
                            name,
                            self.render_type_id(ty)
                        )
                    })
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            HirType::Void => "void".to_string(),
            HirType::Infer => "infer".to_string(),
        }
    }

    fn render_visibility(&self, modifier: &VisibilityModifier) -> &'static str {
        match modifier {
            VisibilityModifier::Public => "pub ",
            VisibilityModifier::Private => "",
            VisibilityModifier::ChildrenPublic => "pub(child) ",
            VisibilityModifier::ParentPublic => "pub(parent) ",
        }
    }

    fn variable_ordinal(&self, id: VariableId) -> usize {
        let mut ids = self.variable_names.keys().copied().collect::<Vec<_>>();
        ids.sort_by_key(|candidate| candidate.as_raw());
        ids.iter()
            .position(|candidate| *candidate == id)
            .unwrap_or_default()
    }
}
