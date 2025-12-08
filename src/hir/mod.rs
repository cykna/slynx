pub mod declaration;
pub mod error;
pub mod macros;
mod scope;
pub mod types;
use std::{
    collections::HashMap,
    mem::discriminant,
    sync::{Arc, atomic::AtomicU64},
};

use crate::{
    hir::{
        declaration::{
            ElementValueDeclaration, HirDeclaration, HirDeclarationKind, HirExpression,
            HirExpressionKind, HirStatment, HirStatmentKind,
        },
        error::{HIRError, HIRErrorKind},
        macros::{DeclarationMacro, ElementMacro, StatmentMacro},
        scope::HIRScope,
        types::{HirType, HirValue, HirValueKind},
    },
    parser::ast::Operator,
    parser::ast::{
        ASTDeclaration, ASTDeclarationKind, ASTExpression, ASTExpressionKind, ASTStatment,
        ASTStatmentKind, ElementDeffinition, ElementDeffinitionKind, ElementValue,
        GenericIdentifier, PropertyModifier, Span,
    },
};

static ACCUMULATOR: AtomicU64 = AtomicU64::new(0);
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
///An ID for name resolution on the HIR
pub struct HirId(pub u64);
impl HirId {
    pub fn new() -> Self {
        Self(ACCUMULATOR.fetch_add(1, std::sync::atomic::Ordering::Relaxed))
    }
}

#[derive(Debug)]
pub struct SlynxHir {
    ///Maps a name N to it's ID on the HIR. This is for something like function declaration and function call.
    names: HashMap<String, HirId>,
    ///Maps the types of top level things on the current scope to their types.
    ///An example is functions, which contain an HirType.
    types: HashMap<HirId, HirType>,
    ///The scopes of this HIR. On the final it's expected to have only one, which is the global one
    scopes: Vec<HIRScope>,
    decl_macros: HashMap<&'static str, Arc<dyn DeclarationMacro>>,
    stmt_macros: HashMap<&'static str, Arc<dyn StatmentMacro>>,
    elmt_macros: HashMap<&'static str, Arc<dyn ElementMacro>>,
    pub declarations: Vec<HirDeclaration>,
}

impl SlynxHir {
    pub fn new() -> Self {
        let out = Self {
            scopes: vec![HIRScope::new()],
            names: HashMap::new(),
            types: HashMap::new(),
            declarations: Vec::new(),
            decl_macros: HashMap::new(),
            stmt_macros: HashMap::new(),
            elmt_macros: HashMap::new(),
        };
        out
    }

    ///Expands all the macro call declarations inside the given `ast`
    fn expand_decls(&mut self, ast: &mut Vec<ASTDeclaration>) -> Result<(), HIRError> {
        let mut idx = 0;
        while idx < ast.len() {
            if let Some(news) = self.expand_decl(idx, &ast[idx])? {
                ast.remove(idx);
                for (nidx, newast) in news.into_iter().enumerate() {
                    ast.insert(idx + nidx, newast);
                }
            }

            idx += 1;
        }
        Ok(())
    }
    ///Expands the element macros inside the given `ast`. Since it's a Declaration array, this will look into
    ///the macro inside the declarations, if some is a macro call, then expands it
    fn expand_elements(&mut self, ast: &mut Vec<ASTDeclaration>) -> Result<(), HIRError> {
        for ast in ast {
            match &mut ast.kind {
                ASTDeclarationKind::ElementDeclaration { deffinitions, .. } => {
                    let mut idx = 0;
                    while idx < deffinitions.len() {
                        if let Some(news) = self.expand_element(idx, &deffinitions[idx])? {
                            deffinitions.remove(idx);

                            for (nidx, newast) in news.into_iter().enumerate() {
                                deffinitions.insert(idx + nidx, newast);
                            }
                        }
                        idx += 1;
                    }
                }
                _ => {}
            }
        }
        Ok(())
    }

    ///Expands the statment macros inside the given `ast`. Since it's a Declaration array, this will look into
    ///the statments inside the declarations, if some is a macro call, then expands it
    fn expand_stmts(&mut self, ast: &mut Vec<ASTDeclaration>) -> Result<(), HIRError> {
        for ast in ast {
            match &mut ast.kind {
                ASTDeclarationKind::MacroCall(..)
                | ASTDeclarationKind::ElementDeclaration { .. } => {}
                ASTDeclarationKind::FuncDeclaration { body, .. } => {
                    let mut idx = 0;
                    while idx < body.len() {
                        if let Some(news) = self.expand_stmt(idx, &body[idx])? {
                            body.remove(idx);
                            for (nidx, newast) in news.into_iter().enumerate() {
                                body.insert(idx + nidx, newast);
                            }
                        }
                        idx += 1;
                    }
                }
            }
        }
        Ok(())
    }

    ///Generates the declarations from the provided `ast`
    pub fn generate(&mut self, mut ast: Vec<ASTDeclaration>) -> Result<(), HIRError> {
        self.expand_decls(&mut ast)?;
        self.expand_elements(&mut ast)?;
        self.expand_stmts(&mut ast)?;

        for ast in &ast {
            self.hoist(ast)?;
        }
        for ast in ast {
            self.resolve(ast)?;
        }
        Ok(())
    }

    #[inline]
    ///Inserts a new Macro that can be executed on declaration toplevel.
    pub fn insert_element_macro(&mut self, macr: Arc<dyn ElementMacro>) {
        self.elmt_macros.insert(macr.name(), macr);
    }
    #[inline]
    ///Inserts a new Macro that can be executed on declaration toplevel.
    pub fn insert_declaration_macro(&mut self, macr: Arc<dyn DeclarationMacro>) {
        self.decl_macros.insert(macr.name(), macr);
    }
    #[inline]
    ///Inserts a new macro that can be executed on statment toplevel.
    pub fn insert_statment_macro(&mut self, macr: Arc<dyn StatmentMacro>) {
        self.stmt_macros.insert(macr.name(), macr);
    }
    ///Retrieves information about the provided `name` going from the current scope to the outer ones, finishing on the global
    pub fn retrieve_information_of_scoped(
        &mut self,
        name: &str,
        span: &Span,
    ) -> Result<(HirId, &HirValue), HIRError> {
        let mut idx = self.scopes.len() - 1;

        while idx != 0 {
            let scope = &self.scopes[idx];

            let Ok(id) = scope.retrieve_name(name, span) else {
                idx -= 1;
                continue;
            };
            let Ok(info) = scope.retrieve_value(*id, name, span) else {
                idx -= 1;
                continue;
            };
            return Ok((*id, info));
        }
        Err(HIRError {
            kind: HIRErrorKind::NameNotRecognized(name.to_string()),
            span: span.clone(),
        })
    }

    ///Tries to retrieve the type and HirId of the provided `name` in the global scope
    pub fn retrieve_information_of(
        &mut self,
        name: &str,
        span: &Span,
    ) -> Result<(HirId, HirType), HIRError> {
        if let Some(name_id) = self.names.get(name)
            && let Some(ty) = self.types.get(name_id)
        {
            Ok((name_id.clone(), ty.clone()))
        } else {
            Err(HIRError {
                kind: HIRErrorKind::NameNotRecognized(name.to_string()),
                span: span.clone(),
            })
        }
    }

    ///Retrieves the hir id of the provided `name` in the global scope
    pub fn retrieve_hirdid_of(&mut self, name: &str, span: &Span) -> Result<HirId, HIRError> {
        self.names.get(name).cloned().ok_or(HIRError {
            kind: HIRErrorKind::NameNotRecognized(name.to_string()),
            span: span.clone(),
        })
    }
    ///Retrieves the type of the provided `name` but in the global scope. The difference of a 'named' to a 'name' is that this function
    ///tries to the the provided `name` as some identifier to something, and the name version does so after checking if the provided name itself
    ///is a type
    pub fn retrieve_type_of_named(&mut self, name: &str, span: &Span) -> Result<HirType, HIRError> {
        if let Some(name_id) = self.names.get(name)
            && let Some(ty) = self.types.get(name_id)
        {
            Ok(ty.clone())
        } else {
            Err(HIRError {
                kind: HIRErrorKind::NameNotRecognized(name.to_string()),
                span: span.clone(),
            })
        }
    }
    ///Retrieves the type of the provided `name` but in the global scope
    pub fn retrieve_type_of_name(
        &mut self,
        name: &GenericIdentifier,
        span: &Span,
    ) -> Result<HirType, HIRError> {
        match HirType::new(name) {
            Ok(value) => Ok(value),
            Err(_) => {
                if let Some(name_id) = self.names.get(&name.identifier)
                    && let Some(ty) = self.types.get(name_id)
                {
                    Ok(ty.clone())
                } else {
                    Err(HIRError {
                        kind: HIRErrorKind::NameNotRecognized(name.to_string()),
                        span: span.clone(),
                    })
                }
            }
        }
    }

    fn enter_scope(&mut self) {
        self.scopes.push(HIRScope::new());
    }
    fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    // fn scope_at(&mut self, idx: usize) -> &mut HIRScope {
    //     &mut self.scopes[idx]
    // }

    fn last_scope(&mut self) -> &mut HIRScope {
        let idx = self.scopes.len() - 1;
        &mut self.scopes[idx]
    }

    ///Creates an hir id for the provided `value` and `name` on the current scope
    fn create_hirid_for(
        &mut self,
        name: String,
        value: HirValue,
        ty: HirType,
        span: &Span,
    ) -> Result<HirId, HIRError> {
        let id = HirId::new();
        self.names.insert(name.clone(), id);
        self.last_scope()
            .insert_named_value(id, name, value, span)?;
        self.types.insert(id, ty);
        Ok(id)
    }

    pub fn resolve_binary(
        &mut self,
        lhs: ASTExpression,
        op: Operator,
        rhs: ASTExpression,
        ty: Option<&HirType>,
    ) -> Result<HirExpression, HIRError> {
        let mut lhs = self.resolve_expr(lhs, ty)?;
        let mut rhs = self.resolve_expr(rhs, ty)?;
        if discriminant(&lhs.ty) != discriminant(&rhs.ty) {
            if matches!(lhs.ty, HirType::Infer) {
                lhs.ty = rhs.ty.clone();
            } else if matches!(rhs.ty, HirType::Infer) {
                rhs.ty = lhs.ty.clone();
            }
        }
        let span = Span {
            start: lhs.span.start,
            end: lhs.span.end,
        };
        Ok(HirExpression {
            ty: lhs.ty.clone(),
            kind: HirExpressionKind::Binary {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
            },
            id: HirId::new(),
            span,
        })
    }
    ///Resolves the provided values on a element expression. The `ty`is the type of the component we are resolving it
    fn resolve_element_values(
        &mut self,
        values: Vec<ElementValue>,
        ty: &HirType,
    ) -> Result<Vec<ElementValueDeclaration>, HIRError> {
        let mut out = Vec::with_capacity(values.len());
        let HirType::Component { props } = ty else {
            unreachable!("The type should be a component instead");
        };

        let accepting_children = props
            .iter()
            .find(|prop| {
                prop.1 == "children"
                    && matches!(
                        prop.0,
                        PropertyModifier::ParentPublic | PropertyModifier::Public
                    )
            })
            .is_some();
        for value in values {
            out.push(match value {
                ElementValue::Assign {
                    prop_name,
                    rhs,
                    span,
                } => {
                    //later change to 'NotRecognizedProperty {name, component_name}'
                    let index =
                        props
                            .iter()
                            .position(|prop| prop.1 == prop_name)
                            .ok_or(HIRError {
                                kind: HIRErrorKind::NameNotRecognized(prop_name.clone()),
                                span: span.clone(),
                            })?;

                    if matches!(
                        props[index].0,
                        PropertyModifier::Private | PropertyModifier::ChildrenPublic
                    ) {
                        return Err(HIRError {
                            kind: HIRErrorKind::PropertyNotVisible { prop_name },
                            span,
                        });
                    }

                    ElementValueDeclaration::Property {
                        id: HirId::new(),
                        index,
                        value: Some(self.resolve_expr(rhs, Some(&props[index].2))?),
                        span,
                    }
                }
                ElementValue::Element(element) => {
                    if accepting_children {
                        let (name, ty) = self.retrieve_information_of(
                            &element.name.identifier,
                            &element.name.span,
                        )?;
                        ElementValueDeclaration::Child {
                            name,
                            values: self.resolve_element_values(element.values, &ty)?,
                            span: element.span,
                        }
                    } else {
                        return Err(HIRError {
                            span: element.span.clone(),
                            kind: HIRErrorKind::InvalidChild { child: element },
                        });
                    }
                }
            });
        }
        Ok(out)
    }
    ///Ty only serves to tell the type of the expression if it's needed to infer and check if it doesnt correspond
    fn resolve_expr(
        &mut self,
        expr: ASTExpression,
        ty: Option<&HirType>,
    ) -> Result<HirExpression, HIRError> {
        match expr.kind {
            ASTExpressionKind::Binary { lhs, op, rhs } => self.resolve_binary(*lhs, op, *rhs, ty),
            ASTExpressionKind::StringLiteral(s) => Ok(HirExpression {
                id: HirId::new(),
                ty: HirType::Str,
                kind: HirExpressionKind::StringLiteral(s),
                span: expr.span,
            }),
            ASTExpressionKind::Identifier(name) => {
                let (id, _) = self.retrieve_information_of_scoped(&name, &expr.span)?;
                Ok(HirExpression {
                    kind: HirExpressionKind::Identifier(id),
                    id: HirId::new(),
                    ty: HirType::Reference {
                        rf: id,
                        generics: Vec::new(),
                    },
                    span: expr.span,
                })
            }
            ASTExpressionKind::IntLiteral(int) => Ok(HirExpression {
                kind: HirExpressionKind::Int(int),
                ty: HirType::Int,
                id: HirId::new(),
                span: expr.span,
            }),
            ASTExpressionKind::Int16x2Literal(a, b) => {
                let a = self.resolve_expr(*a, Some(&HirType::Int))?;
                let b = self.resolve_expr(*b, Some(&HirType::Int))?;
                Ok(HirExpression {
                    id: HirId::new(),
                    ty: HirType::Int16x2,
                    kind: HirExpressionKind::Int16x2(Box::new(a), Box::new(b)),
                    span: expr.span,
                })
            }
            ASTExpressionKind::Uint16x2Literal(a, b) => {
                let a = self.resolve_expr(*a, Some(&HirType::Int))?;
                let b = self.resolve_expr(*b, Some(&HirType::Int))?;
                Ok(HirExpression {
                    id: HirId::new(),
                    ty: HirType::Uint16x2,
                    kind: HirExpressionKind::Uint16x2(Box::new(a), Box::new(b)),
                    span: expr.span,
                })
            }
            ASTExpressionKind::Int8x4Literal(a, b, c, d) => {
                let a = self.resolve_expr(*a, Some(&HirType::Int))?;
                let b = self.resolve_expr(*b, Some(&HirType::Int))?;
                let c = self.resolve_expr(*c, Some(&HirType::Int))?;
                let d = self.resolve_expr(*d, Some(&HirType::Int))?;
                Ok(HirExpression {
                    id: HirId::new(),
                    ty: HirType::Int8x4,
                    kind: HirExpressionKind::Int8x4(
                        Box::new(a),
                        Box::new(b),
                        Box::new(c),
                        Box::new(d),
                    ),
                    span: expr.span,
                })
            }
            ASTExpressionKind::Uint8x4Literal(a, b, c, d) => {
                let a = self.resolve_expr(*a, Some(&HirType::Int))?;
                let b = self.resolve_expr(*b, Some(&HirType::Int))?;
                let c = self.resolve_expr(*c, Some(&HirType::Int))?;
                let d = self.resolve_expr(*d, Some(&HirType::Int))?;
                Ok(HirExpression {
                    id: HirId::new(),
                    ty: HirType::Uint8x4,
                    kind: HirExpressionKind::Uint8x4(
                        Box::new(a),
                        Box::new(b),
                        Box::new(c),
                        Box::new(d),
                    ),
                    span: expr.span,
                })
            }
            ASTExpressionKind::FloatLiteral(float) => Ok(HirExpression::float(float, expr.span)),
            ASTExpressionKind::Element(element) => {
                let (id, ty) =
                    self.retrieve_information_of(&element.name.identifier, &element.span)?;

                Ok(HirExpression {
                    kind: HirExpressionKind::Element {
                        name: id,
                        values: self.resolve_element_values(element.values, &ty)?,
                    },
                    id: HirId::new(),
                    ty,
                    span: expr.span,
                })
            }
        }
    }

    fn resolve_statment(&mut self, statment: ASTStatment) -> Result<HirStatment, HIRError> {
        match statment.kind {
            ASTStatmentKind::Expression(expr) => {
                let expr = self.resolve_expr(expr, None)?;
                Ok(HirStatment {
                    span: expr.span.clone(),
                    kind: HirStatmentKind::Expression { expr },
                })
            }
            _ => {
                unimplemented!("{:?}", statment)
            }
        }
    }

    fn resolve_component_defs(
        &mut self,
        def: Vec<ElementDeffinition>,
    ) -> Result<Vec<ElementValueDeclaration>, HIRError> {
        let mut out = Vec::with_capacity(def.len());

        let mut prop_idx = 0;
        for def in def {
            match def.kind {
                ElementDeffinitionKind::RawJs(js) => out.push(ElementValueDeclaration::Js(js)),
                ElementDeffinitionKind::Property {
                    modifier,
                    ty,
                    rhs,
                    name,
                } => {
                    let ty = if let Some(ty) = ty {
                        let ty = self.retrieve_type_of_name(&ty, &ty.span)?;
                        ty
                    } else {
                        HirType::Infer
                    };
                    let id = HirId::new();

                    out.push(ElementValueDeclaration::Property {
                        id,
                        index: prop_idx,
                        value: if let Some(rhs) = rhs {
                            Some(self.resolve_expr(rhs, Some(&ty))?)
                        } else {
                            None
                        },
                        span: def.span.clone(),
                    });

                    self.last_scope().insert_named_value(
                        id,
                        name,
                        HirValue {
                            ty,
                            kind: HirValueKind::Property { modifier },
                        },
                        &def.span,
                    )?;
                    prop_idx += 1;
                }
                ElementDeffinitionKind::Child(child) => {
                    let (id, ty) =
                        self.retrieve_information_of(&child.name.identifier, &child.span)?;
                    let values = self.resolve_element_values(child.values, &ty)?;
                    out.push(ElementValueDeclaration::Child {
                        name: id,
                        values,
                        span: child.span,
                    })
                }
                _ => {}
            }
        }
        Ok(out)
    }

    ///Hoist the provided `ast` declaration, with so no errors of undefined values because declared later may occurr
    fn hoist(&mut self, ast: &ASTDeclaration) -> Result<(), HIRError> {
        match &ast.kind {
            ASTDeclarationKind::MacroCall(..) => {}
            ASTDeclarationKind::FuncDeclaration {
                name,
                args,
                return_type,
                ..
            } => {
                let args = {
                    let mut vec = Vec::with_capacity(args.len());
                    for arg in args {
                        let ty = self.retrieve_type_of_name(&arg.kind, &arg.span)?;
                        vec.push(ty);
                    }
                    vec
                };
                let func_ty = HirType::Function {
                    args,
                    return_type: Box::new(
                        self.retrieve_type_of_name(&return_type, &return_type.span)?,
                    ),
                };

                self.create_hirid_for(
                    name.to_string(), //add suport for generic identifier
                    HirValue {
                        kind: HirValueKind::Function {
                            modifier: PropertyModifier::Private,
                        },
                        ty: func_ty.clone(),
                    },
                    func_ty,
                    &ast.span,
                )?;
            }
            ASTDeclarationKind::ElementDeclaration { name, deffinitions } => {
                let props = {
                    let mut out = Vec::with_capacity(deffinitions.len());
                    for def in deffinitions {
                        match &def.kind {
                            ElementDeffinitionKind::Property {
                                name, modifier, ty, ..
                            } => {
                                out.push((
                                    modifier.clone(),
                                    name.clone(),
                                    if let Some(generic) = ty {
                                        self.retrieve_type_of_name(&generic, &def.span)?
                                    } else {
                                        HirType::Infer
                                    },
                                ));
                            }
                            ElementDeffinitionKind::Child(_) => {}
                            _ => {}
                        }
                    }
                    out
                };
                self.create_hirid_for(
                    name.to_string(), //add support for generic identifier
                    HirValue {
                        kind: HirValueKind::Component {
                            modifier: PropertyModifier::Private,
                        },
                        ty: HirType::GenericComponent,
                    },
                    HirType::Component { props },
                    &ast.span,
                )?;
            }
        }
        Ok(())
    }
    fn resolve(&mut self, ast: ASTDeclaration) -> Result<(), HIRError> {
        match ast.kind {
            ASTDeclarationKind::MacroCall(..) => {}
            ASTDeclarationKind::FuncDeclaration {
                name,
                args,
                mut body,
                ..
            } => {
                let (id, func) = self.retrieve_information_of(&name.to_string(), &ast.span)?; //modify later to accept the generic identifier instead

                self.enter_scope();
                for arg in args {
                    let id = HirId::new();
                    let ty = self.retrieve_type_of_name(&arg.kind, &arg.kind.span)?;
                    self.last_scope().insert_named_value(
                        id,
                        arg.name,
                        HirValue {
                            kind: HirValueKind::Variable,
                            ty,
                        },
                        &arg.span,
                    )?;
                }
                let statments = if let Some(last) = body.pop() {
                    let mut statments = Vec::with_capacity(body.len());
                    for ast in body {
                        statments.push(self.resolve_statment(ast)?);
                    }
                    if let ASTStatmentKind::Expression(expr) = last.kind {
                        let expr = self.resolve_expr(expr, None)?;
                        statments.push(HirStatment {
                            span: expr.span.clone(),
                            kind: HirStatmentKind::Return { expr },
                        });
                    }
                    statments
                } else {
                    Vec::new()
                };

                self.declarations.push(HirDeclaration {
                    kind: HirDeclarationKind::Function {
                        statments,
                        name: name.to_string(),
                    },
                    id,
                    ty: func,
                    span: ast.span,
                });
                self.exit_scope();
            }
            ASTDeclarationKind::ElementDeclaration { deffinitions, name } => {
                self.enter_scope();

                let (hir, ty) = self.retrieve_information_of(&name.to_string(), &ast.span)?; //modify later to accept the generic identifier instead

                let defs = self.resolve_component_defs(deffinitions)?;
                self.declarations.push(HirDeclaration {
                    id: hir,
                    kind: HirDeclarationKind::ElementDeclaration { props: defs },
                    ty,
                    span: ast.span,
                });
                self.exit_scope();
            }
        }
        Ok(())
    }
    ///Expands, or atleast, tries to, the given AST if it's a macro call declaration
    fn expand_decl(
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
    fn expand_element(
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
    fn expand_stmt(
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
