pub mod declaration;
pub mod error;
mod scope;
pub mod types;
use std::{collections::HashMap, mem::discriminant, sync::atomic::AtomicU64};

use crate::{
    ast::{
        ASTDeclaration, ASTDeclarationKind, ASTExpression, ASTExpressionKind, ASTStatment,
        ASTStatmentKind, ElementDeffinition, ElementDeffinitionKind, ElementValue,
        GenericIdentifier, PropertyModifier, Span,
    },
    hir::{
        declaration::{
            ElementValueDeclaration, HirDeclaration, HirDeclarationKind, HirExpression,
            HirExpressionKind, HirStatment, HirStatmentKind,
        },
        error::{HIRError, HIRErrorKind},
        scope::HIRScope,
        types::{HirType, HirValue, HirValueKind},
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
    pub declarations: Vec<HirDeclaration>,
}

impl SlynxHir {
    pub fn new(ast: Vec<ASTDeclaration>) -> Result<Self, HIRError> {
        let mut out = Self {
            scopes: vec![HIRScope::new()],
            names: HashMap::new(),
            types: HashMap::new(),
            declarations: Vec::new(),
        };

        for ast in &ast {
            out.hoist(ast)?;
        }
        for ast in ast {
            out.resolve(ast)?;
        }
        Ok(out)
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
            let Ok(info) = scope.retrieve_value(*id, span) else {
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
    fn create_hirid_for(&mut self, name: String, value: HirValue, ty: HirType) -> HirId {
        let id = HirId::new();
        self.names.insert(name.clone(), id);
        self.last_scope().insert_named_value(id, name, value);
        self.types.insert(id, ty);
        id
    }

    pub fn resolve_binary(
        &mut self,
        lhs: ASTExpression,
        op: crate::ast::Operator,
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
    ///Resolves the provided values on a element. The `ty`is the type of the component we are resolving it
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
            ASTExpressionKind::Identifier(name) => {
                let (id, _) = self.retrieve_information_of_scoped(&name, &expr.span)?;
                Ok(HirExpression {
                    kind: HirExpressionKind::Identifier(id),
                    id: HirId::new(),
                    ty: HirType::Reference {
                        rf: id,
                        generics: None,
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
                ElementDeffinitionKind::Property {
                    modifier,
                    ty,
                    rhs,
                    name,
                } => {
                    let ty = if let Some(ty) = ty {
                        self.retrieve_type_of_name(&ty, &ty.span)?
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
                        span: def.span,
                    });
                    self.last_scope().insert_named_value(
                        id,
                        name,
                        HirValue {
                            ty,
                            kind: HirValueKind::Property { modifier },
                        },
                    );
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
            }
        }
        Ok(out)
    }

    ///Hoist the provided `ast` declaration, with so no errors of undefined values because declared later may occurr
    fn hoist(&mut self, ast: &ASTDeclaration) -> Result<(), HIRError> {
        match &ast.kind {
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
                    name.clone(),
                    HirValue {
                        kind: HirValueKind::Function {
                            modifier: PropertyModifier::Private,
                        },
                        ty: func_ty.clone(),
                    },
                    func_ty,
                );
            }
            ASTDeclarationKind::ElementDeclaration {
                name, deffinitions, ..
            } => {
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
                        }
                    }
                    out
                };
                self.create_hirid_for(
                    name.clone(),
                    HirValue {
                        kind: HirValueKind::Component {
                            modifier: PropertyModifier::Private,
                        },
                        ty: HirType::GenericComponent,
                    },
                    HirType::Component { props },
                );
            }
        }
        Ok(())
    }
    fn resolve(&mut self, ast: ASTDeclaration) -> Result<(), HIRError> {
        match ast.kind {
            ASTDeclarationKind::FuncDeclaration {
                name,
                args,
                mut body,
                ..
            } => {
                let (id, func) = self.retrieve_information_of(&name, &ast.span)?;

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
                    )
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
                    kind: HirDeclarationKind::Function { statments, name },
                    id,
                    ty: func,
                    span: ast.span,
                });
                self.exit_scope();
            }
            ASTDeclarationKind::ElementDeclaration { deffinitions, name } => {
                self.enter_scope();

                let (hir, ty) = self.retrieve_information_of(&name, &ast.span)?;

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
}
