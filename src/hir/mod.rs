pub mod deffinitions;
pub mod error;
mod implementation;
mod scope;
pub mod types;
use std::{
    collections::HashMap,
    sync::{Arc, atomic::AtomicU64},
};

use crate::{
    hir::{
        deffinitions::{
            ElementValueDeclaration, HirDeclaration, HirDeclarationKind, HirStatment,
            HirStatmentKind,
        },
        error::{HIRError, HIRErrorKind},

        scope::HIRScope,
        types::HirType,
    },
    parser::ast::{
        ASTDeclaration, ASTDeclarationKind, ASTStatment, ASTStatmentKind, ElementDeffinition,
        ElementDeffinitionKind, ElementValue, PropertyModifier, Span,
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
    ///A hashmap mapping the id of some struct or object to its layout. The 'layout' in case is the name of the property. So something like `object Packet {data: [100]u8, ty: PacketTy} would be simply
    ///id => ['data', 'ty'] to resolve its order correctly if some object expression like Packet(ty:PacketTy::Crypto, data:[100]0) appears
    objects_deffinitions: HashMap<HirId, Vec<String>>,
    ///The scopes of this HIR. On the final it's expected to have only one, which is the global one
    scopes: Vec<HIRScope>, 
    pub declarations: Vec<HirDeclaration>,
}

impl SlynxHir {
    pub fn new() -> Self {
        let out = Self {
            scopes: vec![HIRScope::new()],
            objects_deffinitions: HashMap::new(),
            names: HashMap::new(),
            types: HashMap::new(),
            declarations: Vec::new(),        };
        out
    } 

    ///Generates the declarations from the provided `ast`
    pub fn generate(&mut self, ast: Vec<ASTDeclaration>) -> Result<(), HIRError> { 
        for ast in &ast {
            self.hoist(ast)?;
        }
        for ast in ast {
            self.resolve(ast)?;
        }
        Ok(())
    } 
    ///Retrieves information about the provided `name` going from the current scope to the outer ones, finishing on the global
    pub fn retrieve_information_of_scoped(
        &mut self,
        name: &str,
        span: &Span,
    ) -> Result<HirId, HIRError> {
        let mut idx = self.scopes.len() - 1;

        while idx != 0 {
            let scope = &self.scopes[idx];

            let Ok(id) = scope.retrieve_name(name, span) else {
                idx -= 1;
                continue;
            };
            return Ok(*id);
        }
        Err(HIRError {
            kind: HIRErrorKind::NameNotRecognized(name.to_string()),
            span: span.clone(),
        })
    }

    ///Retrieves the hir id of the provided `name` in the global scope
    pub fn retrieve_hirdid_of(&mut self, name: &str, span: &Span) -> Result<HirId, HIRError> {
        self.names.get(name).cloned().ok_or(HIRError {
            kind: HIRErrorKind::NameNotRecognized(name.to_string()),
            span: span.clone(),
        })
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
    ///Hoist the provided `ast` declaration, with so no errors of undefined values because declared later may occurr
    fn hoist(&mut self, ast: &ASTDeclaration) -> Result<(), HIRError> {
        match &ast.kind {
            ASTDeclarationKind::ObjectDeclaration { name, fields } => {
                self.hoist_object(name, fields)?
            }

            ASTDeclarationKind::FuncDeclaration {
                name,
                args,
                return_type,
                ..
            } => self.hoist_function(name, args, return_type)?,
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
                            _ => {}
                        }
                    }
                    out
                };
                self.create_hirid_for(
                    name.to_string(), //add support for generic identifier
                    HirType::Component { props },
                );
            }
        }
        Ok(())
    }
    fn resolve(&mut self, ast: ASTDeclaration) -> Result<(), HIRError> {
        match ast.kind {
            ASTDeclarationKind::ObjectDeclaration { name, fields } => {
                self.resolve_object(name, fields,ast.span)?
            }
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
                    self.last_scope().insert_name(id, arg.name);
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
}
