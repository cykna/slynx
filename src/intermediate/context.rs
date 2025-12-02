use std::{borrow::Cow, collections::HashMap};

use crate::{
    hir::HirId,
    intermediate::{expr::IntermediateExpr, node::IntermediateInstruction},
};

#[derive(Debug)]
///A Intermediate property used to bind an id to it's default value on the current context
pub struct IntermediateProperty {
    pub id: HirId,
    pub default_value: Option<usize>,
}

#[derive(Debug)]
pub enum IntermediateContextType {
    Function {
        name: String,
        instructions: Vec<IntermediateInstruction>,
        param_len: usize,
    },
    Element {
        properties: Vec<IntermediateProperty>,
        children: Vec<usize>,
        js: Vec<Cow<'static, str>>,
    },
}

#[derive(Debug)]
pub struct IntermediateContext {
    pub id: HirId,
    pub exprs: Vec<IntermediateExpr>,
    ///A vector of pointer to the expressions
    pub vars: Vec<usize>,
    ///Maps some name to an expression
    pub names: HashMap<HirId, usize>,
    pub ty: IntermediateContextType,
}

impl IntermediateContext {
    pub fn new_function(id: HirId, name: String, args_len: usize) -> Self {
        Self {
            id,
            exprs: Vec::new(),
            vars: Vec::new(),
            names: HashMap::new(),
            ty: IntermediateContextType::Function {
                instructions: Vec::new(),
                name,
                param_len: args_len,
            },
        }
    }
    pub fn new_element(id: HirId) -> Self {
        Self {
            id,
            exprs: Vec::new(),
            vars: Vec::new(),
            names: HashMap::new(),
            ty: IntermediateContextType::Element {
                properties: Vec::new(),
                children: Vec::new(),
                js: Vec::new(),
            },
        }
    }
    ///Inserts the provided `expr` on this context and returns it's id
    pub fn insert_expr(&mut self, expr: IntermediateExpr) -> usize {
        let id = self.exprs.len();
        self.exprs.push(expr);
        id
    }
    ///Adds on this context instructions, the given `instr`.
    ///Returns the ID of the instruction. Returns None if this isn't a function
    pub fn insert_node(&mut self, instr: IntermediateInstruction) -> Option<usize> {
        match &mut self.ty {
            IntermediateContextType::Function { instructions, .. } => {
                let id = instructions.len();
                instructions.push(instr);
                Some(id)
            }
            _ => None,
        }
    }
    ///Inserts a new property on this element and returns it's property child index.
    ///Returns None if this isn't an element
    pub fn insert_property(&mut self, expr: IntermediateProperty) -> Option<usize> {
        match &mut self.ty {
            IntermediateContextType::Element { properties, .. } => {
                let prop = properties.len();
                properties.push(expr);
                Some(prop)
            }
            _ => None,
        }
    }

    ///Creates a new child that value is the provided `expr` in this context. Returns its index if this is a context, else None, and does nothing
    pub fn insert_child(&mut self, expr: usize) -> Option<usize> {
        if let IntermediateContextType::Element {
            ref mut children, ..
        } = self.ty
        {
            let id = children.len();
            children.push(expr);
            Some(id)
        } else {
            None
        }
    }
    ///Inserts the provided `js` code inside. Note that this won't change the way it's transpiled. On components, this will be copy-pasted directly, after the props and on functions, it will be in order of appearance
    pub fn insert_js(&mut self, js: Cow<'static, str>) {
        match &mut self.ty {
            IntermediateContextType::Element { js: js_vec, .. } => {
                js_vec.push(js);
            }
            IntermediateContextType::Function { instructions, .. } => {
                instructions.push(IntermediateInstruction::Js(js));
            }
        }
    }
}
