pub mod js;
use std::fmt::Debug;

use crate::parser::ast::{ASTDeclaration, ASTStatment, ElementDeffinition, MacroElementArgs};

///A trait to be applied to structs that implement macros that are executed on declaration level
pub trait DeclarationMacro: 'static + Debug {
    fn name(&self) -> &'static str;
    ///Executes the given `args` and returns a new vector of declarations to be used. The `declaration_index` is the index where
    ///this macro call appeared on the declarations array
    fn execute(&self, args: &Vec<ASTDeclaration>, declaration_index: usize) -> Vec<ASTDeclaration>;
}
///A trait to be applied to structs that implement macros that are executed on statment level
pub trait StatmentMacro: 'static + Debug {
    fn name(&self) -> &'static str;
    ///Executes the given `args` and returns a new vector of statments to be used instead. The `stamtnet_index` is the index wher
    ///this macro call appeared on a stamtnet array. Note that a code may have multiple functions, if this was 23, 7, 12, it means that, on the first func, it's 23, the second it's 7, and the third, 12
    fn execute(&self, args: &Vec<ASTStatment>, statment_index: usize) -> Vec<ASTStatment>;
}

///A trait to be applied to structs that implement macro that are executed on a component deffinition level.
///They can be used to inject JS to the function that will be generated or expand things
pub trait ElementMacro: 'static + Debug {
    fn name(&self) -> &'static str;
    fn execute(&self, args: &MacroElementArgs, deffinition_index: usize)
    -> Vec<ElementDeffinition>;
}
