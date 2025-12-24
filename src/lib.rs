use std::{path::PathBuf, sync::Arc};

use crate::{
    checker::TypeChecker,
    compiler::Compiler,
    hir::{SlynxHir, macros::js::JSMacro},
    intermediate::IntermediateRepr,
};

pub mod checker;
pub mod compiler;
pub mod parser;

pub mod hir;
pub mod intermediate;

pub fn compile_code(path: PathBuf) -> i32 {
    let code = std::fs::read_to_string(&path).unwrap();
    let tokens = parser::lexer::Lexer::tokenize(&code).unwrap();
    let mut ast = parser::Parser::new(tokens);
    let decls = ast.parse_declarations().unwrap();
    let mut hir = SlynxHir::new();
    {
        let js = Arc::new(JSMacro {});
        hir.insert_element_macro(js.clone());
        hir.insert_statment_macro(js);
    }
    hir.generate(decls).unwrap();
    TypeChecker::check(&mut hir).unwrap();
    let mut intermediate = IntermediateRepr::new();
    intermediate.generate(hir.declarations);
    let mut compiler = Compiler::new();
    compiler.compile(&intermediate);
    let code = compiler.compile(&intermediate);
    println!("Writing: \n{code}");
    std::fs::write(path.with_extension("js"), code).unwrap();
    0
}
