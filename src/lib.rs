use std::path::PathBuf;

use crate::{
    checker::TypeChecker,
    compiler::{js::WebCompiler, slynx_compiler::SlynxCompiler},
    hir::SlynxHir,
    intermediate::IntermediateRepr,
};

pub mod checker;
pub mod compiler;
pub mod parser;

mod context;
pub mod hir;
pub mod intermediate;
pub use context::*;

pub fn compile_code(path: PathBuf) -> color_eyre::eyre::Result<()> {
    let code = std::fs::read_to_string(&path)?;
    let tokens = parser::lexer::Lexer::tokenize(&code)?;
    let mut ast = parser::Parser::new(tokens);
    let decls = ast.parse_declarations()?;
    let mut hir = SlynxHir::new();

    hir.generate(decls)?;
    TypeChecker::check(&mut hir)?;
    let mut intermediate = IntermediateRepr::new();
    intermediate.generate(hir.declarations);
    let compiler = WebCompiler::new();
    let code = compiler.compile(intermediate);
    std::fs::write(path.with_extension("js"), code)?;
    Ok(())
}
