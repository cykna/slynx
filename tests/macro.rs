use slynx::{
    checker::TypeChecker,
    compile_code,
    hir::{SlynxHir, macros::js::JSMacro},
    parser::{Parser, ast::ASTDeclaration, lexer::Lexer},
};
#[cfg(test)]
use std::path::PathBuf;
use std::{io::Write, sync::Arc};

fn generate_hir(ast: Vec<ASTDeclaration>) -> SlynxHir {
    let mut hir = SlynxHir::new();

    hir
}

#[test]
fn test_macro() {
    let path = PathBuf::from("./slynx/macro.slynx");
    compile_code(path);
    std::io::stdout().flush().unwrap();
}
