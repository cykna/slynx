use slynx::{
    checker::TypeChecker,
    hir::{SlynxHir, macros::js::JSMacro},
    parser::ast::ASTDeclaration,
    parser::{Parser, lexer::Lexer},
};
#[cfg(test)]
use std::path::PathBuf;
use std::{io::Write, sync::Arc};

fn generate_hir(ast: Vec<ASTDeclaration>) -> SlynxHir {
    let mut hir = SlynxHir::new();
    let js = Arc::new(JSMacro {});
    hir.insert_statment_macro(js.clone());
    hir.insert_element_macro(js);
    hir.generate(ast).unwrap();

    hir
}

#[test]
fn test_macro() {
    let path = PathBuf::from("./slynx/macro.slynx");

    let file = std::fs::read_to_string(&path).unwrap();
    let stream = Lexer::tokenize(&file);
    let value = Parser::new(Lexer::tokenize(&file))
        .parse_declarations()
        .unwrap();

    //println!("{value:#?}");
    let mut hir = generate_hir(value);
    if let Err(e) = TypeChecker::check(&mut hir) {
        eprint!("Type Error: {:?}; ", e.kind);
        let line = match stream.new_lines.binary_search(&e.span.start) {
            Ok(idx) => idx,
            Err(idx) => idx + 1,
        };
        let column = e.span.end - e.span.start;
        panic!("At {}:{}", line, column);
    };
    println!("\n{hir:#?}");
    std::io::stdout().flush().unwrap();
}
