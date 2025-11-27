use slynx::{
    ast::ASTDeclaration,
    checker::TypeChecker,
    hir::{SlynxHir, macros::js::JSMacro},
};
#[cfg(test)]
use std::path::PathBuf;
use std::sync::Arc;

fn generate_hir(ast: Vec<ASTDeclaration>) -> SlynxHir {
    let mut hir = SlynxHir::new();
    let js = Arc::new(JSMacro {});
    hir.insert_declaration_macro(js.clone());
    hir.insert_statment_macro(js);
    hir.generate(ast).unwrap();

    hir
}

#[test]
fn test_macro() {
    let path = PathBuf::from("./slynx/macro.slynx");

    let file = std::fs::read_to_string(&path).unwrap();
    let lines = file
        .chars()
        .enumerate()
        .filter_map(|(idx, c)| if c == '\n' { Some(idx) } else { None })
        .collect::<Vec<usize>>();

    let value = slynx::slynx::ProgramParser::new().parse(&file).unwrap();

    //println!("{value:#?}");
    let mut hir = generate_hir(value);
    if let Err(e) = TypeChecker::check(&mut hir) {
        eprint!("Type Error: {:?}; ", e.kind);
        let line = match lines.binary_search(&e.span.start) {
            Ok(idx) => idx,
            Err(idx) => idx + 1,
        };
        let column = e.span.end - e.span.start;
        panic!("At {}:{}", line, column);
    };
    println!("\n{hir:#?}");
}
