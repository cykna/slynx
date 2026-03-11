use frontend::{TypeChecker, hir::SlynxHir, lexer::Lexer, parser::Parser};

fn render_hir(source: &str) -> String {
    let tokens = Lexer::tokenize(source).expect("source should tokenize");
    let declarations = Parser::new(tokens)
        .parse_declarations()
        .expect("source should parse");
    let mut hir = SlynxHir::new();
    hir.generate(declarations).expect("HIR should generate");
    let types = TypeChecker::check(&mut hir).expect("HIR should typecheck");
    hir.types_module = types;
    hir.render_text()
}

#[test]
fn renders_function_declaration_with_typed_body() {
    let rendered = render_hir(
        r#"
func inc(value: int): int {
    let result = value + 1;
    result
}
"#,
    );

    assert!(rendered.contains("func inc(value: int) -> int"));
    assert!(rendered.contains("let result: int = (value: int Add 1: int): int;"));
    assert!(rendered.contains("ret result: int;"));
}

#[test]
fn renders_object_and_component_shapes() {
    let rendered = render_hir(
        r#"
object Person{
    name: str,
    age: int
}

component Counter {
    pub prop count: int = 0;
    Text {
        text: count
    }
}
"#,
    );

    assert!(rendered.contains("object Person : Person"));
    assert!(rendered.contains("name: str"));
    assert!(rendered.contains("age: int"));
    assert!(rendered.contains("component Counter : Counter"));
    assert!(rendered.contains("prop pub count: int = 0: int;"));
    assert!(rendered.contains("specialized Text { text: count: int }: Component;"));
}
