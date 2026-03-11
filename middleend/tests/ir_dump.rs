use frontend::{TypeChecker, hir::SlynxHir, lexer::Lexer, parser::Parser};
use middleend::IntermediateRepr;

fn render_ir(source: &str) -> String {
    let tokens = Lexer::tokenize(source).expect("source should tokenize");
    let declarations = Parser::new(tokens)
        .parse_declarations()
        .expect("source should parse");

    let mut hir = SlynxHir::new();
    hir.generate(declarations).expect("HIR should generate");
    let types = TypeChecker::check(&mut hir).expect("HIR should typecheck");
    hir.types_module = types.clone();

    let mut ir = IntermediateRepr::new();
    ir.generate(hir.declarations.clone(), types);
    ir.render_text()
}

#[test]
fn renders_function_values_and_code() {
    let rendered = render_ir(
        r#"
func inc(n: int): int {
    let result = n + 1;
    result
}
"#,
    );

    assert!(rendered.contains("int #inc(p0: int) {"));
    assert!(rendered.contains("values {"));
    assert!(rendered.contains("= add "));
    assert!(rendered.contains("code {"));
    assert!(rendered.contains("alloc l0;"));
    assert!(rendered.contains("ret %e"));
}

#[test]
fn renders_structs_and_internalized_strings() {
    let rendered = render_ir(
        r#"
object Person {
    name: str,
    age: int,
}

func main(): Person -> Person(name: "jorge", age: 44);
"#,
    );

    assert!(rendered.contains("struct %StrHandle {usize, usize};"));
    assert!(rendered.contains("@str0 = \"jorge\";"));
    assert!(rendered.contains("struct %Person {%StrHandle, int};"));
    assert!(rendered.contains("%Person {"));
}

#[test]
fn renders_components_and_native_nodes() {
    let rendered = render_ir(
        r#"
component HelloBox {
    pub prop maria = 0;

    Div {
        Text {
            text: maria
        }
    }
}

func main(): Component -> HelloBox {};
"#,
    );

    assert!(rendered.contains("component %HelloBox(int) {"));
    assert!(rendered.contains("%maria: int = %e0;"));
    assert!(rendered.contains("Rect(children=["));
    assert!(rendered.contains("Text(text="));
}
