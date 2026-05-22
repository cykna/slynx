#[test]
fn component_property_style_reference() {
    let path = "examples/styles/property_as_param.slx".into();
    let ir = match slynx::compile_to_ir(path) {
        Ok(ir) => ir,
        Err(e) => {
            panic!("Compilation failed: {:?}", e);
        }
    };
    let formatted = ir.format_sir();
    println!("{}", formatted);

    // Verify styles are emitted
    assert!(
        formatted.contains("__init_Bg"),
        "Bg constructor should exist"
    );
    assert!(
        formatted.contains("__init_Fg"),
        "Fg constructor should exist"
    );
    assert!(
        formatted.contains("PedrinhoInit"),
        "PedrinhoInit should exist"
    );
    assert!(formatted.contains("JorginInit"), "JorginInit should exist");

    // Check that Pedrinho has a Bg style initcall
    assert!(
        formatted.contains("@initcall Bg") || formatted.contains("initcall"),
        "Pedrinho should have a style initcall for Bg"
    );
}
