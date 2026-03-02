use slynx::{
    checker::{
        TypeChecker,
        error::{TypeError, TypeErrorKind},
    },
    hir::{
        SlynxHir,
        definitions::{HirDeclarationKind, HirExpressionKind, HirStatementKind},
    },
    parser::{Parser, lexer::Lexer},
};

fn load_hir(path: &str) -> SlynxHir {
    let source = std::fs::read_to_string(path).expect("source file should exist");
    let tokens = Lexer::tokenize(&source).expect("source should tokenize");
    let declarations = Parser::new(tokens)
        .parse_declarations()
        .expect("source should parse");
    let mut hir = SlynxHir::new();
    hir.generate(declarations).expect("HIR should generate");
    hir
}

#[test]
fn typechecker_rejects_function_call_with_missing_arg() {
    let mut hir = load_hir("slynx/functioncall.slynx");
    let mut found = false;

    for declaration in &mut hir.declarations {
        let HirDeclarationKind::Function {
            name, statements, ..
        } = &mut declaration.kind
        else {
            continue;
        };
        if name != "main" {
            continue;
        }

        for statement in statements {
            let expr = match &mut statement.kind {
                HirStatementKind::Variable { value, .. } => value,
                HirStatementKind::Expression { expr } => expr,
                HirStatementKind::Return { expr } => expr,
                HirStatementKind::Assign { value, .. } => value,
            };
            let HirExpressionKind::FunctionCall { args, .. } = &mut expr.kind else {
                continue;
            };
            args.pop();
            found = true;
            break;
        }

        if found {
            break;
        }
    }
    assert!(found, "expected to find a function call inside main");

    let err = TypeChecker::check(&mut hir)
        .expect_err("type checker should reject function calls with missing args");
    let type_error = err
        .downcast_ref::<TypeError>()
        .expect("error should come from type checker");

    match &type_error.kind {
        TypeErrorKind::InvalidFuncallArgLength {
            expected_length,
            received_length,
        } => {
            assert_eq!(*expected_length, 2);
            assert_eq!(*received_length, 1);
        }
        other => panic!("expected InvalidFuncallArgLength, got {other:?}"),
    }
}
