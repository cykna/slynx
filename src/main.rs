pub mod checker;
pub mod compiler;
pub mod hir;
pub mod intermediate;
pub mod parser;
use std::path::PathBuf;

use crate::{
    checker::TypeChecker,
    compiler::Compiler,
    intermediate::IntermediateRepr,
    parser::{Parser as SlynxParser, error::ParseError, lexer::Lexer},
    //flattener::{FlattenedHir, Flattener},
};
use clap::Parser;

#[derive(Debug, Parser)]
struct Cli {
    #[arg(short, long)]
    target: String,
}

/*#[link_name = "slynx_backend"]
unsafe extern "C" {
    fn compile_code(hir: FlattenedHir);
}*/

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();
    let path = PathBuf::from(cli.target);

    let file = std::fs::read_to_string(&path).unwrap();
    let lines = file
        .chars()
        .enumerate()
        .filter_map(|(idx, c)| if c == '\n' { Some(idx) } else { None })
        .collect::<Vec<usize>>();
    let stream = Lexer::tokenize(&file);
    let mut value = SlynxParser::new(stream);
    let ast = match value.parse_declarations() {
        Ok(val) => val,
        Err(e) => match e {
            ParseError::UnexpectedToken(ref tk) => {
                let line = match lines.binary_search(&tk.span.start) {
                    Ok(ok) => ok,
                    Err(e) => e + 1,
                };
                eprintln!("At line {line} errored: {e}");
                std::process::exit(1);
            }
            _ => panic!("{e:?}"),
        },
    };

    let mut hir = hir::SlynxHir::new();
    hir.generate(ast)?;
    if let Err(e) = TypeChecker::check(&mut hir) {
        eprint!("Type Error: {:?}; ", e.kind);
        let line = match lines.binary_search(&e.span.start) {
            Ok(idx) => idx,
            Err(idx) => idx + 1,
        };
        let column = e.span.end - e.span.start;
        eprintln!("At {}:{}", line, column);
        std::process::exit(1);
    };
    let mut intermediate = IntermediateRepr::new();
    let _ = intermediate.generate(hir.declarations);
    let mut compiler = Compiler::new();
    let out = compiler.compile(&intermediate);
    let _ = std::fs::write(path.with_extension("js"), out);
    Ok(())
}
