pub mod ast;
pub mod checker;
pub mod compiler;
pub mod hir;
pub mod intermediate;
use std::path::PathBuf;

use clap::Parser;

use lalrpop_util::lalrpop_mod;

use crate::{
    checker::TypeChecker,
    compiler::Compiler,
    intermediate::IntermediateRepr,
    //flattener::{FlattenedHir, Flattener},
};
lalrpop_mod!(pub slynx, "/grammar/slynx.rs");

#[derive(Debug, Parser)]
struct Cli {
    #[arg(short, long)]
    target: String,
}

/*#[link_name = "slynx_backend"]
unsafe extern "C" {
    fn compile_code(hir: FlattenedHir);
}*/

fn main() {
    let cli = Cli::parse();
    let path = PathBuf::from(cli.target);

    let file = std::fs::read_to_string(&path).unwrap();
    let lines = file
        .chars()
        .enumerate()
        .filter_map(|(idx, c)| if c == '\n' { Some(idx) } else { None })
        .collect::<Vec<usize>>();

    let value = slynx::ProgramParser::new().parse(&file).unwrap();

    let mut hir = match hir::SlynxHir::new(value) {
        Ok(hir) => hir,
        Err(e) => panic!("{e:#?}"),
    };
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
}
