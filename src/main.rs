use std::{path::PathBuf, process::exit};

use clap::Parser;
use color_eyre::eyre::Result;

use backend::compiler::js::WebCompiler;
use slynx::context::SlynxContext;

#[derive(Debug, Parser)]
struct Cli {
    #[arg(short, long)]
    target: String,
    #[arg(long)]
    hir: bool,
    #[arg(long)]
    ir: bool,
}

fn main() -> Result<()> {
    color_eyre::install()?;
    let cli = Cli::parse();
    let path = PathBuf::from(cli.target);
    let ctx = SlynxContext::new(path.into())?;
    let result = (|| -> Result<()> {
        let stages = ctx.build_stages()?;
        if cli.hir {
            std::fs::write(ctx.dump_path("hir"), stages.hir_text())?;
        }
        if cli.ir {
            std::fs::write(ctx.dump_path("ir"), stages.ir_text())?;
        }
        let output = stages.compile_output(ctx.entry_point_path(), WebCompiler::new());
        output.write()?;
        Ok(())
    })();
    if let Err(e) = result {
        if !cfg!(debug_assertions) {
            eprintln!("{e}");
            exit(1);
        } else {
            Err(e)
        }
    } else {
        Ok(())
    }
}
