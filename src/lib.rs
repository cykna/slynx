use std::{path::PathBuf, sync::Arc};

pub mod context;
pub use context::*;
pub use frontend::checker;
pub use frontend::parser;
use middleend::IntermediateRepr;
pub use middleend::intermediate;

///Compiels the provided `slynx` code from the provided `path` and writes the slynx IR textual form into the same `path` but with extension `sir`
pub fn compile_code(path: PathBuf) -> color_eyre::eyre::Result<()> {
    let context = SlynxContext::new(Arc::new(path))?;
    let output = context.compile()?;
    output.write()?;
    Ok(())
}

///Compiels the provided `slynx` code from the provided `path` and writes the slynx IR textual form into the same `path` but with extension `sir`
pub fn compile_to_ir(path: PathBuf) -> color_eyre::eyre::Result<IntermediateRepr> {
    let context = SlynxContext::new(Arc::new(path))?;
    let output = context.compile()?;
    Ok(output.ir())
}
