use std::{path::PathBuf, sync::Arc};

pub mod context;
pub use backend::compiler;
use backend::js::WebCompiler;
pub use context::*;
pub use frontend::checker;
pub use frontend::parser;
pub use middleend::intermediate;

pub fn compile_code(path: PathBuf) -> color_eyre::eyre::Result<()> {
    let context = SlynxContext::new(Arc::new(path))?;
    let output = context.compile(WebCompiler::new())?;
    output.write()?;
    Ok(())
}
