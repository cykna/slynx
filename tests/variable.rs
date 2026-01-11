use std::{path::PathBuf, sync::Arc};

use slynx::compiler::js::WebCompiler;

#[test]
fn test_variables() {
    let context =
        slynx::SlynxContext::new(Arc::new(PathBuf::from("slynx/variables.slynx"))).unwrap();
    context.start_compilation(WebCompiler::new()).unwrap();
}
