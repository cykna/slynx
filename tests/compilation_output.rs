use std::{
    fs,
    path::PathBuf,
    sync::Arc,
    time::{SystemTime, UNIX_EPOCH},
};

use slynx::{SlynxContext, compile_code, compiler::js::WebCompiler};

fn temp_case_dir(name: &str) -> PathBuf {
    let mut path = std::env::temp_dir();
    let nonce = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("clock should be after unix epoch")
        .as_nanos();
    path.push(format!("slynx-{name}-{}-{nonce}", std::process::id()));
    path
}

fn write_temp_source(case_dir: &PathBuf) -> PathBuf {
    fs::create_dir_all(case_dir).expect("temp case dir should be created");
    let source_path = case_dir.join("input.slynx");
    let source = fs::read_to_string("slynx/booleans.slynx").expect("fixture should exist");
    fs::write(&source_path, source).expect("temp source should be written");
    source_path
}

#[test]
fn compile_returns_output_before_writing() {
    let case_dir = temp_case_dir("compile-output");
    let source_path = write_temp_source(&case_dir);
    let output_path = source_path.with_extension("js");

    let context = SlynxContext::new(Arc::new(source_path)).expect("context should be created");
    let output = context
        .compile(WebCompiler::new())
        .expect("compilation should succeed");

    assert_eq!(output.output_path(), output_path.as_path());
    assert!(!output.bytes().is_empty());
    assert!(!output_path.exists());

    output.write().expect("output should be written");
    assert!(output_path.exists());

    fs::remove_dir_all(case_dir).expect("temp case dir should be removed");
}

#[test]
fn compile_code_still_writes_js_output() {
    let case_dir = temp_case_dir("compile-code");
    let source_path = write_temp_source(&case_dir);
    let output_path = source_path.with_extension("js");

    compile_code(source_path).expect("compile_code should still write the output file");

    assert!(output_path.exists());
    assert!(
        !fs::read_to_string(&output_path)
            .expect("generated output should be readable")
            .is_empty()
    );

    fs::remove_dir_all(case_dir).expect("temp case dir should be removed");
}

#[test]
fn build_stages_produces_stable_hir_and_ir_dumps() {
    let case_dir = temp_case_dir("stable-dumps");
    let source_path = write_temp_source(&case_dir);

    let first = SlynxContext::new(Arc::new(source_path.clone()))
        .expect("context should be created")
        .build_stages()
        .expect("stages should build");
    let second = SlynxContext::new(Arc::new(source_path))
        .expect("context should be created")
        .build_stages()
        .expect("stages should build");

    assert_eq!(first.hir_text(), second.hir_text());
    assert_eq!(first.ir_text(), second.ir_text());

    fs::remove_dir_all(case_dir).expect("temp case dir should be removed");
}

#[test]
fn build_stages_can_write_hir_ir_and_js_outputs() {
    let case_dir = temp_case_dir("dump-files");
    let source_path = write_temp_source(&case_dir);
    let hir_path = source_path.with_extension("hir");
    let ir_path = source_path.with_extension("ir");
    let js_path = source_path.with_extension("js");

    let context = SlynxContext::new(Arc::new(source_path)).expect("context should be created");
    let stages = context.build_stages().expect("stages should build");

    fs::write(&hir_path, stages.hir_text()).expect("hir dump should be written");
    fs::write(&ir_path, stages.ir_text()).expect("ir dump should be written");
    stages
        .compile_output(context.entry_point_path(), WebCompiler::new())
        .write()
        .expect("js output should be written");

    assert!(hir_path.exists());
    assert!(ir_path.exists());
    assert!(js_path.exists());

    fs::remove_dir_all(case_dir).expect("temp case dir should be removed");
}
