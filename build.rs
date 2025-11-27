use std::{path::PathBuf, process::Command};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let backend_source = PathBuf::from("./backend/src/main.zig");
    let out_dir = PathBuf::from(std::env::var("OUT_DIR").unwrap());
    let backend_name = "slynx_backend";
    let backend_outdir = out_dir.join(format!("lib{}.a", backend_name));
    let mut cmd = Command::new("zig");
    cmd.current_dir("./backend");
    let status = cmd
        .arg("build-lib")
        .arg("src/main.zig")
        .arg("--name")
        .arg(backend_name)
        .arg(format!("-femit-bin={}", backend_outdir.display()))
        .arg("-static")
        .arg("-target")
        .arg("x86_64-linux-musl")
        .arg("-fPIC")
        .spawn()
        .unwrap()
        .wait()
        .expect("Failed to execute Zig compiler. Ensure 'zig' is in your PATH.");

    if !status.success() {
        panic!("Zig compilation failed with status: {:?}", status);
    } else {
        println!("{:?}", status);
    }

    // 4. Tell Cargo about the compiled library

    // Tell Cargo where to find the static library file (`libbackend.a`)
    println!("cargo:rustc-link-search=native={}", out_dir.display());

    // Tell Cargo to link against the library named 'backend'
    println!("cargo:rustc-link-lib=static={}", backend_name);

    // Optional: Re-run the build script if the Zig source changes
    println!("cargo:rerun-if-changed={}", backend_source.display());

    lalrpop::Configuration::new()
        .always_use_colors()
        .log_verbose()
        .process_current_dir()
}
