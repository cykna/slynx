#Test the project and print output to the terminal.
test:
    cargo test -- --no-capture

#Run test with Rust backtraces enabled.
#Shows a stack trace when a test panics.
test-backtrace:
    RUST_BACKTRACE=1 cargo test -- --no-capture

#Run test, display all output, and save it to a file.
test-logging file:
    cargo test -- --no-capture --show-output 2>&1 | tee {{file}}

#Run the application in debug mode with backtraces enabled targeting a specific file.
#Useful for diagnosing runtime panics.
debug file:
    RUST_BACKTRACE=1 cargo run -- --target {{file}}

#Run the application in debug mode targeting a specific file.
run file:
    cargo run -- --target {{file}}

#Run the application in release mode.
release:
    cargo run --release
