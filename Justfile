test:
    cargo test -- --no-capture

test-backtrace:
    RUST_BACKTRACE=1 cargo test -- --no-capture

test-logging file:
    cargo test -- --no-capture --show-output 2>&1 | tee {{file}}

debug file:
    RUST_BACKTRACE=1 cargo run -- --target {{file}}

run file:
    cargo run -- --target {{file}}

release:
    cargo run --release
