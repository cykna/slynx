test:
    cargo test -- --no-capture

test-logging file:
    cargo test -- --no-capture --show-output 2>&1 | tee {{file}}

debug:
    RUST_BACKTRACE=1 cargo run

run:
    cargo run

release:
    cargo run --release
