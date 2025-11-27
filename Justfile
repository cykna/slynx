test:
    cargo test -- --no-capture

dev:
    RUST_BACKTRACE=1 cargo run

run:
    cargo run

release:
    cargo run --release
