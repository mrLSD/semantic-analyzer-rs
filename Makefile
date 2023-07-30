clippy:
	@cargo clippy -- -D warnings
	@cargo fmt --check

clippy-12:
	@cargo clippy --features=llvm12 --no-default-features -- -D warnings
	@cargo fmt --check

clippy-15:
	@cargo clippy --features=llvm15 -- -D warnings
	@cargo fmt --check

fmt:
	@cargo fmt

test:
	@cargo test --features=llvm15  -- --nocapture