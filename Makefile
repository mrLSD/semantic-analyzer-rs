clippy:
	@cargo clippy -- -D warnings
	@cargo fmt --check

clippy-12:
	@cargo clippy --features=llvm12 -- -D warnings
	@cargo fmt --check

clippy-15:
	@cargo clippy --features=llvm15 -- -D warnings
	@cargo fmt --check

fmt:
	@cargo fmt
	