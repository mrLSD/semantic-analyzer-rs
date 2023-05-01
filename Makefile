clippy:
	@cargo clippy -- -D warnings
	@cargo fmt --check

fmt:
	@cargo fmt
	