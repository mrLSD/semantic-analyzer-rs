clippy:
	@cargo clippy -- -D clippy::pedantic
	@cargo fmt --check

fmt:
	@cargo fmt
	