all: build

build:
	cargo build

release:
	cargo build --release

.PHONY: build clean

clean:
	-rm -rf target
