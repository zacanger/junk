PREFIX ?= /usr/local

build:
	@cargo build

release:
	@cargo build --release

clean:
	@rm -rf target

install: release
	@mkdir -p $(PREFIX)/bin
	@cp -f target/release/zterm $(PREFIX)/bin/zterm
	@chmod 755 $(PREFIX)/bin/zterm

tags:
	@ctags --exclude=x -R .

.PHONY: build clean install tags
