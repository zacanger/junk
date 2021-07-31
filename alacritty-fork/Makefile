TARGET = alacritty

ASSETS_DIR = extra
RELEASE_DIR = target/release
MANPAGE = $(ASSETS_DIR)/alacritty.man
TERMINFO = $(ASSETS_DIR)/alacritty.info

vpath $(TARGET) $(RELEASE_DIR)

all: build

build: | $(TARGET)
$(TARGET):
	cargo build

release: | $(TARGET)
$(TARGET):
	cargo build --release

.PHONY: build clean $(TARGET)

clean:
	-rm -rf target
