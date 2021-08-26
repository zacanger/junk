TARGET = minterm

ASSETS_DIR = extra
RELEASE_DIR = target/release
MANPAGE = $(ASSETS_DIR)/minterm.man
TERMINFO = $(ASSETS_DIR)/minterm.info

APP_NAME = Minterm.app
APP_TEMPLATE = $(ASSETS_DIR)/osx/$(APP_NAME)
APP_DIR = $(RELEASE_DIR)/osx
APP_BINARY = $(RELEASE_DIR)/$(TARGET)
APP_BINARY_DIR = $(APP_DIR)/$(APP_NAME)/Contents/MacOS
APP_EXTRAS_DIR = $(APP_DIR)/$(APP_NAME)/Contents/Resources

vpath $(TARGET) $(RELEASE_DIR)
vpath $(APP_NAME) $(APP_DIR)

build:
	@cargo build

binary: | $(TARGET)
$(TARGET):
	@cargo build --release

app: | $(APP_NAME)
$(APP_NAME): $(TARGET)
	@mkdir -p $(APP_BINARY_DIR)
	@mkdir -p $(APP_EXTRAS_DIR)
	@gzip -c $(MANPAGE) > $(APP_EXTRAS_DIR)/minterm.1.gz
	@tic -xe minterm,minterm-direct -o $(APP_EXTRAS_DIR) $(TERMINFO)
	@cp -fRp $(APP_TEMPLATE) $(APP_DIR)
	@cp -fp $(APP_BINARY) $(APP_BINARY_DIR)
	@touch -r "$(APP_BINARY)" "$(APP_DIR)/$(APP_NAME)"
	@echo "Created '$@' in '$(APP_DIR)'"

install:
	@rm -rf /Applications/$(APP_NAME)
	@mv -f $(APP_DIR)/$(APP_NAME) /Applications/

count:
	@cloc --exclude-dir=x,.git,.github,target .

.PHONY: app binary clean dmg install $(TARGET)

clean:
	@cargo clean
