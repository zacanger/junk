SHELL := /bin/bash
BINARY := term
FONTPATH := ./gui/packed-fonts
VERSION := $(shell git describe --tags 2>/dev/null)

build:
	@go build -ldflags "-X github.com/zacanger/term/version.Version=$version"

lint:
	@go fmt ./...
	@go vet ./...
	@staticcheck ./...

test:
	@go test ./...

cover:
	@go test -coverprofile=coverage.out ./...

coverage:
	@go tool cover -html=coverage.out

fmt:
	go fmt ./...

install:
	@mkdir -p $(PREFIX)/bin
	@cp -f cozy $(PREFIX)/bin/term
	@chmod 755 $(PREFIX)/bin/term

clean:
	@rm -f term coverage.out

tags:
	@ctags --exclude=x -R .

.PHONY: clean install tags todo
