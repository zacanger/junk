PC=$(shell pkg-config --cflags --libs Qt5Widgets qtermwidget5)
PREFIX ?= /usr/local
CONFIG_PREFIX ?= /usr/share

mt:
	g++ $(PC) -fPIC -o mt main.cpp

install:
	mkdir -p $(PREFIX)/bin
	cp -f mt $(PREFIX)/bin/mt
	chmod 755 $(PREFIX)/bin/mt
	mkdir -p $(CONFIG_PREFIX)/qtermwidget5/color-schemes
	cp -f Z.colorscheme $(CONFIG_PREFIX)/qtermwidget5/color-schemes/Z.colorscheme

clean:
	rm mt

.PHONY: mt clean install
