.PHONY: build

STACK = $(shell which stack >/dev/null && which stack)

build:
	PKG_CONFIG_PATH=/usr/local/opt/libffi/lib/pkgconfig $(STACK) build
