.PHONY: run

IDRIS = $(shell which idris >/dev/null && which idris)

all: build

build: $(PACKAGES)
	$(IDRIS) --install Contrib.ipkg

clean:
	rm -f ./**/*.ibc
