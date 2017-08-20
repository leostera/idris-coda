.PHONY: run

IDRIS = $(shell which idris >/dev/null && which idris)

all: build

build: $(PACKAGES)
	$(IDRIS) --install Coda.ipkg

clean:
	rm -f ./**/*.ibc
