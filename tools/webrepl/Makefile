.PHONY: run

NODE  = $(shell which node >/dev/null && which node)
IDRIS = $(shell which idris >/dev/null && which idris)

OUT = server.js

all: $(OUT)

codegen:
	$(IDRIS) --codegen node ./Server.idr -o $(OUT)

run: codegen
	$(NODE) $(OUT)

clean:
	rm -f $(OUT)
