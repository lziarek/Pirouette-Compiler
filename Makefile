FILE := $(word 2, $(MAKECMDGOALS))

%:
	@:

.PHONY: all build clean test-pp bisect-pp pp json

build:
	dune build

pp:
	dune exec pirc -- -ast-dump pprint $(FILE)

json:
	dune exec pirc -- -ast-dump json $(FILE)

test-pp: clean
	dune exec --instrument-with bisect_ppx test/prettyprint_test.exe

bisect-pp: clean test-pp
	bisect-ppx-report html --theme=dark

clean:
	dune clean
	rm -rf _coverage bisect*.coverage