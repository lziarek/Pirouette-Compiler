build:
	dune build

.PHONY: test
test-parsing:
	dune exec test/parsing_test.exe

test-pp:
	dune exec --instrument-with bisect_ppx test/prettyprint_test.exe

bisect-pp: clean test-pp
	bisect-ppx-report html --theme=light

clean:
	dune clean
	rm -rf _coverage bisect*.coverage