FILE := $(word 2, $(MAKECMDGOALS))
LATEX_DOCS := $(wildcard docs/*.tex)

.PHONY: all build docs pp json dot test-pp bisect-pp clean cleandoc cleanall

build:
	dune build

doc: 
	for file in $(LATEX_DOCS); do \
		pdflatex -output-directory=docs $$file; \
	done

pp:
	dune exec pirc -- -pprint $(FILE)

json:
	dune exec pirc -- -json $(FILE)

dot: 
	dune exec pirc -- -dot $(FILE)

test-pp: cleanall
	dune exec test/prettyprint_test.exe

bisect-pp: cleanall
	dune exec --instrument-with bisect_ppx test/prettyprint_test.exe
	bisect-ppx-report html --theme=dark

clean:
	dune clean

cleandoc:
	rm -rf docs/*.aux docs/*.log docs/*.out

cleanall: clean cleandoc
	rm -rf _build _coverage bisect*.coverage examples/*.json examples/*.ast examples/*.dot docs/*.pdf

%:
	@:
