FILE := $(word 2, $(MAKECMDGOALS))
LATEX_DOCS := $(shell find docs -name '*.tex')

.PHONY: build docs pp json dot test-pp bisect-pp clean cleandoc cleanall

build:
	dune build

docs: $(LATEX_DOCS)
	@for file in $^; do \
		pdflatex -output-directory=$$(dirname $$file) $$file; \
	done
pp:
	dune exec pirc -- -pprint $(FILE)

json:
	dune exec pirc -- -json $(FILE)

dot: 
	dune exec pirc -- -dot $(FILE)

test-infer: cleanall
	dune exec test/typcheck_test.exe

test-pp: cleanall
	dune exec test/prettyprint_test.exe

bisect-pp: cleanall
	dune exec --instrument-with bisect_ppx test/prettyprint_test.exe
	bisect-ppx-report html

cleandocs:
	rm -rf $(shell find docs -name '*.aux' -o -name '*.log' -o -name '*.out')

clean: cleandocs
	dune clean

cleanall: clean
	rm -rf _build _coverage bisect*.coverage $(shell find docs -name '*.pdf')

%:
	@:
