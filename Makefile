FILE := $(word 2, $(MAKECMDGOALS))
LATEX_DOCS := $(shell find docs -name '*.tex')

.PHONY: build docs pp json dot test-infer test-pp bisect-pp clean cleandocs cleanall

build:
	dune build

docs: $(LATEX_DOCS)
	@for file in $^; do \
		base=$$(basename $$file .tex); \
		dir=$$(dirname $$file); \
		if [ -f "$${dir}/$$base.bib" ]; then \
			pdflatex -output-directory=$$dir $$file; \
			biber --input-directory $$dir --output-directory $$dir $$base; \
			pdflatex -output-directory=$$dir $$file; \
			pdflatex -output-directory=$$dir $$file; \
		else \
			pdflatex -output-directory=$$dir $$file; \
		fi; \
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
	rm -rf $(shell find docs -name '*.aux' -o -name '*.log' -o -name '*.out' \
	-o -name '*.bbl' -o -name '*.bcf' -o -name '*.blg' -o -name '*.run.xml' \
	-o -name '*.lox' -o -name '*.toc')

clean: cleandocs
	dune clean

cleanall: clean
	rm -rf _build _coverage bisect*.coverage $(shell find docs -name '*.pdf')

%:
	@:
