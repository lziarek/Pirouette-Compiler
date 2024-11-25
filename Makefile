FILE := $(word 2, $(MAKECMDGOALS))
LATEX_DOCS := $(shell find docs -name '*.tex')

.PHONY: build docs check-env pp json dot test-infer test-pp bisect-pp clean cleandocs cleanall

build:
	dune build

check-env:
	@command -v pdflatex >/dev/null 2>&1 || { echo "Error: pdflatex is not installed. Please install TeX Live or MacTeX."; exit 1; }
	@command -v biber >/dev/null 2>&1 || { echo "Error: biber is not installed. Please install biber."; exit 1; }

docs: check-env
	@for file in $(LATEX_DOCS); do \
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
