.PHONY: all slide clean cleanall

all: cleanall slide

slide: intro.pdf

intro.pdf: intro.tex
	xelatex intro.tex
	xelatex intro.tex

CLEANABLES := $(shell find . \( -name '*.aux'\
							-o -name '*.log'\
							-o -name '*.nav'\
							-o -name '*.out'\
							-o -name '*.snm'\
							-o -name '*-blx.bib'\
							-o -name '*.bbl'\
							-o -name '*.bcf'\
							-o -name '*.blg'\
							-o -name '*.run.xml'\
							-o -name '*.toc' \) -type f -not -path "./.git/*")

ALLCLEANABLES := $(CLEANABLES) $(shell find . -name '*.pdf' -type f -not -path "./.git/*")

clean:
	@for file in $(CLEANABLES); do\
		echo "Removing $$file";\
		rm -f $$file;\
	done

cleanall:
	@for file in $(ALLCLEANABLES); do\
		echo "Removing $$file";\
		rm -f $$file;\
	done
