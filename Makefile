LATEX = pdflatex

default: main

main:
	$(LATEX) main
	bibtex main
	$(LATEX) main
	$(LATEX) main

	rm -f *.aux *.dvi *.out *.log *.bbl *.blg

	evince main.pdf &
