TARGET=report.pdf
TEXFILES=preamble.tex macros.tex lex.tex parse.tex type.tex compile.tex reqs.tex design.tex tests.tex

all: $(TARGET)

# pdf'en skal opdateres hver gang der er ændret i en texfil eller i tests'ene.
$(TARGET): $(TEXFILES) ../tests/tests.log

# Oprydning
clean:
	rm -rf *.aux *.log *.out *.toc *.data *.blg *.bbl *~ $(TARGET)

# pdflatex kræver en del gennemkøringer for at opdatere sidetal og lignende
%.pdf: %.tex
	pdflatex $*
	bibtex $*
	pdflatex $*
	pdflatex $*


# Fejlhåndtering
../tests/tests.log:
	echo tests.log findes ikke
	echo Kør make på testsmappen før du fortsætter
	exit 0
