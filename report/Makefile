TARGET=report.pdf
TEXFILES=preamble.tex lex.tex parse.tex compile.tex

all: $(TARGET)

# pdf'en skal opdateres hver gang der er ændret i en texfil eller i tests'ene.
$(TARGET): $(TEXFILES) ../tests/tests.log

# Oprydning
clean:
	rm -rf *.aux *.log *.out *.toc *.data *~ $(TARGET)

# pdflatex kræver en del gennemkøringer for at opdatere sidetal og lignende
%.pdf: %.tex
	pdflatex $*.tex
#	bibtex $*.tex
	pdflatex $*.tex
	pdflatex $*.tex


# Fejlhåndtering
../tests/tests.log:
	echo tests.log findes ikke
	echo Kør make på testsmappen før du fortsætter
	exit 0
