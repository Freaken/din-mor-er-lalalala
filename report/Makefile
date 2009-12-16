TARGET=report.pdf
TEXFILES=preamble.tex intro.tex macros.tex lex.tex parse.tex type.tex compile.tex reqs.tex design.tex tests.tex

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

print: $(TARGET)
	    cat $(TARGET) | ssh mathias@ask.diku.dk lp -d m1b

# Fejlhåndtering
../tests/tests.log:
	@echo "\033[31mFejl: tests.log findes ikke\033[0m"
	@echo "\033[31mFejl: Kør make på testsmappen før du fortsætter\033[0m"
	@exit 1
