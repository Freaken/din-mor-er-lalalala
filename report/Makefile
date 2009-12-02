GRAPHS=
TARGET=report.pdf
TEXFILES=preamble.tex del1.tex
HELPFILES=

all: $(TARGET)

$(TARGET): $(GRAPHS) $(TEXFILES) $(HELPFILES) Makefile

clean:
	rm -rf *.aux *.log *.out *.toc *.data *~ $(GRAPHS) $(TARGET)

%.svg: %.dot
	dot $*.dot -Tsvg -o $*.svg

%.png: %.dot
	dot $*.dot -Tpng -o $*.png

%.ps: %.dot
	dot $*.dot -Tps -o $*.ps

%_dot.pdf: %.dot
	dot $*.dot -Tpdf -o $*_dot.pdf

%_neato.pdf: %.dot
	neato $*.dot -Tpdf -o $*_neato.pdf

%_circo.pdf: %.dot
	circo $*.dot -Tpdf -o $*_circo.pdf

%.pdf: %.tex
	pdflatex $*.tex
	pdflatex $*.tex
	pdflatex $*.tex

print: $(TARGET)
	cat $(TARGET) | ssh mathias@ask.diku.dk lp -d m1b
