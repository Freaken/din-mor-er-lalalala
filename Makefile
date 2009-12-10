TARGETS = tests src report

all: $(TARGETS)
	cp report/report.pdf .
	cp src/JC .

.PHONY : $(TARGETS) clean

src:
	cd src; make src

tests: src
	cd tests; make tests

report: tests
	cd report; make report

clean:
	for i in $(TARGETS); do \
	  ( cd $$i; make clean ) ; \
	done
	rm -f report.pdf JC

commit: clean
	git commit -a
	git push


