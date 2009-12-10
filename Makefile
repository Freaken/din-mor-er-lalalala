TARGETS = src tests report

all: $(TARGETS)

.PHONY : all $(TARGETS)

src:
	(cd src; make)

tests:
	(cd tests; make)

report:
	(cd report; make)

clean:
	for i in $(TARGETS); do \
	  ( cd $$i; make clean) ; \
	done

commit: clean
	git commit -a
	git push


