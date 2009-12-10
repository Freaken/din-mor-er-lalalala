NAME=errors
TESTS = error01.log \
		error02.log \
		error03.log \
		error04.log \
		error05.log \
		error06.log \
		error07.log \
		error08.log \
		error09.log \
		error10.log \
		error11.log \
		error12.log \
		error13.log \
		error14.log \
		error15.log

.PHONY : all log $(TESTS) $(shell echo $(TESTS) | sed -e 's/.log/.jan/g')

all: log cleantmp

log: $(TESTS)
	echo -n > log
	for i in $(TESTS); do \
		cat $$i >> log; \
	done

error01.log: error01.compile.log
error02.log: error02.compile.log
error03.log: error03.compile.log
error04.log: error04.compile.log
error05.log: error05.compile.log
error06.log: error06.compile.log
error07.log: error07.compile.log
error08.log: error08.compile.log
error09.log: error09.compile.log
error10.log: error10.standard.log
error11.log: error11.standard.log
error12.log: error12.standard.log
error13.log: error13.standard.log
error14.log: error14.standard.log
error15.log: error15.standard.log

include ../Makefile.template
