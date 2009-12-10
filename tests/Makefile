TESTS = decrypt.test	\
		encrypt.test	\
		errors.test		\
		fall.test		\
		fib1.test		\
		fib2.test		\
		fib3.test		\
		identity.test	\
	    logic.test		\
		reverse.test	\
		stack.test		\
		sum.test		\
		sumdif.test

TARGET = tests.log

.PHONY : $(TESTS)

all:
	cd ..; make tests


tests: $(TARGET)
	

$(TARGET): $(TESTS)
	echo -n > $(TARGET)
	for i in `echo $(TESTS) | sed -e 's/.test//g'`; do \
		(cd $$i; make); \
		cat $$i/log >> $(TARGET); \
	done

%.test:
	cd $*; make

clean:
	rm -f $(TARGET)
	for i in `echo $(TESTS) | sed -e 's/.test//g'`; do \
		(cd $$i; make clean); \
	done

