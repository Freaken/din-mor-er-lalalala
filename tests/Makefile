# Disse tests er der
TESTS = decrypt.test	\
		encrypt.test	\
		error.test		\
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

# Targets for at køre make i alle mapperne
MAKETESTS = $(shell echo $(TESTS) | sed -e 's/.test/.make/g')

# De reele filer
TESTLOGS = $(shell echo $(TESTS) | sed -e 's:\([a-z1-3]*\).test:\1/\1.log:g')

# Outputtet af at køre make
TARGET = tests.log

# Først bliver der kørt make i testmapperne, $(TARGET) opdateret,
# hvis der rent faktisk er sket ændringer i nogen af mapperne
all: $(MAKETESTS) $(TARGET)

# Hvis der er sket ændringer, så skriv dem til $(TARGET)
$(TARGET): $(TESTLOGS)
	cat $(TESTLOGS) > $(TARGET); \

# Kør make i den tilsvarende mappe
%.make:
	(cd $*; make)

# Oprydning (også i alle mapperne)
clean:
	rm -f $(TARGET)
	for i in `echo $(TESTS) | sed -e 's/.test//g'`; do \
		(cd $$i; make clean); \
	done

