LOGS=decrypt/log encrypt/log errors/log fall/log fib/log identity/log \
	 logic/log reverse/log stack/log sum/log sumdif/log

TARGET = tests.log

all:
	cd ..; make tests


tests: $(TARGET)
	
$(TARGET): $(LOGS)
	cat $(LOGS) > tests.log

%/log:
	touch $*/log

clean:
	rm -f $(TARGET) $(LOGS)
