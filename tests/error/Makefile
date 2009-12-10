NAME=error

TESTLOGS = error01.error.log \
		   error02.error.log \
		   error03.error.log \
		   error04.error.log \
		   error05.error.log \
		   error06.error.log \
		   error07.error.log \
		   error08.error.log \
		   error09.error.log \
		   error10.standard.log \
		   error11.standard.log \
		   error12.standard.log \
		   error13.standard.log \
		   error14.standard.log \
		   error15.standard.log \

$(NAME).log: $(TESTLOGS)
	cat $(TESTLOGS) > $(NAME).log

include ../Makefile.template
