TARGETS = test
CC = gcc -ggdb $(TEST_DEFS)
TEST_FILES = $(shell find . -name \*_test.c)
TEST_OBJS = $(TEST_FILES:.c=.o)
PROG_OBJS = $(patsubst %_test.c,../%.c,$(TEST_FILES))
OBJS = $(TEST_OBJS) test.o ../pcg/pcg_basic.o $(PROG_OBJS)

.PHONY: all clean

all : run_tests

clean:
	rm -f *.o $(TARGETS)
	rm -rf *.dSYM/

test: TEST_DEFS := -D_TEST_ $(shell node ./runbuilder.js $(TEST_FILES))
test: $(OBJS)

run_tests: test
	@echo
	@./test 2> /dev/null
