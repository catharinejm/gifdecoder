CC = gcc -ggdb $(TEST_DEFS)
TEST_FILES = $(shell find . -name \*_test.c)
TEST_OBJS = $(TEST_FILES:.c=.o)
OBJS = $(TEST_OBJS) test.o ../pcg/pcg_basic.o ../decoder.o

all : test

clean:
	rm -f *.o $(TARGETS)
	rm -rf *.dSYM/

test: TEST_DEFS := -D_TEST_ $(shell node ./runbuilder.js $(TEST_FILES))
test: clean $(OBJS)
	$(CC) -o test $(OBJS)
	@echo
	@./test 2> /dev/null

