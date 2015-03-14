TARGETS = main
TEST_FILES = $(wildcard	*_test.c)

CC = gcc -ggdb

.PHONY: all test clean

clean:
	rm -f *.o $(TARGETS)
	rm -rf *.dSYM/
	$(MAKE) -C test clean

decoder_test: test/decoder_test.o decoder.o pcg/pcg_basic.o test/test.o

pcg/pgc_basic.o: pcg/pcg_basic.c pcg/pcg_basic.h

main: decoder.o

test:
	$(MAKE) -C test
