TARGETS = decoder_test main

CC = gcc -ggdb

clean:
	rm -f *.o **/*.o $(TARGETS)
	rm -rf *.dSYM/ **/*.dSYM/

decoder_test: CC += -D_TEST_
decoder_test: decoder_test.o decoder.o pcg/pcg_basic.o test/test.o

pcg/pgc_basic.o: pcg/pcg_basic.c pcg/pcg_basic.h

test: clean decoder_test
	./decoder_test 2> /dev/null

main: decoder.o
