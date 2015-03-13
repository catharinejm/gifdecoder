TARGETS = decoder decoder_test

CC = gcc -ggdb

clean:
	rm -f **/*.o $(TARGETS)

decoder_test: decoder_test.o pcg/pcg_basic.o

decoder_test.o: decoder_test.c
pcg/pgc_basic.o: pcg/pcg_basic.c pcg/pcg_basic.h

test: clean decoder_test
	./decoder_test 2> /dev/null

