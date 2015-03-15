TARGETS = main
SOURCES = $(wildcard *.c)
OBJS = $(SOURCES:.c=.o)

CC = gcc -ggdb

.PHONY: all test clean

all: main

clean:
	rm -f *.o $(TARGETS)
	rm -rf *.dSYM/
	$(MAKE) -C test clean

pcg/pgc_basic.o: pcg/pcg_basic.c pcg/pcg_basic.h

main: $(OBJS)

test:
	$(MAKE) -C test
