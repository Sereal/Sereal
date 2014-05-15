# This is a terrible makefile. I know. Patches welcome.

CC=cc
LD=gcc
INCLUDE=-I. -I..
LIBS=-lm -lc

CFLAGS=-O2 -ggdb3 -Wall -Wextra -Wno-unused-function -fPIC $(INCLUDE)
LDFLAGS=$(LIBS)

OBJS = $(wildcard ../*.o)
SRC = $(wildcard *.c)
EXE = t_csrl_hash

all: $(EXE)

t_csrl_hash: t_csrl_hash.o $(OBJS)
	$(LD) $^ $(LDFLAGS) -o $@

.c.o:
	$(CC) $(CFLAGS) -c $<
 
test: $(EXE)
	@./t_csrl_hash

test_harness: $(EXE)
	@prove -e '' ./t_csrl_hash

clean:
	rm -f $(EXE) *.o

.SUFFIXES: .c

.PHONY: clean all
