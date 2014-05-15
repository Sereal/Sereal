# This is a terrible makefile. I know. Patches welcome.

CC=cc
LD=ld
INCLUDE=-I.

CFLAGS=-O2 -ggdb3 -Wall -Wextra -Wno-unused-function -fPIC $(INCLUDE)
LDFLAGS=-shared -fPIC

SUBDIRS = t
CLEANDIRS = $(SUBDIRS:%=clean-%)

SRC = $(wildcard *.c)
OBJS = $(SRC:.c=.o)

all: libsereal.so

libsereal.so: $(OBJS)
	$(LD) $^ $(LDFLAGS) -o $@

$(INSTALLDIRS): 
	$(MAKE) -C $(@:install-%=%) install

 
clean: $(CLEANDIRS)
	rm -f libsereal.so $(OBJS)
 
$(CLEANDIRS): 
	$(MAKE) -C $(@:clean-%=%) clean

subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

.c.o:
	$(CC) $(CFLAGS) -c $<

test: libsereal.so
	$(MAKE) -C t test

test_harness: libsereal.so
	$(MAKE) -C t test_harness
	

.SUFFIXES: .c

.PHONY: clean all test test_harness
.PHONY: subdirs $(SUBDIRS)
.PHONY: subdirs $(CLEANDIRS)
