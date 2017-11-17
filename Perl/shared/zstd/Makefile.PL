use 5.008;

use strict;
use warnings;
use Config;

my @zstd_files = (
    glob('common/*.c'),
    glob('compress/*.c'),
    glob('decompress/*.c')
);

open(my $fh, '>', "Makefile") or die $!;
print $fh q{
# ################################################################
# Copyright (c) 2016-present, Yann Collet, Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.
# ################################################################
#
# This file is modified version (unnecessary parts are removed) of original
# Makefile provided in zstd repository

CC       = } . $Config{cc} . q{
CPPFLAGS+= -I. -I./common -DXXH_NAMESPACE=ZSTD_
CFLAGS  ?= -O3
CFLAGS  += -fPIC -Wall -Wextra -Wcast-qual -Wcast-align -Wshadow -Wstrict-aliasing=1 \
           -Wswitch-enum -Wdeclaration-after-statement -Wstrict-prototypes -Wundef \
           -Wpointer-arith
CFLAGS  += $(MOREFLAGS)
FLAGS    = $(CPPFLAGS) $(CFLAGS)
AR       = ar
ARFLAGS  = rcs
RM       = rm -f

ZSTD_FILES := } . join(' ', @zstd_files) . q{
CPPFLAGS  += -DZSTD_LEGACY_SUPPORT=0

.PHONY: default all clean test test_dynamic

test:
test_dynamic:

default: lib

all: lib

libzstd.o: $(ZSTD_FILES)
	@echo compiling static library
	$(CC) $(FLAGS) -c $(ZSTD_FILES)
	$(AR) $(ARFLAGS) libzstd.o *.o

lib: libzstd.o

clean:
	$(RM) core *.o *.a *.gcda
	$(RM) decompress/*.o
	@echo Cleaning zstd library completed
};
