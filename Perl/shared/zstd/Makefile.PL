use 5.008;

use strict;
use warnings;

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

# Version numbers
LIBVER_MAJOR_SCRIPT:=`sed -n '/define ZSTD_VERSION_MAJOR/s/.*[[:blank:]]\([0-9][0-9]*\).*/\1/p' < ./zstd.h`
LIBVER_MINOR_SCRIPT:=`sed -n '/define ZSTD_VERSION_MINOR/s/.*[[:blank:]]\([0-9][0-9]*\).*/\1/p' < ./zstd.h`
LIBVER_PATCH_SCRIPT:=`sed -n '/define ZSTD_VERSION_RELEASE/s/.*[[:blank:]]\([0-9][0-9]*\).*/\1/p' < ./zstd.h`
LIBVER_SCRIPT:= $(LIBVER_MAJOR_SCRIPT).$(LIBVER_MINOR_SCRIPT).$(LIBVER_PATCH_SCRIPT)
LIBVER_MAJOR := $(shell echo $(LIBVER_MAJOR_SCRIPT))
LIBVER_MINOR := $(shell echo $(LIBVER_MINOR_SCRIPT))
LIBVER_PATCH := $(shell echo $(LIBVER_PATCH_SCRIPT))
LIBVER := $(shell echo $(LIBVER_SCRIPT))
VERSION?= $(LIBVER)

CPPFLAGS+= -I. -I./common -DXXH_NAMESPACE=ZSTD_
CFLAGS  ?= -O3 -fPIC
CFLAGS  += -Wall -Wextra -Wcast-qual -Wcast-align -Wshadow -Wstrict-aliasing=1 \
           -Wswitch-enum -Wdeclaration-after-statement -Wstrict-prototypes -Wundef \
           -Wpointer-arith
CFLAGS  += $(MOREFLAGS)
FLAGS    = $(CPPFLAGS) $(CFLAGS)

ZSTD_FILES := $(wildcard common/*.c compress/*.c decompress/*.c)
CPPFLAGS  += -DZSTD_LEGACY_SUPPORT=0

.PHONY: default all clean test test_dynamic

test:
test_dynamic:

default: lib

all: lib

libzstd.o: ARFLAGS = rcs
libzstd.o: $(ZSTD_FILES)
	@echo compiling static library
	$(CC) $(FLAGS) -c $^
	$(AR) $(ARFLAGS) $@ *.o

lib: libzstd.o

clean:
	$(RM) core *.o *.a *.gcda
	$(RM) decompress/*.o
	@echo Cleaning zstd library completed
};
