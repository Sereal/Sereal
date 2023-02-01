use 5.008;

use strict;
use warnings;
use Config;
use Data::Dumper;

print "# Running zstd/Makefile.PL\n";
my @c_files= (
    glob('common/*.c'),
    glob('compress/*.c'),
    glob('decompress/*.c') );
my @S_files= (
    glob('common/*.S'),
    glob('compress/*.S'),
    glob('decompress/*.S') );

my @source_files= @c_files;
my $disable_asm= "";
if ($ENV{NO_ASM} || $ENV{ZSTD_DISABLE_ASM}) {
    $disable_asm= "CPPFLAGS += -DZSTD_DISABLE_ASM\n";
} else {
    push @source_files, @S_files;
}
my $is_gcc= ($Config{cc}=~/gcc/ || $Config{gccversion});
my @targets;
my @o_files;
foreach my $source_file (@source_files) {
    (my $o_file= $source_file)=~s/\.[cS]$/.o/;
    $o_file=~s!^.*?/!!;
    my $extra_flags= "";
    if ($source_file =~ /zstd_decompress_block/ && $is_gcc) {
        $extra_flags= "-fno-tree-vectorize";
    }
    push @targets, sprintf "%s : %s\n\t\$(CC) \$(CPPFLAGS) \$(CFLAGS) %s -c %s\n",
        $o_file, $source_file, $extra_flags, $source_file;
    push @o_files, $o_file;
}


open( my $fh, '>', "Makefile" ) or die $!;
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
# This file is modified version (unnecessary parts are removed) of
# original Makefile provided in zstd repository. The message above
# refers to the contents of the original repository which can be
# found at the following URL:
#
# https://github.com/facebook/zstd/blob/dev/lib/Makefile
#
# Used under the GPL V2. Gracious thanks to Facebook and Yann Collet
# for making this available for public use.

CC       = } . $Config{cc} . q{
CPPFLAGS+= -I. -I./common -DXXH_NAMESPACE=ZSTD_
CFLAGS  ?= -O3
CFLAGS  += -fPIC -Wall -Wextra -Wcast-qual -Wcast-align -Wshadow -Wstrict-aliasing=1 \
           -Wswitch-enum -Wdeclaration-after-statement -Wstrict-prototypes -Wundef \
           -Wpointer-arith
CFLAGS  += $(MOREFLAGS)
AR       = ar
ARFLAGS  = rcs
RM       = rm -f

ZSTD_FILES := } . join( ' ', @o_files ) . q{

CPPFLAGS += -DZSTD_LEGACY_SUPPORT=0

} . $disable_asm . q{

.PHONY: default all clean test test_dynamic lib

all: lib

test:
test_dynamic:

default: lib

} . join("\n", @targets) . q{

libzstd.o: $(ZSTD_FILES)
	@echo compiling static library
	$(AR) $(ARFLAGS) libzstd.o *.o

lib: libzstd.o

clean:
	$(RM) core *.o *.a *.gcda
	@echo Cleaning zstd library completed
};
