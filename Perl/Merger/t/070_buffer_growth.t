#!perl
use strict;
use warnings;
use Test::More;

# must be loaded before Sereal::TestSet
use Sereal::Merger qw(:all);
use Sereal::Merger::Constants qw(:all);
use Sereal::Decoder qw(decode_sereal);
use File::Spec;

use lib File::Spec->catdir(qw(t lib));
BEGIN {
  lib->import('lib')
    if !-d 't';
}

use Sereal::TestSet qw(varint);

# append() reserves the size of the input document in the output buffer,
# but the output can be larger than the input: a COPY offset that fits in
# one byte in the input is re-emitted with the offset of its target in the
# output, which takes four bytes once the earlier output is over 2 MiB.
# EXTEND items must not write past the reservation that this used up.

my $header = "=\xF3rl\x06\x00";

# first document: one 2.2 MB string
my $big  = 2_200_000;
my $doc1 = $header . "\x26" . varint($big) . ("A" x $big);

# second document: REFN ARRAY [ "x", POS_0 x $pad, ( COPY of "x" x $copies,
# EXTEND v-string "" x $exts ) x $blocks ]
my ($pad, $copies, $exts, $blocks) = (80, 20, 5, 40_000);
my $count = 1 + $pad + $blocks * ($copies + $exts);
my $array = "\x28\x2B" . varint($count);
my $copy  = "\x2F" . varint(1 + length $array); # body offset of "x"
my $ext   = "\x3E\x26\x00";                      # EXTEND, VSTRING, length 0
my $doc2  = $header . $array . "\x61x" . ("\x00" x $pad)
          . ((($copy x $copies) . ($ext x $exts)) x $blocks);

my $mrg = Sereal::Merger->new();
$mrg->append($doc1);
$mrg->append($doc2);
my $out = eval { $mrg->finish() };
ok(defined $out, "documents are merged") or diag $@;

# the Decoder refuses empty v-strings, so compare the bytes instead:
# REFN ARRAY, the element count in five bytes of padding, then both bodies
# with the COPY offsets rewritten to point at "x" in the output
my $top     = "\x28\x2B" . "\x02" . ("\x3F" x 4);
my $body1   = substr($doc1, length $header);
my $x_ofs   = 1 + length($top) + length($body1) + length($array);
my $out_copy = "\x2F" . varint($x_ofs);
my $expected = $header . $top . $body1 . $array . "\x61x" . ("\x00" x $pad)
             . ((($out_copy x $copies) . ($ext x $exts)) x $blocks);
is(length($out_copy), 5, "output COPY offsets are wider than the input ones");
is(length($out), length($expected), "output has the expected length");
ok($out eq $expected, "output has the expected bytes");

done_testing;
