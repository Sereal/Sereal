#!perl
use strict;
use warnings;
use Sereal::Encoder qw(encode_sereal);

# These tests are extraordinarily basic, badly-done and really just
# for basic sanity testing during development.

use Test::More tests => 3;

my $hdr = "srl" . chr(0b0000_0001); # magic string + proto version

my $out = encode_sereal(1);
ok(defined $out);
is($out, $hdr . chr(0b0000_0001));

pass("Alive");

