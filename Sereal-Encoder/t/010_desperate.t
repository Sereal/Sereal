#!perl
use strict;
use warnings;
use Sereal::Encoder qw(encode_sereal);
use Sereal::Constants qw(:all);

# These tests are extraordinarily basic, badly-done and really just
# for basic sanity testing during development.

use Test::More;

my $hdr = SRL_MAGIC_STRING . chr(SRL_PROTOCOL_VERSION) . chr(0);

my @basic_tests = (
  # warning: this hardcodes the POS/NEG headers
  [1, chr(0b0000_0001), "encode 1"],
  [0, chr(0b0000_0000), "encode 0"],
  [-1, chr(0b0001_0000), "encode -1"],
);

foreach my $bt (@basic_tests) {
  my ($in, $exp, $name) = @$bt;
  $exp = "$hdr$exp";
  my $out = encode_sereal($in);
  ok(defined $out, "defined: $name");
  #is(length($out), length($exp));
  is($out, $exp, "correct: $name");
}

done_testing();

