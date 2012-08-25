#!perl
use strict;
use warnings;
use Sereal::Decoder qw(decode_sereal);
use Sereal::Decoder::Constants qw(:all);
use Scalar::Util qw(weaken);
use Data::Dumper;

use constant FBIT => 128;

# These tests are extraordinarily basic, badly-done and really just
# for basic sanity testing during development.

use Test::More;

sub array {
  chr(SRL_HDR_ARRAY) . varint(0+@_) . join("", @_) . chr(SRL_HDR_TAIL)
}

sub array_fbit {
  chr(SRL_HDR_ARRAY+FBIT) . varint(0+@_) . join("", @_) . chr(SRL_HDR_TAIL)
}

sub varint {
  my $n = shift;
  my $out = '';
  while ($n >= 0x80) {
    $out .= chr( ($n & 0x7f) | 0x80 );
    $n >>= 7;
  }
  $out .= chr($n);
  return $out;
}


our $hdr;
our @basic_tests;
do 't/test_set.pl' or die $@;

run_tests("plain");
done_testing();
diag("All done folks!");

sub run_tests {
  my ($extra_name, $opt_hash) = @_;
  foreach my $bt (@basic_tests) {
    my ($in, $exp, $name) = @$bt;
    $exp = "$hdr$exp";
    my $out;
    my $ok= eval { $out = decode_sereal($exp); 1};
    ok($ok,"($extra_name) did not die: $name")
        or do {
            diag "$@"||"Zombie error";
            diag "input=", Data::Dumper::qquote($exp);
            next;
        };
    ok(defined($out)==defined($in), "($extra_name) defined: $name");
    is_deeply($out, $in,"($extra_name) is_deeply: $name");
  }
}

