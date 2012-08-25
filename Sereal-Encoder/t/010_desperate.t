#!perl
use strict;
use warnings;
use Sereal::Encoder qw(encode_sereal);
use Sereal::Encoder::Constants qw(:all);
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
do "t/test_set.pl" or die $@;

run_tests("plain");
run_tests("no_shared_hk", {no_shared_hashkeys => 1});
done_testing();


sub run_tests {
  my ($extra_name, $opt_hash) = @_;
  foreach my $bt (@basic_tests) {
    my ($in, $exp, $name) = @$bt;

    #next unless $name=~/PAD/;

    $exp = "$hdr$exp";
    my $out = encode_sereal($bt->[0], $opt_hash ? ($opt_hash) : ()); # must use bt here or we get a copy
    ok(defined $out, "($extra_name) defined: $name");
    #is(length($out), length($exp));
    is(Data::Dumper::qquote($out), Data::Dumper::qquote($exp), "($extra_name) correct: $name")
      or do {
        if ($ENV{DEBUG_SEREAL}) {
          print STDERR "\nEXPECTED:\n";
          hobodecode($exp);
          print STDERR "\nGOT:\n";
          hobodecode($out);
          print STDERR "\n";
        }
      };
  }
}

sub hobodecode {
  open my $fh, "| $^X -Mblib author_tools/hobodecoder.pl -e" or die $!;
  print $fh @_;
  close $fh;
}
