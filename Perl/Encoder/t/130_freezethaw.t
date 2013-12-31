#!perl
use strict;
use warnings;
# most be loaded before Sereal::TestSet
use Sereal::Encoder qw(encode_sereal);
use Sereal::Encoder::Constants qw(:all);
use File::Spec;
use Test::More;

use lib File::Spec->catdir(qw(t lib));
BEGIN {
  lib->import('lib')
    if !-d 't';
}

use Sereal::TestSet qw(:all);

my $thaw_called = 0;
my $freeze_called = 0;

package Foo;
sub new {
  my $class = shift;
  return bless({bar => 1, @_} => $class);
}

sub FREEZE {
  my ($self, $serializer) = @_;
  $freeze_called = 1;
  return "frozen object";
}

sub THAW {
  my ($class, $serializer, $data) = @_;
  $thaw_called = 1;

  return Foo->new(data => $data);
}

package main;

my $enc = Sereal::Encoder->new({freeze_callbacks => 1});
my $srl = $enc->encode(Foo->new());
ok($freeze_called, "FREEZE was invoked");


my $run_decoder_tests = have_encoder_and_decoder();
if ($run_decoder_tests) {
  # Simple round-trip test
  my $dec = Sereal::Decoder->new;
  my $obj = $dec->decode($srl);
  ok(defined($obj));
  isa_ok($obj, "Foo");
  is($obj->{data}, "frozen object");
  is($obj->{bar}, 1);

  # Test referential integrity
  my $foo = Foo->new;
  my $data = [$foo, $foo];
  my $srl = $enc->encode($data);
  my $out = $dec->decode($enc->encode($data));
  cmp_ok($out->[0], "eq", $out->[1],
         "Referential integrity: multiple RVs do not turn into clones");
}

done_testing();
