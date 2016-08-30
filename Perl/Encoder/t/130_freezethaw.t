#!perl
use strict;
use warnings;
# must be loaded before Sereal::TestSet
use File::Spec;
use Test::More;
use Data::Dumper;

use lib File::Spec->catdir(qw(t lib));
BEGIN {
  lib->import('lib')
    if !-d 't';
}

use Sereal::TestSet qw(:all);
use Sereal::Encoder qw(encode_sereal);
use Sereal::Encoder::Constants qw(:all);

my $ok = have_encoder_and_decoder();
if (not $ok) {
    plan skip_all => 'Did not find right version of decoder';
    exit 0;
}

my $thaw_called = 0;
my $freeze_called = 0;

package Foo;
sub new {
  my $class = shift;
  return bless({bar => 1, @_} => $class);
}

sub FREEZE {
  my ($self, $serializer) = @_;
  $freeze_called = $serializer eq 'Sereal' ? 1 : 0;
  return "frozen object", 12, [2];
}

sub THAW {
  my ($class, $serializer, @data) = @_;
  $thaw_called = $serializer eq 'Sereal' ? 1 : 0;
  Test::More::is_deeply(\@data, ["frozen object", 12, [2]], "Array of frozen values roundtrips");

  return Foo->new();
}

package Bar;
sub new {
  my $class = shift;
  return bless({bar => 1, @_} => $class);
}

sub FREEZE {
  my ($self, $serializer) = @_;
  return "frozen Bar";
}

package main;

my $enc = Sereal::Encoder->new({freeze_callbacks => 1});
my $srl = $enc->encode(Foo->new());
ok($freeze_called, "FREEZE was invoked");


# Simple round-trip test
my $dec = Sereal::Decoder->new;
my $obj = $dec->decode($srl);
ok(defined($obj));
isa_ok($obj, "Foo");
is(eval{$obj->{bar}}, 1) or diag Dumper($obj);

# Test referential integrity
my $foo = Foo->new;
my $data = [$foo, $foo];
$srl = $enc->encode($data);
ok($srl =~ /frozen object/);

my $out = $dec->decode($srl);
is_deeply($out, $data, "Roundtrip works");

cmp_ok($out->[0], "eq", $out->[1],
       "Referential integrity: multiple RVs do not turn into clones")
       or diag(Dumper($data,$out));

my $barobj = Bar->new;
$srl = $enc->encode($barobj);
ok(not(eval {$dec->decode($srl); 1}), "Decoding without THAW barfs");


# Multiple-object-same-class test from Christian Hansen

{
    package MyObject;

    sub from_num {
        my ($class, $num) = @_;
        return bless { num => $num }, $class;
    }

    sub num {
        my ($self) = @_;
        return $self->{num};
    }

    sub FREEZE {
        return $_[0]->num;
    }

    sub THAW {
        my ($class, undef, $num) = @_;
        return $class->from_num($num);
    }
}

my @objects = map { MyObject->from_num($_) } (10, 20, 30);
my $encoded = encode_sereal([ @objects ], { freeze_callbacks => 1 });
my $decoded = Sereal::Decoder::decode_sereal($encoded);

isa_ok($decoded, 'ARRAY');
is(scalar @$decoded, 3, 'array has three elements');
isa_ok($decoded->[0], 'MyObject', 'first element');
isa_ok($decoded->[1], 'MyObject', 'second element');
isa_ok($decoded->[2], 'MyObject', 'third element');

is($decoded->[0]->num, 10, 'first MyObject->num');
is($decoded->[1]->num, 20, 'second MyObject->num');
is($decoded->[2]->num, 30, 'third MyObject->num');
 

done_testing();
