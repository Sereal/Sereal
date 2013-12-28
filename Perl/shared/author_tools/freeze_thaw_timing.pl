#!/usr/bin/env perl
use strict;
use warnings;
use Sereal::Encoder;
use Sereal::Decoder;

use Benchmark::Dumb qw(cmpthese);

my $enc_nocb = Sereal::Encoder->new();
my $enc_cb = Sereal::Encoder->new({freeze_callbacks => 1});
my $dec = Sereal::Decoder->new();

package Foo;
sub new {
  my $class = shift;
  return bless({@_} => $class);
}

sub FREEZE {
  my ($self, $serializer) = @_;
  return $self->{name}; # performance
}

sub THAW {
  my ($class, $serializer, $data) = @_;
  return Foo->new(name => $data);
}

package main;

my $data = Foo->new(name => "blargh");
my $data_big = [];
for (1..100) {
  push @$data_big, Foo->new(name => "blargh");
}
my $data_big_nocb = [];
for (1..100) {
  push @$data_big_nocb, bless({name => "blargh"} => "Bar");
}

my $frozen_nocb = $enc_nocb->encode($data);
my $frozen_cb = $enc_cb->encode($data);

my $frozen_big_nocb = $enc_nocb->encode($data_big);
my $frozen_big_cb = $enc_cb->encode($data_big);

my $timing = "1000.01";

print "Comparing small serialization with/out callbacks...\n";
cmpthese(
  $timing,
  {
    cb    => sub {$enc_cb->encode($data)},
    no_cb => sub {$enc_nocb->encode($data)},
  }
);

print "Comparing big serialization with/out callbacks...\n";
cmpthese(
  $timing,
  {
    cb             => sub {$enc_cb->encode($data_big)},
    no_cb          => sub {$enc_nocb->encode($data_big)},
    cb_nocbdata    => sub {$enc_cb->encode($data_big_nocb)},
    no_cb_nocbdata => sub {$enc_nocb->encode($data_big_nocb)},
  }
);


print "Comparing small deserialization with/out callbacks...\n";
cmpthese(
  $timing,
  {
    cb    => sub {$dec->decode($frozen_cb)},
    no_cb => sub {$dec->decode($frozen_nocb)},
  }
);

print "Comparing big deserialization with/out callbacks...\n";
cmpthese(
  $timing,
  {
    cb    => sub {$dec->decode($frozen_big_cb)},
    no_cb => sub {$dec->decode($frozen_big_nocb)},
  }
);
