#!perl
use strict;
use warnings;
use Test::More;

# Encoder reentrancy test courtesy of Zefram

use Sereal::Encoder;
use Sereal::Decoder;

my $enc = Sereal::Encoder->new({freeze_callbacks=>1});

package Foo;
sub FREEZE { $enc->encode($_[0]->{a}) }
sub THAW {
  my $class = shift;
  return bless(
    {a => Sereal::Decoder->new->decode($_[1])}
    => $class
  );
}

package main;

my $data = bless({a=>42},"Foo");
my $a = $enc->encode($data);
my $output;
my $err;
eval {
  $output = Sereal::Decoder->new->decode($a);
  1
}
or do {
  $err = $@ || "Zombie Error";
};

ok(!$err, "Decoding did not barf")
  or diag("Decoding barfed with '$err'");

is_deeply($output,
          $data,
          "Decoded result is correct");

done_testing();
