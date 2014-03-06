#!perl
use strict;
use warnings;
use Test::More;

# Encoder reentrancy test courtesy of Zefram

use Sereal::Encoder;
use Sereal::Decoder;

my $enc = Sereal::Encoder->new({freeze_callbacks=>1});
package Foo {
  sub FREEZE { $enc->encode({%{$_[0]}}) }
  sub THAW { bless(Sereal::Decoder->new->decode($_[2]), $_[0]) }
}

my $a = $enc->encode(bless({a=>1},"Foo"));
my $b;
my $err;
eval {
  $b = Sereal::Decoder->new->decode($a);
  1
}
or do {
  $err = $@ || "Zombie Error";
};

ok(!$err, "Decoding did not barf")
  or diag("Decoding barfed with '$err'");

is_deeply($b, bless({a => 1} => "Foo"), "Decoded result is correct");

done_testing();
