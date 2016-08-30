#!perl
use strict;
use warnings;
use Test::More;
use File::Spec;
use lib File::Spec->catdir(qw(t lib));
BEGIN {
    lib->import('lib')
        if !-d 't';
}
use Sereal::TestSet qw(:all);
use Sereal::Encoder;

if (not have_encoder_and_decoder()) {
    plan skip_all => 'Did not find right version of decoder';
    exit 0;
}

# Encoder reentrancy test courtesy of Zefram

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
