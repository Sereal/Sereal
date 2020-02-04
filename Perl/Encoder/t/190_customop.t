#!perl

use strict;
use warnings;

use Sereal::Encoder qw(sereal_encode_with_object);
use Test::More tests => 2;

my $srl_encoder= Sereal::Encoder->new();

my $enc= sereal_encode_with_object($srl_encoder,[]);
is($enc,"=\363rl\4\0\@", "check that sereal_encode_with_object works");
pass("did not segfault!")
