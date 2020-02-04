#!perl

use strict;
use warnings;

use Sereal::Decoder qw(sereal_decode_with_object scalar_looks_like_sereal);
use Test::More tests => 3;
use Data::Dumper;

my $srl_decoder= Sereal::Decoder->new();
my $empty_array_as_sereal= "=\363rl\4\0\@";

my $dec= sereal_decode_with_object($srl_decoder, $empty_array_as_sereal);
is( Dumper($dec), Dumper([]), "check that sereal_decode_with_object works as expected");
is( scalar_looks_like_sereal($empty_array_as_sereal), 4, #expect version 4
    "check that scalar_looks_like_sereal works as expected");
pass("did not segfault!");
