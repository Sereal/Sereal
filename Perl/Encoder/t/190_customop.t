#!perl

use strict;
use warnings;

use Sereal::Encoder            qw(sereal_encode_with_object);
use Sereal::Encoder::Constants qw(SRL_PROTOCOL_VERSION);
use Test::More tests => 2;

my $srl_encoder = Sereal::Encoder->new();

my $enc = sereal_encode_with_object( $srl_encoder, [] );
is(
    $enc,
    "=\363rl" . chr(SRL_PROTOCOL_VERSION) . "\0\@",
    "check that sereal_encode_with_object works",
);
pass("did not segfault!")
