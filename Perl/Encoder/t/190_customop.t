#!perl

use strict;
use warnings;

use Sereal::Encoder            qw(sereal_encode_with_object);
use Sereal::Encoder::Constants qw(SRL_PROTOCOL_VERSION);
use Test::More;

my $srl_encoder = Sereal::Encoder->new();

my $enc = sereal_encode_with_object( $srl_encoder, [] );
is(
    $enc,
    "=\363rl" . chr(SRL_PROTOCOL_VERSION) . "\0\@",
    "check that sereal_encode_with_object works",
);
pass("did not segfault!");

my @warnings;
{
    local $SIG{__WARN__} = sub { push @warnings, @_ };
    my @values = ( 'before', sereal_encode_with_object( $srl_encoder, [] ), 'after' );
    is_deeply( \@values, [ 'before', $enc, 'after' ], 'custom op preserves adjacent stack values' );
    sereal_encode_with_object( $srl_encoder, [] );
    sereal_encode_with_object( $srl_encoder, [], 'header' );
}
is_deeply( \@warnings, [], 'custom op balances stack references' );

done_testing;
