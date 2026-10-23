#!perl
use strict;
use warnings;
use Test::More;
use Sereal::Decoder;

my $dec = Sereal::Decoder->new;
for my $version ( 2 .. 6 ) {
    my $prefix = ( $version == 2 ? '=srl' : "=\xf3rl" ) . chr($version);
    for my $suffix ( "\0", "\0ignored" ) {
        my $header = $prefix . chr( length($suffix) ) . $suffix;
        my $result = eval { $dec->decode_only_header($header) };
        is( $@, '', "v$version complete non-user suffix at EOF is accepted" );
        ok( !defined($result), "v$version no user metadata returned" );
        eval { $dec->decode($header) };
        like(
            $@, qr/unexpected end of input|Unexpected termination/,
            "v$version body still required for full decode"
        );
    }
    is(
        $dec->decode_only_header( $prefix . "\x02\x01\x07" ), 7,
        "v$version metadata may end exactly at EOF"
    );
    eval { $dec->decode_with_header( $prefix . "\x02\x01\x61k" ) };
    like( $@, qr/Unexpected termination/, "v$version metadata cannot borrow a byte from the body" );
    eval { $dec->decode_only_header( $prefix . "\x02\0" ) };
    like( $@, qr/Unexpected termination/, "v$version declared suffix must fit input" );
}
done_testing;
