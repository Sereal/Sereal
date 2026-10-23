#!perl
use strict;
use warnings;
use Test::More;
use Sereal::Decoder;

for my $version ( 1 .. 6 ) {
    my $header = ( $version < 3 ? '=srl' : "=\xf3rl" ) . chr($version) . "\0";
    my $dec    = Sereal::Decoder->new;
    for my $tag ( "\x2b", "\x2a" ) {

        # I32_MAX is representable as a count but needs more contents. The
        # next count must fail before allocation or count multiplication.
        eval { $dec->decode( $header . "\x28" . $tag . "\xff\xff\xff\xff\x07" ) };
        like(
            $@, qr/insufficient remaining tags/,
            "v$version maximum count is bounded by packet size"
        );
        eval { $dec->decode( $header . "\x28" . $tag . "\x80\x80\x80\x80\x08" ) };
        like( $@, qr/Count 2147483648 exceeds I32_MAX/, "v$version oversized count is rejected" );
    }
    is_deeply( $dec->decode( $header . "\x28\x2b\0" ),   [],  "v$version empty array" );
    is_deeply( $dec->decode( $header . "\x28\x2a\0" ),   {},  "v$version empty hash" );
    is_deeply( $dec->decode( $header . "\x28\x2b\1\7" ), [7], "v$version one-element array" );
    is_deeply(
        $dec->decode( $header . "\x28\x2a\1\x60\7" ), { '' => 7 },
        "v$version one-entry hash"
    );
}
done_testing;
