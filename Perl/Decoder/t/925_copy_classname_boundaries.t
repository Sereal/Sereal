#!perl
use strict;
use warnings;
use Test::More;
use Sereal::Decoder;

for my $version ( 1 .. 6 ) {
    my $header = ( $version < 3 ? '=srl' : "=\xf3rl" ) . chr($version) . "\0";
    my $offset = $version == 1 ? 8 : 3;
    my $dec    = Sereal::Decoder->new;
    for my $source ( "\x68CopyName", "\x26\x08CopyName", "\x27\x08CopyName" ) {
        for my $tracked ( 0, 0x80 ) {
            my $encoded = $source;
            substr( $encoded, 0, 1 ) = chr( ord( substr( $encoded, 0, 1 ) ) | $tracked );
            my $packet =
                  $header . "\x42"
                . chr( 0x60 + length($encoded) )
                . $encoded
                . "\x2c\x2f"
                . chr($offset) . "\x40";
            my $out = $dec->decode($packet);
            is( ref( $out->[1] ), 'CopyName', "v$version COPY classname accepts tracked=$tracked "
                    . unpack( 'H*', substr( $source, 0, 1 ) ) );

            # The hash key path shares the same string-tag and varint logic.
            my $hash =
                  $header . "\x42"
                . chr( 0x60 + length($encoded) )
                . $encoded
                . "\x51\x2f"
                . chr($offset) . "\x07";
            is_deeply(
                $dec->decode($hash)->[1], { CopyName => 7 },
                "v$version copied hash key accepts tracked=$tracked"
            );
        }
    }
    for my $source ( "\x63", "\x26\x03", "\x27\x03\xc2", "\x27\x80" ) {
        my $packet =
              $header . "\x42"
            . chr( 0x60 + length($source) )
            . $source
            . "\x2c\x2f"
            . chr($offset) . "\x40";
        eval { $dec->decode($packet) };
        like(
            $@, qr/Corrupted COPY tag|Unexpected termination|varint/,
            "v$version overlapping classname is rejected before UTF8 validation"
        );
    }

    # A one-byte classname ends at COPY: the preceding OBJECT tag is its byte.
    my $packet = $header . "\x42\x61\x61\x2c\x2f" . chr($offset) . "\x40";
    is( ref( $dec->decode($packet)->[1] ), ',', "v$version classname may end exactly at COPY" );
}
done_testing;
