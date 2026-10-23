#!perl
use strict;
use warnings;
use Test::More;
use Sereal::Decoder;

my $dec = Sereal::Decoder->new;
for my $version ( 1 .. 6 ) {
    my $header = ( $version < 3 ? '=srl' : "=\xF3rl" ) . chr($version) . "\0";
    my $target = $version == 1 ? 8 : 3;

    # The first string contains a tag byte. COPY points at that byte, whose
    # value would then consume COPY's own tag/offset and possibly later data.
    for my $case (
        [ 'SHORT_BINARY ends in COPY tag',     0x61, '' ],
        [ 'SHORT_BINARY ends in COPY offset',  0x62, '' ],
        [ 'SHORT_BINARY extends beyond COPY',  0x63, "\0" ],
        [ 'BINARY length comes from COPY',     0x26, "\0" x 64 ],
        [ 'STR_UTF8 length comes from COPY',   0x27, "\0" x 64 ],
        [ 'VARINT reads COPY as its argument', 0x20, '' ],
        [ 'ZIGZAG reads COPY as its argument', 0x21, '' ],
        [ 'FLOAT extends beyond COPY',         0x22, "\0" x 4 ],
        [ 'DOUBLE extends beyond COPY',        0x23, "\0" x 8 ],
        )
    {
        my ( $name, $tag, $trailer ) = @$case;
        my $packet = $header . "\x42\x61" . chr($tag) . "\x2F" . chr($target) . $trailer;
        eval { $dec->decode($packet); 1 };
        like(
            $@,
            qr/Unexpected termination|unexpected end of input|end of packet reached before varint parsed/,
            "v$version $name is rejected at the COPY boundary"
        );
    }

    # A zero-length string at the planted tag ends exactly where COPY starts.
    is_deeply(
        $dec->decode( $header . "\x42\x61\x60\x2F" . chr($target) ),
        [ "\x60", '' ], "v$version copied value may end exactly at COPY"
    );

    my $earlier = $version == 1 ? 7 : 2;
    my $packet  = $header . "\x43\x61k\x2F" . chr($earlier) . "\x07";
    is_deeply(
        $dec->decode($packet), [ 'k', 'k', 7 ],
        "v$version restores the packet boundary after a valid string COPY"
    );
    is( $dec->bytes_consumed, length($packet), "v$version consumes the following item" );
    is_deeply(
        $dec->decode( $header . "\x43\x41\x07\x2F" . chr($earlier) . "\x08" ),
        [ [7], [7], 8 ], "v$version can still COPY a complete container"
    );
}
done_testing;
