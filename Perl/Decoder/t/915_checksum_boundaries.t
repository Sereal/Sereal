#!perl
use strict;
use warnings;
use Test::More;
use Sereal::Decoder;

my $key    = 'Out damned spot!';
my $deadly = "\x3e\0";

# All fixture lengths fit in a single varint byte. Authenticate the deliberately
# incomplete payload itself, so rejection tests bounds rather than MAC failure.
sub envelope {
    my ( $type, $encoding, $payload ) = @_;
    die 'fixture too large' if length($payload) >= 128;
    my $prefix = "=\xf3rl" . chr( 0x86 | $encoding ) . "\0" . chr($type) . chr( $type ? 16 : 8 );
    my $tail   = chr( length($payload) ) . $payload;
    my $mac =
        $type
        ? Sereal::Decoder::siphash36( $prefix . $tail, $key )
        : Sereal::Decoder::siphash24( $prefix . $tail, $key );
    return $prefix . $mac . $tail;
}

# Complete streams with known output; no optional compression module needed.
my @streams = (
    [ 'legacy Snappy',      0x10, pack( 'H*', '0204616b' ),   pack( 'H*', '010061' ) ],
    [ 'incremental Snappy', 0x20, pack( 'H*', '040204616b' ), pack( 'H*', '03010061' ) ],
    [
        'zlib', 0x30, pack( 'H*', '020a789c4bcc0600012f00cd' ),
        pack( 'H*', '0109789c4b040000620062' )
    ],
    [
        'zstd', 0x40, pack( 'H*', '0b28b52ffd2002110000616b' ),
        pack( 'H*', '0a28b52ffd200109000061' )
    ],
);
for my $type ( 0, 1 ) {
    for my $policy ( 'die', 'ignore' ) {
        my $dec =
            Sereal::Decoder->new( { "checksum_v${type}_key" => $key, checksum_policy => $policy } );
        my $label = "checksum $type policy $policy";
        for my $case (
            [ 'empty body',    '' ],         [ 'short string',    "\x61" ],
            [ 'varint',        "\x20\x80" ], [ 'array item',      "\x41" ],
            [ 'binary length', "\x26\x80" ], [ 'extended tag ID', "\x3e" ],
            )
        {
            my $p = envelope( $type, 0, $case->[1] );
            eval { $dec->decode( $p . $deadly . ( "\0" x 16 ) ) };
            like(
                $@,
                qr/Unexpected termination|unexpected end of input|end of packet reached before varint parsed/,
                "$label raw $case->[0] stops at authenticated end"
            );
        }
        my $p = envelope( $type, 0, "\x07" );
        is( $dec->decode( $p . $deadly ), 7,          "$label ignores sentinel outside packet" );
        is( $dec->bytes_consumed,         length($p), "$label consumes only envelope" );
        is(
            $dec->decode_with_offset( $p . $p, length($p) ), 7,
            "$label concatenated envelopes work"
        );
        eval { $dec->decode( envelope( $type, 0, $deadly ) ) };
        like( $@, qr/Encountered DEADLY/, "$label signed DEADLY is fatal" );
        for my $stream (@streams) {
            my ( $name, $encoding, $complete, $incomplete_body ) = @$stream;
            $p = envelope( $type, $encoding, $complete );
            is( $dec->decode( $p . $deadly ), 'k',        "$label $name complete stream works" );
            is( $dec->bytes_consumed,         length($p), "$label $name consumes only envelope" );
            eval { $dec->decode( envelope( $type, $encoding, $incomplete_body ) . $deadly . "k" ) };
            like(
                $@, qr/Unexpected termination|unexpected end of input/,
                "$label $name parser stops at decompressed end"
            );
            my $truncated = substr( $complete, 0, -1 );
            my $last      = substr( $complete, -1 );
            eval { $dec->decode( envelope( $type, $encoding, $truncated ) . $last . $deadly ) };
            ok( $@ ne '', "$label $name cannot take compressed continuation outside envelope" );
            unlike(
                $@, qr/checksum verification failed|Encountered DEADLY/,
                "$label $name rejection is a compressed-input boundary failure"
            );
        }
    }
}
done_testing;
