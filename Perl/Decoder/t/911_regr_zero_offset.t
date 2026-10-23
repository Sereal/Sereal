#!perl
use strict;
use warnings;
use Test::More;
use Scalar::Util qw(refaddr);
use Sereal::Decoder;

my $dec = Sereal::Decoder->new;

sub header {
    my ( $version, $encoding ) = @_;
    return ( $version < 3 ? '=srl' : "=\xF3rl" ) . chr( $version | ( $encoding || 0 ) ) . chr(0);
}

sub rejects_zero_offset {
    my ( $packet, $name ) = @_;
    eval { $dec->decode($packet); 1 };
    like(
        $@, qr/Offset 0 does not point into the document body/,
        "$name rejects zero before resolving the reference"
    );
}

my @cases = (
    [ 'COPY value',             "\x2F\x00" ],
    [ 'COPY hash key',          "\x51\x2F\x00\x01" ],
    [ 'COPY class name',        "\x2C\x2F\x00\x40" ],
    [ 'REFP',                   "\x29\x00" ],
    [ 'OBJECTV',                "\x2D\x00\x40" ],
    [ 'ALIAS',                  "\x41\x2E\x00" ],
    [ 'COPY frozen class name', "\x32\x2F\x00\x40", 2 ],
    [ 'OBJECTV_FREEZE',         "\x33\x00\x40",     2 ],
);

for my $version ( 1 .. 6 ) {
    for my $case (@cases) {
        next if ( $case->[2] || 1 ) > $version;
        for my $padding ( '', "\x00" x 16 ) {

            # Padding selects the unrolled varint reader; the short packet
            # selects the bounds-checked reader.
            rejects_zero_offset(
                header($version) . $case->[1] . $padding,
                "v$version $case->[0]" . ( length($padding) ? ' (fast reader)' : ' (safe reader)' )
            );
        }
    }

    # A failed decode must not spoil the next decode, and real backward
    # references must still work under both offset conventions.
    my $string_offset = $version == 1 ? 7 : 2;
    is_deeply(
        $dec->decode( header($version) . "\x42\x61k\x2F" . chr($string_offset) ),
        [ 'k', 'k' ], "v$version valid COPY survives rejection of zero offsets"
    );

    my $first_offset = $version == 1 ? 6 : 1;
    my $value        = $dec->decode( header($version) . "\xC1\x29" . chr($first_offset) );
    is(
        refaddr( ${ $value->[0] } ), refaddr($value),
        "v$version reference to the first body byte remains valid"
    );
    $value->[0] = undef;    # Break the deliberately constructed cycle.

    if ( $version >= 2 ) {
        my $packet = substr( header($version), 0, 5 ) . "\x03\x01\x2F\x00\x01";
        eval { $dec->decode_with_header($packet); 1 };
        like(
            $@, qr/Offset 0 does not point into the document body/,
            "v$version header user data also rejects offset zero"
        );
    }
}

# These streams all decompress to COPY(0). Their declared sizes are correct;
# the decoder must reject the reference instead of reading the uninitialized
# byte immediately before the decompressed body. Keep fixtures independent of
# Encoder and optional Perl compression modules.
my $snappy = pack( 'H*', '02042f00' );
my $zlib   = pack( 'H*', '789cd367000000600030' );
my $zstd   = pack( 'H*', '28b52ffd20021100002f00' );
rejects_zero_offset( header( 1, 0x10 ) . $snappy, 'v1 legacy Snappy' );
for my $version ( 2, 6 ) {
    rejects_zero_offset(
        header( $version, 0x20 ) . chr( length($snappy) ) . $snappy,
        "v$version incremental Snappy"
    );
}
for my $version ( 3, 6 ) {
    rejects_zero_offset(
        header( $version, 0x30 ) . chr(2) . chr( length($zlib) ) . $zlib,
        "v$version zlib"
    );
}
for my $version ( 4, 6 ) {
    rejects_zero_offset(
        header( $version, 0x40 ) . chr( length($zstd) ) . $zstd,
        "v$version zstd"
    );
}

done_testing;
