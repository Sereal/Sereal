#!perl
use strict;
use warnings;
use File::Spec;
use lib File::Spec->catdir(qw(t lib));

BEGIN {
    lib->import('lib')
        if !-d 't';
}

use Sereal::Decoder;
use Sereal::Decoder::Constants qw(:all);
use Sereal::TestSet::Corpus qw(read_packet);
use Test::More;

sub MAX_UNCOMPRESSED_SIZE () { 10 * 1024 * 1024 }    # 10 MiB

my $corpus = File::Spec->catdir(qw(t data packet_corpus));
my $dec = Sereal::Decoder->new( { max_uncompressed_size => MAX_UNCOMPRESSED_SIZE } );
my %cases = (
    snappy => {
        offset       => 10,
        encoding     => SRL_PROTOCOL_ENCODING_SNAPPY_INCREMENTAL(),
        size_bytes   => [ 134, 128, 128, 5 ],
        forged_error => qr/^Sereal: Error: Snappy decompression of Sereal packet payload failed with error -3!/,
    },
    zlib => {
        offset       => 6,
        encoding     => SRL_PROTOCOL_ENCODING_ZLIB(),
        size_bytes   => [ 134, 128, 128, 5 ],
        forged_error => qr/^Sereal: Error: ZLIB decompression of Sereal packet payload failed with error -5!/,
    },
    zstd => {
        offset       => 25,
        encoding     => SRL_PROTOCOL_ENCODING_ZSTD(),
        size_bytes   => [ 129, 128, 128, 5 ],
        forged_error => qr/^The expected uncompressed size is larger than the allowed maximum size/,
    },
);

for my $name ( sort keys %cases ) {
    my $case = $cases{$name};
    my $packet = read_packet( $corpus, 'decoder-features', "zipbomb/$name" );
    eval { $dec->decode($packet) };
    like( $@, qr/^The expected uncompressed size is larger than the allowed maximum size/,
        "$name packet over the configured limit is rejected" );

    is( ord( substr( $packet, 4, 1 ) ), $case->{encoding} | SRL_PROTOCOL_VERSION(),
        "$name corpus packet uses the expected compression format" );
    is_deeply(
        [ unpack 'C4', substr( $packet, $case->{offset}, 4 ) ],
        $case->{size_bytes},
        "$name corpus packet declares the expected uncompressed size",
    );

    substr( $packet, $case->{offset} + 3, 1, "\0" );
    is( eval { $dec->decode($packet) }, undef, "$name packet with forged size fails" );
    like( $@, $case->{forged_error}, "$name forged size is rejected safely" );
}

{
    # Incremental Snappy packet declaring four uncompressed bytes, while the
    # block contains only ARRAYREF_3 and two integer items.
    my $short_output =
        "=\xF3rl" . chr(0x24) . chr(0) . chr(5) . chr(4) . chr(8) . chr(0x43) . chr(1) . chr(1);
    my $decoded_ok = eval { $dec->decode($short_output); 1 };
    ok( !$decoded_ok, 'Snappy output shorter than declared is rejected' );
    like( $@, qr/Snappy uncompressed size mismatch/, 'Snappy size mismatch is reported' );
}

{
    my $short_output = read_packet( $corpus, 'decoder-features', 'zipbomb/zlib_short' );
    substr( $short_output, 6, 1, chr( ord( substr( $short_output, 6, 1 ) ) + 1 ) );
    my $decoded_ok = eval { $dec->decode($short_output); 1 };
    ok( !$decoded_ok, 'zlib output shorter than declared is rejected' );
    like( $@, qr/ZLIB uncompressed size mismatch/, 'zlib size mismatch is reported' );
}

done_testing;
