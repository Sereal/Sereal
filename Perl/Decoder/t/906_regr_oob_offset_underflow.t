#!perl
use strict;
use warnings;
use File::Spec;
use lib File::Spec->catdir(qw(t lib));

BEGIN {
    lib->import('lib')
        if !-d 't';
}

use Config;
use Test::More;
use Sereal::Decoder qw(decode_sereal);

plan skip_all => 'Requires 64-bit UVs' if $Config{uvsize} != 8;
plan tests    => 2;

# A protocol v4 document containing an array with UNDEF followed by COPY.
# The COPY offset is 2^64 - 16, encoded as a legal 10-byte varint. Before
# the offset check was changed to compare integers, body_pos + offset
# wrapped to 11 bytes before the input allocation and the COPY reader
# dereferenced that address.
my $header          = "=\xF3rl" . chr(0x04) . chr(0x00);
my $uv_max_minus_15 = chr(0xF0) . ( chr(0xFF) x 8 ) . chr(0x01);
my $body            = chr(0x28) . chr(0x2B) . chr(0x02)            # REFN ARRAY count=2
    . chr(0x25)                                                    # UNDEF
    . chr(0x2F) . $uv_max_minus_15;                                # COPY with underflowing offset
my $packet = $header . $body;

my $ok = eval { decode_sereal($packet); 1 };
ok( !$ok, 'underflowing COPY offset is rejected' );
like(
    $@, qr/points past current position/i,
    'decoder reports the invalid back-reference offset'
);
