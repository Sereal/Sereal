use strict;
use Test::More tests => 3;

use Sereal::Decoder;

# v6 zlib document declaring 1024 uncompressed bytes, with no compressed data.
my $doc = "=\xF3rl\x36\x00" . "\x80\x08" . "\x00";

eval { Sereal::Decoder->new->decode($doc) };
like( $@, qr/ZLIB decompression/, "default permits the declared size" );

eval { Sereal::Decoder->new( { max_uncompressed_size => undef } )->decode($doc) };
like( $@, qr/ZLIB decompression/, "undef keeps the unlimited default" );

eval { Sereal::Decoder->new( { max_uncompressed_size => 512 } )->decode($doc) };
like( $@, qr/larger than the allowed maximum/, "explicit limit rejects the declared size" );
