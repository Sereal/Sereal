use strict;
use Test::More tests => 2;

use Sereal::Decoder;

# v6 zlib document declaring 2**30 uncompressed bytes, with no compressed data.
my $doc = "=\xF3rl\x36\x00" . "\x80\x80\x80\x80\x04" . "\x00";

eval { Sereal::Decoder->new->decode($doc) };
like( $@, qr/larger than the allowed maximum/, "default limit rejects a 1 GiB body" );

eval { Sereal::Decoder->new( { max_uncompressed_size => undef } )->decode($doc) };
like( $@, qr/larger than the allowed maximum/, "undef keeps the default" );
