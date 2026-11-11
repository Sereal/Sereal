use strict;
use Test::More;
plan $^O eq 'MSWin32' ? ( skip_all => 'main thread stack is 1-2 MiB' ) : ( tests => 2 );

use Sereal::Decoder;

# { k => { k => ... } }, 9999 hashes deep: within max_recursion_depth.
my $doc = "=\xF3rl\x03\x00" . ( "\x51\x61k" x 9999 ) . "\x00";
my $data = Sereal::Decoder->new->decode($doc);
my $depth = 0;
( $data, $depth ) = ( $data->{k}, $depth + 1 ) while ref $data;
is( $depth, 9999, "9999 nested hashes decode" );

eval { Sereal::Decoder->new->decode( "=\xF3rl\x03\x00" . ( "\x51\x61k" x 10000 ) . "\x00" ) };
like( $@, qr/recursion limit/, "one more level reaches the limit" );
