use strict;
use Test::More;
# Both native Windows and Cygwin use an executable-defined main thread stack.
# This deliberately deep test needs more stack than their Perl builds may have.
plan $^O eq 'MSWin32' || $^O eq 'cygwin'
    ? ( skip_all => 'main thread stack may be too small for 9999 hashes' )
    : ( tests => 2 );

use Sereal::Decoder;

# { k => { k => ... } }, 9999 hashes deep: within an explicit 10000 limit.
my $doc = "=\xF3rl\x03\x00" . ( "\x51\x61k" x 9999 ) . "\x00";
my $data = Sereal::Decoder->new({ max_recursion_depth => 10000 })->decode($doc);
my $depth = 0;
( $data, $depth ) = ( $data->{k}, $depth + 1 ) while ref $data;
is( $depth, 9999, "9999 nested hashes decode" );

eval { Sereal::Decoder->new->decode( "=\xF3rl\x03\x00" . ( "\x51\x61k" x 8192 ) . "\x00" ) };
like( $@, qr/recursion limit/, "default limit rejects 8192 levels" );
