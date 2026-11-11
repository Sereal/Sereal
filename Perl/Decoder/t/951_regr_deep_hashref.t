use strict;
use Test::More;
# Native Windows has a small main thread stack. Cygwin also uses an
# executable-defined stack, so only its 9999-level case needs skipping.
plan $^O eq 'MSWin32'
    ? ( skip_all => 'main thread stack may be too small for deep hashes' )
    : ( tests => 2 );

use Sereal::Decoder;

SKIP: {
    skip 'Cygwin main thread stack may be too small for 9999 hashes', 1
        if $^O eq 'cygwin';
    # { k => { k => ... } }, 9999 hashes deep: within an explicit 10000 limit.
    my $doc = "=\xF3rl\x03\x00" . ( "\x51\x61k" x 9999 ) . "\x00";
    my $data = Sereal::Decoder->new({ max_recursion_depth => 10000 })->decode($doc);
    my $depth = 0;
    ( $data, $depth ) = ( $data->{k}, $depth + 1 ) while ref $data;
    is( $depth, 9999, "9999 nested hashes decode" );
}

my $default_limit = $^O eq 'cygwin' ? 4096 : 8192;
eval { Sereal::Decoder->new->decode( "=\xF3rl\x03\x00" . ( "\x51\x61k" x $default_limit ) . "\x00" ) };
like( $@, qr/Reached recursion limit \($default_limit\)/,
    "default limit rejects $default_limit levels" );
