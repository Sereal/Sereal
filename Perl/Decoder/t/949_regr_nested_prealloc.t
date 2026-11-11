use strict;
use Test::More tests => 2;

use Sereal::Decoder;

# Ten nested arrays that each declare 1000 elements, followed by 1000 bytes.
my $doc = "=\xF3rl\x03\x00" . ( "\x28\x2b\xe8\x07" x 10 ) . ( "\x00" x 1000 );
eval { Sereal::Decoder->new->decode($doc) };
like( $@, qr/enclosing containers/, "nested arrays cannot each claim the same input" );

# [[1, 2], [3]]
my $ok = "=\xF3rl\x03\x00" . "\x28\x2b\x02" . "\x28\x2b\x02\x01\x02" . "\x28\x2b\x01\x03";
is_deeply( Sereal::Decoder->new->decode($ok), [ [ 1, 2 ], [3] ], "nested arrays that fit still decode" );
