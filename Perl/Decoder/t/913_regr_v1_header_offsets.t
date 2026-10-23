#!perl
use strict;
use warnings;
use Test::More;
use Sereal::Decoder;

my $dec = Sereal::Decoder->new;
for my $suffix ( '', "\x61k" ) {
    my $header = '=srl' . "\x01" . chr( length($suffix) ) . $suffix;
    for my $offset ( 1 .. length($header) - 1 ) {
        for my $case (
            [ 'COPY value', "\x2f",     '' ],
            [ 'COPY key',   "\x51\x2f", "\x01" ],
            [ 'COPY class', "\x2c\x2f", "\x40" ],
            [ 'REFP',       "\x29",     '' ],
            [ 'OBJECTV',    "\x2d",     "\x40" ],
            [ 'ALIAS',      "\x41\x2e", '' ],
            )
        {
            for my $padding ( '', "\0" x 16 ) {
                eval { $dec->decode( $header . $case->[1] . chr($offset) . $case->[2] . $padding ) };
                like(
                    $@, qr/offset points before the document body/,
                    "$case->[0] rejects v1 header offset $offset (suffix "
                        . length($suffix)
                        . ', padding '
                        . length($padding) . ')'
                );
            }
        }
    }
    is_deeply(
        $dec->decode( $header . "\x42\x61k\x2f" . chr( length($header) + 1 ) ),
        [ 'k', 'k' ], 'valid v1 body reference after optional header remains usable'
    );
}

# A literal Snappy block decoding to COPY(5). The decompressed buffer reserves
# header space without initializing it; none of that space is a legal target.
eval { $dec->decode( '=srl' . "\x11\0" . pack( 'H*', '02042f05' ) ) };
like( $@, qr/offset points before the document body/, 'v1 Snappy rejects header reference' );
done_testing;
