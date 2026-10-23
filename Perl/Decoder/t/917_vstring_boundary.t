#!perl
use strict;
use warnings;
use Test::More;
use Scalar::Util qw(reftype);
use Sereal::Decoder;
my $header = "=\xf3rl\x06\0";
my $dec    = Sereal::Decoder->new;
for my $spelling ( 'v1', 'v12', 'v1.2' ) {
    my $body     = "\x3e\x26" . chr( length($spelling) ) . $spelling;
    my $expected = $spelling;
    $expected =~ s/^v//;
    for my $tail ( '', 'A', '=>', '.3' ) {
        my $value = eval { $dec->decode( $header . $body . $tail ) };
        is( $@,                             '', "$spelling is independent of trailing '$tail'" );
        is( sprintf( '%vd', $value // '' ), $expected, 'declared spelling alone determines value' );
        is( reftype( \$value ),             'VSTRING', 'v-string magic is preserved' );
    }

    # SHORT_BINARY(1) is byte 0x61, ASCII 'a': the Perl scanner must not
    # interpret this next tag as part of the v-string spelling.
    my $array = eval { $dec->decode( $header . "\x42" . $body . "\x61k" ) };
    is( $@, '', "$spelling followed by another array element decodes" );
    is( sprintf( '%vd', $array->[0] // '' ), $expected, 'array v-string value is correct' );
    is( $array->[1],                         'k',       'next element is intact' );
}
done_testing;
