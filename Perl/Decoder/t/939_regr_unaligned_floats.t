#!perl
use strict;
use warnings;
use File::Spec;

use lib File::Spec->catdir(qw(t lib));
BEGIN { lib->import('lib') if !-d 't' }

use Test::More;
use Sereal::TestSet qw(Header);
use Sereal::Decoder qw(decode_sereal);
use Sereal::Decoder::Constants qw(:all);

for my $case (
    [ 'FLOAT',       SRL_HDR_FLOAT,       'f' ],
    [ 'DOUBLE',      SRL_HDR_DOUBLE,      'd' ],
    [ 'LONG_DOUBLE', SRL_HDR_LONG_DOUBLE, 'D' ],
) {
    my ( $name, $tag, $format ) = @$case;
    SKIP: {
        skip "$name packing is unavailable on this Perl", 6 * 16 * 3
            unless eval { length( pack( $format, 0 ) ) };

        for my $version ( 1 .. 6 ) {
            for my $padding ( 0 .. 15 ) {
                for my $value ( 0, -1.5, 0.1 ) {
                    my $payload = pack( $format, $value );
                    # PAD shifts the floating-point payload through every
                    # alignment without changing the decoded value.
                    my $packet = Header($version)
                        . ( chr(SRL_HDR_PAD) x $padding )
                        . chr($tag) . $payload;
                    is(
                        decode_sereal($packet), unpack( $format, $payload ),
                        "v$version $name value $value after $padding PAD bytes"
                    );
                }
            }
        }
    }
}

done_testing();
