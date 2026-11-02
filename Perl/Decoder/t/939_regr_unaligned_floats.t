#!perl
use strict;
use warnings;
use File::Spec;

use lib File::Spec->catdir(qw(t lib));
BEGIN { lib->import('lib') if !-d 't' }

use Test::More;
use Sereal::TestSet            qw(Header floating_point_wire_cases);
use Sereal::Decoder            qw(decode_sereal);
use Sereal::Decoder::Constants qw(:all);

for my $case (
    [ 'FLOAT',       SRL_HDR_FLOAT,       'f' ],
    [ 'DOUBLE',      SRL_HDR_DOUBLE,      'd' ],
    [ 'LONG_DOUBLE', SRL_HDR_LONG_DOUBLE, 'D' ],
    )
{
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
                    my $packet =
                        Header($version) . ( chr(SRL_HDR_PAD) x $padding ) . chr($tag) . $payload;
                    is(
                        decode_sereal($packet), unpack( $format, $payload ),
                        "v$version $name value $value after $padding PAD bytes"
                    );
                }
            }
        }
    }
}

# Portable v7 values and explicitly ordered legacy binary128 values take
# conversion paths in addition to the native copies above. Offsetting each
# packet exercises those paths without assuming aligned input storage.
my $portable = Sereal::Decoder->new({legacy_float_byteorder => 'little'});
for my $version (5 .. 7) {
    for my $case (floating_point_wire_cases()) {
        my ($name, $tag, $hex, $expected) = @$case;
        next if $version < 7 && $tag != SRL_HDR_FLOAT_128;
        for my $padding (0 .. 15) {
            my $packet = Header($version) . (chr(SRL_HDR_PAD) x $padding) . chr($tag) . pack('H*', $hex);
            is($portable->decode($packet), $expected, "v$version $name after $padding PAD bytes");
        }
    }
}
done_testing();
