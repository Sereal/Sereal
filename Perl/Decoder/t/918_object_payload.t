#!perl
use strict;
use warnings;
use Test::More;
use Sereal::Decoder;

for my $version ( 1 .. 6 ) {
    my $header = ( $version < 3 ? '=srl' : "=\xf3rl" ) . chr($version) . "\0";
    for my $options (
        {}, { set_readonly => 1 }, { set_readonly_scalars => 1 },
        { no_bless_objects => 1 }, { no_bless_objects => 1, set_readonly => 1 } )
    {
        my $dec   = Sereal::Decoder->new($options);
        my $label = "v$version " . join( ',', sort keys %$options );
        for my $payload ( "\x01", "\x25", "\x61x", "\x2b\0", "\x2a\0" ) {
            my $object = "\x2c\x61X";

            # The first array element defines classname X; the second reuses it.
            my $objectv = "\x42" . $object . "\x40\x2d" . chr( $version == 1 ? 8 : 3 );
            for my $prefix ( $object, $objectv ) {
                eval { $dec->decode( $header . $prefix . $payload ) };
                # Bare ARRAY/HASH payloads are rejected even earlier, as
                # aggregates outside a REFN (t/935).
                like(
                    $@, qr/OBJECT\(V\) must be followed by a reference|only valid as the referent of a REFN/,
                    "$label rejects non-reference payload " . unpack( 'H*', $prefix . $payload ) );
            }
        }
        my $good = $dec->decode( $header . "\x2c\x61X\x40" );
        is(
            ref($good), $options->{no_bless_objects} ? 'ARRAY' : 'X',
            "$label valid object still works after rejection"
        );
    }
}
done_testing;
