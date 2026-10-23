#!perl
use strict;
use warnings;
use Test::More;
use Scalar::Util qw(blessed isweak refaddr);
use Sereal::Decoder;

{

    package WeakBlessTarget;
    our $destroyed = 0;
    sub DESTROY { ++$destroyed }
}

my $class = 'WeakBlessTarget';
for my $version ( 1 .. 6 ) {
    my $header = ( $version < 3 ? '=srl' : "=\xf3rl" ) . chr($version) . "\0";
    for my $options ( {}, { set_readonly => 1 }, { set_readonly_scalars => 1 } ) {
        my $dec    = Sereal::Decoder->new($options);
        my $label  = "v$version " . ( join( ',', sort keys %$options ) || 'default' );
        my $object = "\x2c" . chr( 0x60 + length($class) ) . $class . "\x30\x28";

        # ARRAYREF(1), OBJECT, WEAKEN, REFN, tracked ARRAY(0).
        # A handcrafted packet is needed to preserve a weak-only object.
        $WeakBlessTarget::destroyed = 0;
        my $weak_only = $dec->decode( $header . "\x41" . $object . "\xab\0" );
        ok( !defined( $weak_only->[0] ), "$label weak-only target expires" );
        is( $WeakBlessTarget::destroyed, 0, "$label expired target was never blessed" );
        undef $weak_only;
        is( $WeakBlessTarget::destroyed, 0, "$label cleanup does not call DESTROY" );

        # Introduce the weak reference first, so the target gets a temporary
        # keepalive, then add a strong REFP to the same tracked array.
        my $prefix    = "\x42" . $object;
        my $offset    = length($prefix) + ( $version == 1 ? length($header) : 1 );
        my $surviving = $dec->decode( $header . $prefix . "\xab\0\x29" . chr($offset) );
        is( blessed( $surviving->[1] ), $class, "$label strong target is blessed" );
        is( blessed( $surviving->[0] ), $class, "$label weak reference sees blessing" );
        ok( isweak( $surviving->[0] ),  "$label first reference remains weak" );
        ok( !isweak( $surviving->[1] ), "$label second reference remains strong" );
        is(
            refaddr( $surviving->[0] ), refaddr( $surviving->[1] ),
            "$label references share target"
        );
        is(
            !!Internals::SvREADONLY( @{ $surviving->[1] } ),
            !!( $options->{set_readonly} || $options->{set_readonly_scalars} ),
            "$label target readonly flag is preserved"
        );
        is(
            $WeakBlessTarget::destroyed, 0,
            "$label surviving target is not destroyed during decode"
        );
        undef $surviving;
        is( $WeakBlessTarget::destroyed, 1, "$label surviving target is destroyed on release" );
    }
}
done_testing;
