#!perl
use strict;
use warnings;
use Test::More;
use Scalar::Util qw(isweak refaddr);
use Sereal::Decoder;

{

    package WeakFrozen;
    our $calls = 0;
    sub THAW { ++$calls; return bless [], __PACKAGE__ }
}

my $class = 'WeakFrozen';
for my $version ( 2 .. 6 ) {
    my $header = ( $version < 3 ? '=srl' : "=\xf3rl" ) . chr($version) . "\0";
    for my $options (
        {}, { no_thaw_objects => 1 },
        { thaw_allow_classes   => [], thaw_deny_action => 'raw' },
        { thaw_allow_classes   => [] }, { set_readonly => 1 },
        { set_readonly_scalars => 1 } )
    {
        my $dec   = Sereal::Decoder->new($options);
        my $label = "v$version " . ( join( ',', sort keys %$options ) || 'default' );
        $WeakFrozen::calls = 0;
        for my $wrapper ( '', "\x41" ) {
            my $packet =
                  $header
                . $wrapper . "\x32"
                . chr( 0x60 + length($class) )
                . $class
                . "\x30\x28\x2b\0";
            my ( $out, $ok );
            $ok = eval { $out = $dec->decode($packet); 1 };
            ok( $ok, "$label weak-only frozen target decodes" ) or diag($@);
            is_deeply(
                $out, length($wrapper) ? [undef] : undef,
                "$label weak-only target expires"
            );
            is( $WeakFrozen::calls,               0, "$label expired target does not invoke THAW" );
            is( $dec->decode( $header . "\x07" ), 7, "$label decoder is reusable" );
        }
    }
    for my $raw_options (
        { no_thaw_objects    => 1 },
        { thaw_allow_classes => [], thaw_deny_action => 'raw' } )
    {
        for my $readonly_options ( {}, { set_readonly => 1 }, { set_readonly_scalars => 1 } ) {
            my %options = ( %$raw_options, %$readonly_options );
            my $dec     = Sereal::Decoder->new( \%options );
            my $label   = "v$version " . join( ',', sort keys %options );
            for my $weak_first ( 0, 1 ) {
                my $prefix =
                      "\x43\x32"
                    . chr( 0x60 + length($class) )
                    . $class
                    . ( $weak_first ? "\x30" : '' ) . "\x28";
                my $offset = length($prefix) + 1;
                my $packet =
                    $header . $prefix . "\xab\1\x07\x29" . chr($offset) . "\x30\x29" . chr($offset);
                $WeakFrozen::calls = 0;
                my $out = $dec->decode($packet);
                is(
                    ref( $out->[0] ), 'Sereal::Decoder::THAW_args',
                    "$label raw target is blessed"
                );
                is_deeply(
                    $out->[0], bless( [ 7, $class ], 'Sereal::Decoder::THAW_args' ),
                    "$label classname is appended once"
                );
                is(
                    refaddr( $out->[0] ), refaddr( $out->[1] ),
                    "$label strong references share raw target"
                );
                is(
                    refaddr( $out->[2] ), refaddr( $out->[1] ),
                    "$label weak reference shares raw target"
                );
                is( !!isweak( $out->[0] ), !!$weak_first, "$label primary strength is preserved" );
                ok( isweak( $out->[2] ), "$label duplicate remains weak" );
                is( $WeakFrozen::calls, 0, "$label raw fallback does not invoke THAW" );
                is(
                    !!Internals::SvREADONLY( @{ $out->[1] } ), !!keys(%$readonly_options),
                    "$label argument array readonly flag is preserved"
                );
                is(
                    !!Internals::SvREADONLY( $out->[1]->[-1] ), !!keys(%$readonly_options),
                    "$label appended classname respects readonly option"
                );
            }
        }
    }
}
done_testing;
