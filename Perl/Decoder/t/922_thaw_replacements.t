#!perl
use strict;
use warnings;
use Test::More;
use Scalar::Util qw(blessed isweak refaddr);
use Sereal::Decoder;

{

    package ThawReturn;
    our ( $value, $calls, $destroyed );

    sub THAW {
        ++$calls;
        return $value unless defined($value) && $value eq 'reference';
        return bless [], 'ThawResult';
    }

    package ThawResult;
    sub DESTROY { ++$ThawReturn::destroyed }
}

my $class = 'ThawReturn';
for my $version ( 2 .. 6 ) {
    my $header = ( $version < 3 ? '=srl' : "=\xf3rl" ) . chr($version) . "\0";
    for my $options ( {}, { set_readonly => 1 }, { set_readonly_scalars => 1 } ) {
        my $dec = Sereal::Decoder->new($options);
        for my $weak_first ( 0, 1 ) {

            # Three references to one frozen array: a strong one, a weak one,
            # and either a weak or strong primary OBJECT_FREEZE reference.
            my $prefix =
                  "\x43\x32"
                . chr( 0x60 + length($class) )
                . $class
                . ( $weak_first ? "\x30" : '' ) . "\x28";
            my $offset = length($prefix) + 1;
            my $packet =
                $header . $prefix . "\xab\x01\x07\x29" . chr($offset) . "\x30\x29" . chr($offset);
            for my $value ( undef, 0, '', 42, 'text', 'reference' ) {
                $ThawReturn::value = $value;
                $ThawReturn::calls = $ThawReturn::destroyed = 0;
                my $label =
                      "v$version weak_first=$weak_first "
                    . join( ',', sort keys %$options )
                    . ' return='
                    . ( defined($value) ? "'$value'" : 'undef' );
                my ( @warnings, $got );
                {
                    local $SIG{__WARN__} = sub { push @warnings, @_ };
                    $got = $dec->decode($packet);
                }
                is_deeply( \@warnings, [], "$label no reference-count warnings" );
                is( $ThawReturn::calls, 1, "$label THAW runs once" );
                if ( defined($value) && $value eq 'reference' ) {
                    is( blessed( $got->[1] ), 'ThawResult', "$label strong result is blessed" );
                    is(
                        refaddr( $got->[0] ), refaddr( $got->[1] ),
                        "$label primary shares result"
                    );
                    is(
                        refaddr( $got->[2] ), refaddr( $got->[1] ),
                        "$label weak fixup shares result"
                    );
                    is(
                        !!isweak( $got->[0] ), !!$weak_first,
                        "$label primary strength is preserved"
                    );
                    ok( isweak( $got->[2] ), "$label fixup remains weak" );
                    is( $ThawReturn::destroyed, 0, "$label result is alive" );
                }
                else {
                    is_deeply(
                        $got, [ $value, $value, $value ],
                        "$label all occurrences get scalar result"
                    );
                }
                undef $got;
                is(
                    $ThawReturn::destroyed,
                    defined($value) && $value eq 'reference' ? 1 : 0,
                    "$label result is released exactly once"
                );
            }
        }
    }
}
done_testing;
