#!perl
use strict;
use warnings;
use File::Spec;
use Scalar::Util qw(weaken refaddr isweak);

use lib File::Spec->catdir(qw(t lib));
BEGIN { lib->import('lib') if !-d 't' }
use Sereal::TestSet qw(have_encoder_and_decoder);
use Sereal::Encoder;
use Test::More;

if ( !have_encoder_and_decoder() ) {
    plan skip_all => 'Did not find right version of decoder';
    exit 0;
}

for my $version ( 1 .. 6 ) {
    {
        my $obj = [];
        my @a   = ($obj);
        weaken( $a[0] );
        push @a, \$a[0];
        my $encoder = Sereal::Encoder->new( { protocol_version => $version } );
        my $decoded = Sereal::Decoder::decode_sereal( $encoder->encode( \@a ) );
        is(
            refaddr( $decoded->[1] ), refaddr( \$decoded->[0] ),
            "v$version tracked weak slot reference identity"
        );
        push @{ ${ $decoded->[1] } }, 'changed';
        is( $decoded->[0][0], 'changed', "v$version tracked weak slot reference mutation" );
    }

    {
        my $obj  = [];
        my @refs = ( $obj, $obj );
        weaken( $refs[0] );
        my $encoder = Sereal::Encoder->new( { protocol_version => $version } );
        my $decoded = Sereal::Decoder::decode_sereal( $encoder->encode( \@refs ) );
        ok( isweak( $decoded->[0] ), "v$version supported weak reference remains weak" );
        is(
            refaddr( $decoded->[0] ), refaddr( $decoded->[1] ),
            "v$version supported weak reference retains identity"
        );
    }

    for my $option (qw(undef_unknown stringify_unknown)) {
        {
            my $code = sub { };
            my @refs = ( $code, $code );
            weaken( $refs[0] );
            push @refs, \$refs[0];
            my $encoder = Sereal::Encoder->new( { protocol_version => $version, $option => 1 } );
            my $decoded = Sereal::Decoder::decode_sereal( $encoder->encode( \@refs ) );
            is(
                refaddr( $decoded->[2] ), refaddr( \$decoded->[0] ),
                "v$version $option: reference to replaced weak slot retains identity"
            );
            ${ $decoded->[2] } = 'changed';
            is(
                $decoded->[0], 'changed',
                "v$version $option: mutation reaches the replaced weak slot"
            );
        }

        for my $shape (qw(weak_only two_weak weak_then_strong strong_then_weak)) {
            my $code = sub { };
            my @refs = $shape eq 'weak_only' ? ($code) : ( $code, $code );
            weaken( $refs[0] ) if $shape ne 'strong_then_weak';
            weaken( $refs[1] ) if $shape eq 'two_weak' || $shape eq 'strong_then_weak';
            my $decoded = eval {
                my $encoder = Sereal::Encoder->new( {
                    protocol_version => $version,
                    $option          => 1,
                } );
                Sereal::Decoder::decode_sereal( $encoder->encode( \@refs ) );
            };
            my $error = $@;
            ok( !$error && ref($decoded) eq 'ARRAY', "v$version $option $shape decodes" )
                or diag($error);
            SKIP: {
                skip 'decode failed', 1 if $error;
                my $expected = $option eq 'undef_unknown' ? undef : "$code";
                is_deeply(
                    $decoded, [ map { $expected } @refs ],
                    "v$version $option $shape replacement values survive"
                );
            }
        }
    }
}

done_testing();
