#!perl
use strict;
use warnings;
use File::Spec;
use Scalar::Util qw(dualvar refaddr);
use Test::More;
use lib File::Spec->catdir(qw(t lib));
BEGIN { lib->import('lib') if !-d 't' }
use Sereal::TestSet qw(have_encoder_and_decoder);
use Sereal::Encoder;
plan skip_all => 'decoder unavailable' unless have_encoder_and_decoder();
{

    package SerealCachedIntegerFetch;
    sub TIESCALAR { bless { value => $_[1], count => 0 }, $_[0] }
    sub FETCH     { ++$_[0]{count}; $_[0]{value} }
}
my $uvmax  = ~0;
my $ivmax  = $uvmax >> 1;
my $ivmin  = -$ivmax - 1;
my @values = (
    0,     1,     15,         16,     127,    128, 255, 256, 16383, 16384,
    65535, 65536, 2147483647, $ivmax, $uvmax, -1,  -16, -17, $ivmin
);
for my $version ( 1 .. 6 ) {
    my $encoder = Sereal::Encoder->new( { protocol_version => $version } );
    for my $value (@values) {
        my $label   = "v$version integer=$value";
        my $shared  = $value;
        my $data    = [ $value, \$shared, \$shared ];
        my $packet  = $encoder->encode($data);
        my $decoded = Sereal::Decoder->new->decode($packet);
        is_deeply( $decoded, $data, "$label direct and referenced values round trip" );
        is(
            refaddr( $decoded->[1] ), refaddr( $decoded->[2] ),
            "$label preserves shared references"
        );
        is( $encoder->encode($data), $packet, "$label has stable output on Encoder reuse" );

        my @magic = (undef);
        my $tie   = tie $magic[0], 'SerealCachedIntegerFetch', $value;
        is_deeply(
            Sereal::Decoder->new->decode( $encoder->encode( \@magic ) ), [$value],
            "$label encodes the fetched integer"
        );
        is( $tie->{count}, 1, "$label FETCHes once" );
    }
    my $dual = dualvar( 37, 'use the string' );
    is(
        $encoder->encode( [$dual] ), $encoder->encode( ['use the string'] ),
        "v$version cached numeric value does not override a dualvar string"
    );
}
done_testing;
