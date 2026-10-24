#!perl
use strict;
use warnings;
use File::Spec;
use Test::More;
use lib File::Spec->catdir(qw(t lib));
BEGIN { lib->import('lib') if !-d 't' }
use Sereal::TestSet qw(have_encoder_and_decoder);
use Sereal::Encoder;
plan skip_all => 'decoder unavailable' unless have_encoder_and_decoder();

{

    package SerealFetchOnceScalar;
    sub TIESCALAR { bless { value => $_[1], count => 0 }, $_[0] }
    sub FETCH     { ++$_[0]{count}; $_[0]{value} }

    package SerealFetchOnceArray;
    sub TIEARRAY  { bless { values => $_[1], count => {} }, $_[0] }
    sub FETCHSIZE { scalar @{ $_[0]{values} } }
    sub FETCH     { ++$_[0]{count}{ $_[1] }; $_[0]{values}[ $_[1] ] }

    package SerealFetchOnceHash;
    sub TIEHASH  { bless { values => $_[1], count => {} }, $_[0] }
    sub FIRSTKEY { keys %{ $_[0]{values} }; each %{ $_[0]{values} } }
    sub NEXTKEY  { each %{ $_[0]{values} } }
    sub FETCH    { ++$_[0]{count}{ $_[1] }; $_[0]{values}{ $_[1] } }

    package SerealFetchOnceString;
    use overload '""' => sub { 'stringified once' }, fallback => 1;
}

for my $version ( 1 .. 6 ) {
    for my $dedupe ( 0, 1 ) {
        for my $value ( undef, 0, -17, 1.25, 'string value', "wide \x{100}", [ 1, 2 ] ) {
            my @input = (undef);
            my $tie   = tie $input[0], 'SerealFetchOnceScalar', $value;
            my $encoder =
                Sereal::Encoder->new( { protocol_version => $version, dedupe_strings => $dedupe } );
            my $decoded = Sereal::Decoder->new->decode( $encoder->encode( \@input ) );
            is( $tie->{count}, 1, "v$version dedupe=$dedupe scalar FETCH once" );
            is_deeply( $decoded, [$value], 'encodes the fetched value' );
        }
    }
    my @array;
    my $array   = tie @array, 'SerealFetchOnceArray', [ 11, 'value', 1.25 ];
    my $encoder = Sereal::Encoder->new( { protocol_version => $version } );
    is_deeply(
        Sereal::Decoder->new->decode( $encoder->encode( \@array ) ), [ 11, 'value', 1.25 ],
        'tied array round trips'
    );
    is_deeply( $array->{count}, { 0 => 1, 1 => 1, 2 => 1 }, 'tied array FETCH once per index' );
    for my $sort ( 0, 1, 2 ) {
        my %hash;
        my $hash = tie %hash, 'SerealFetchOnceHash', { a => 11, b => 'value', c => 1.25 };
        $encoder = Sereal::Encoder->new( { protocol_version => $version, sort_keys => $sort } );
        is_deeply(
            Sereal::Decoder->new->decode( $encoder->encode( \%hash ) ), $hash->{values},
            "sort=$sort tied hash round trips"
        );
        is_deeply(
            $hash->{count}, { a => 1, b => 1, c => 1 },
            "sort=$sort tied hash FETCH once per key"
        );
    }
    {
        my @input = (undef);
        my $tie = tie $input[0], 'SerealFetchOnceScalar', bless( sub { }, 'SerealFetchOnceString' );
        $encoder = Sereal::Encoder->new( { protocol_version => $version, stringify_unknown => 1 } );
        is_deeply(
            Sereal::Decoder->new->decode( $encoder->encode( \@input ) ), ['stringified once'],
            'stringification uses the fetched reference'
        );
        is( $tie->{count}, 1, 'stringification does not FETCH the reference again' );
    }
    {
        my @input = ( sub { }, 'sibling' );
        $encoder = Sereal::Encoder->new(
            { protocol_version => $version, undef_unknown => 1, warn_unknown => 1 } );
        local $SIG{__WARN__} = sub { @input = () };
        eval { $encoder->encode( \@input ) };
        like(
            $@, qr/input array was modified during encoding/,
            'warning handlers are guarded callbacks'
        );
        is_deeply(
            Sereal::Decoder->new->decode( $encoder->encode( [7] ) ), [7],
            'reuse after a warning-handler mutation'
        );
    }
    {
        my $value;
        my $tie = tie $value, 'SerealFetchOnceScalar', 'shared magical value';
        $encoder = Sereal::Encoder->new( { protocol_version => $version } );
        my $decoded = Sereal::Decoder->new->decode( $encoder->encode( [ \$value, \$value ] ) );
        is( $tie->{count}, 1, 'an already encoded magical referent is not FETCHed for REFP' );
        is_deeply(
            [ ${ $decoded->[0] },     ${ $decoded->[1] } ],
            [ 'shared magical value', 'shared magical value' ],
            'both references use the originally fetched value'
        );
    }
}
done_testing;
