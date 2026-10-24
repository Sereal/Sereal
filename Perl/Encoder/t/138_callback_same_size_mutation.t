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

    package SerealSameSizeFetch;
    sub TIESCALAR { bless { callback => $_[1], count => 0 }, $_[0] }
    sub FETCH     { ++$_[0]{count}; $_[0]{callback}->(); 'fetched' }

    package SerealUntieArrayFetch;
    sub TIEARRAY  { bless {}, $_[0] }
    sub FETCHSIZE { 1 }
    sub FETCH     { untie @main::untie_input; 7 }
    sub UNTIE     { }
}
our @untie_input;
for my $version ( 1 .. 6 ) {
    for my $sort ( 0, 1, 2, 3 ) {
        my $encoder = Sereal::Encoder->new( { protocol_version => $version, sort_keys => $sort } );
        for my $victim (qw(a b)) {
            for my $iteration ( 1 .. 8 ) {
                my %input = ( a => undef, b => 'sibling', c => 'third' );
                my $ran;
                tie $input{a}, 'SerealSameSizeFetch', sub {
                    return if $ran++;
                    delete $input{$victim};
                    $input{"replacement-$iteration"} = 'replacement';
                };
                my $packet = eval { $encoder->encode( \%input ) };
                my $error  = $@;
                my $label  = "v$version sort=$sort replace=$victim pass=$iteration";
                if ($error) {
                    like(
                        $error, qr/input hash was modified during encoding/,
                        "$label fails safely"
                    );
                }
                else {
                    my $decoded = eval { Sereal::Decoder->new->decode($packet) };
                    is( ref($decoded), 'HASH', "$label produces a decodable hash" ) or diag $@;
                }
                %input = ();    # break the tie's closure cycle
                is_deeply(
                    Sereal::Decoder->new->decode( $encoder->encode( [7] ) ), [7],
                    "$label encoder remains reusable"
                );
            }
        }
    }
    my @input = ( undef, 'old second', 'old third' );
    my $tie   = tie $input[0], 'SerealSameSizeFetch',
        sub { @input[ 1, 2 ] = ( 'new second', 'new third' ) };
    my $encoder = Sereal::Encoder->new( { protocol_version => $version } );
    is_deeply(
        Sereal::Decoder->new->decode( $encoder->encode( \@input ) ),
        [ 'fetched', 'new second', 'new third' ],
        'reads later array values at traversal time'
    );
    is( $tie->{count}, 1, 'value replacement does not FETCH the current value again' );
    @input = ();
    tie @untie_input, 'SerealUntieArrayFetch';
    eval { $encoder->encode( \@untie_input ) };
    like(
        $@, qr/input tied container was modified during encoding/,
        'untie during FETCH is rejected safely'
    );
}
done_testing;
