#!perl
use strict;
use warnings;
use File::Spec;
use Test::More;
use lib File::Spec->catdir(qw(t lib));
BEGIN { lib->import('lib') if !-d 't' }
use Sereal::TestSet qw(have_encoder_and_decoder);
use Sereal::Encoder qw(encode_sereal);

plan skip_all => 'Did not find right version of decoder' if !have_encoder_and_decoder();

{

    package ChangingInteger;
    sub TIESCALAR { my $class = shift; bless [ 0, @_ ], $class }

    sub FETCH {
        my $self  = shift;
        my $index = ++$self->[0];
        return $self->[ $index > 3 ? 3 : $index ];
    }
}

my $uv_max = ~0;
my $iv_min = -( $uv_max >> 1 ) - 1;
for my $version ( 1 .. 6 ) {
    my $encoder = Sereal::Encoder->new( { protocol_version => $version } );
    for my $api (qw(object functional)) {
        for my $case (
            [ 44,      -100, 44 ],
            [ -100,    -100, 44 ],
            [ 44,      44,   -100 ],
            [ 0,       -100, 44 ],
            [ 15,      -100, 44 ],
            [ 16,      -100, 44 ],
            [ -16,     -100, 44 ],
            [ -17,     -100, 44 ],
            [ $iv_min, -100, 44 ],
            [ ( $uv_max >> 1 ) + 1, -100, 44 ],
            [ $uv_max - 1, -100, 44 ],
            [ $uv_max, -100, 44 ],
            )
        {
            tie my $value, 'ChangingInteger', @$case;
            my $decoded = eval {
                my $packet =
                      $api eq 'object'
                    ? $encoder->encode($value)
                    : encode_sereal( $value, { protocol_version => $version } );
                Sereal::Decoder->new->decode($packet);
            };
            my $error = $@;
            my $label = "v$version $api first=$case->[0]";
            ok( !$error, "$label: changing FETCH values cannot corrupt an integer tag" )
                or diag($error);
            SKIP: {
                skip 'encode/decode failed', 1 if $error;
                is( $decoded, $case->[0], "$label: sign and payload use the fetched integer" );
            }
        }
    }
}
done_testing();
