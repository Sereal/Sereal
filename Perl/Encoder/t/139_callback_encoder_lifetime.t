#!perl
use strict;
use warnings;
use File::Spec;
use Test::More;
use lib File::Spec->catdir(qw(t lib));
BEGIN { lib->import('lib') if !-d 't' }
use Sereal::TestSet qw(have_encoder_and_decoder);
use Sereal::Encoder qw(sereal_encode_with_object);
plan skip_all => 'decoder unavailable' unless have_encoder_and_decoder();
our $encoder;
{

    package SerealDropEncoderFreeze;
    sub FREEZE { undef $main::encoder; 'frozen' }
    sub THAW   { bless { value => $_[2] }, $_[0] }

    package SerealDropEncoderFetch;
    sub TIESCALAR { bless {}, $_[0] }
    sub FETCH     { undef $main::encoder; 'fetched' }
}
for my $version ( 2 .. 6 ) {
    for my $custom ( 0, 1 ) {
        $encoder = Sereal::Encoder->new( { protocol_version => $version, freeze_callbacks => 1 } );
        my $object = bless {}, 'SerealDropEncoderFreeze';
        my $packet =
            $custom ? sereal_encode_with_object( $encoder, $object ) : $encoder->encode($object);
        ok( !defined $encoder, 'FREEZE dropped the encoder variable' );
        is(
            Sereal::Decoder->new->decode($packet)->{value}, 'frozen',
            'encode finishes with its owner retained'
        );

        my @input = (undef);
        tie $input[0], 'SerealDropEncoderFetch';
        $encoder = Sereal::Encoder->new( { protocol_version => $version } );
        $packet =
            $custom ? sereal_encode_with_object( $encoder, \@input ) : $encoder->encode( \@input );
        ok( !defined $encoder, 'FETCH dropped the encoder variable' );
        is_deeply(
            Sereal::Decoder->new->decode($packet), ['fetched'],
            'FETCH cannot free the active encoder'
        );
    }
}
done_testing;
