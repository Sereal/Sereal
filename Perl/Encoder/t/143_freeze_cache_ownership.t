#!perl
use strict;
use warnings;
use File::Spec;
use Scalar::Util qw(weaken);
use Test::More;
use lib File::Spec->catdir(qw(t lib));
BEGIN { lib->import('lib') if !-d 't' }
use Sereal::TestSet qw(have_encoder_and_decoder);
use Sereal::Encoder;
plan skip_all => 'decoder unavailable' unless have_encoder_and_decoder();

our ( $owner, $calls, $destroys, $drop );
{

    package SerealFreezeCacheOwner;

    sub FREEZE {
        ++$main::calls;
        undef $main::owner if $main::drop eq 'freeze';
        return 'payload';
    }
    sub DESTROY { ++$main::destroys }
}
for my $version ( 2 .. 6 ) {
    for my $weak ( 0, 1 ) {
        for my $how (qw(keep freeze predicate)) {
            ( $calls, $destroys, $drop ) = ( 0, 0, $how );
            $owner = bless {}, 'SerealFreezeCacheOwner';
            my $data = [ $owner, $owner ];
            if ($weak) { weaken( $data->[0] ); weaken( $data->[1] ) }
            my %options = ( protocol_version => $version, freeze_callbacks => 1 );
            $options{freeze_allow_classes} = sub {
                undef $owner;
                undef $_[1];
                return 1;
                }
                if $how eq 'predicate';
            my $encoder = Sereal::Encoder->new( \%options );
            my $packet  = eval { $encoder->encode($data) };
            my $error   = $@;
            my $label   = "v$version weak=$weak drop=$how";
            is( $error, '', "$label encodes safely" );
            is( $calls, 1,  "$label invokes FREEZE once for the same referent" );
            ok( defined $packet, "$label produces a packet" );
            is_deeply(
                Sereal::Decoder->new->decode( $encoder->encode( [7] ) ), [7],
                "$label Encoder remains reusable"
            );
            undef $owner;
            undef $data;
            is( $destroys, 1, "$label releases the original object once" );
        }
    }
}
done_testing;
