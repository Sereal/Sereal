#!perl
use strict;
use warnings;
use Sereal::Decoder;
use File::Spec;
use Scalar::Util qw( blessed );
use lib File::Spec->catdir(qw(t lib));

BEGIN {
    lib->import('lib')
        if !-d 't';
}

use Sereal::TestSet::Corpus qw(read_packet);
use Sereal::TestSet qw(:all);
use Test::More;

my $class = 'MyFoo';
my %hash = ( x => 1 );
my $object = bless( \%hash, $class );
my $blob = read_packet(
    File::Spec->catdir( 't', 'data', 'packet_corpus' ),
    'decoder-features', 'nobless'
);

# do not bless anything
{
    my $dec  = Sereal::Decoder->new( { no_bless_objects => 1 } );
    my $data = $dec->decode($blob);

    ok( ref($data) && !blessed($data), 'reference without class' );
    is_deeply( $data, \%hash, 'same structure' );
}

# normally do the blessing
{
    my $dec  = Sereal::Decoder->new();
    my $data = $dec->decode($blob);

    is_deeply( $data, $object, 'same structure' );
    isa_ok( $data, $class, 'same class' );
}

done_testing();

