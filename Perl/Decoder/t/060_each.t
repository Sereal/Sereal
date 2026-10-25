#!perl
use strict;
use warnings;
use Test::More;
use File::Spec;
use lib File::Spec->catdir(qw(t lib));

BEGIN {
    lib->import('lib')
        if !-d 't';
}
use Sereal::TestSet qw(:all);
use Sereal::Decoder;
use Sereal::TestSet::Corpus qw(read_packet);

plan tests => 1004;
my $d = Sereal::Decoder->new();
my $corpus = File::Spec->catdir( 't', 'data', 'packet_corpus' );
my @cases = (
    [ 'each/alpha',      26 ],
    [ 'each/upper',      26 ],
    [ 'each/small_nums', 101 ],
    [ 'each/large_nums', 513 ],
    [ 'each/distinct',   26 ],
);

sub check_each {
    my ( $corpus, $decoder, $name, $expected ) = @_;
    my $packet = read_packet( $corpus, 'decoder-features', $name );
    my $undump = $decoder->decode($packet);
    my $count  = 0;
    while ( my ( $h, $k ) = each %$undump ) {
        $count++;
    }
    is( $count, $expected, "each iterates all keys in $name" );
}

for ( 1 .. 1000 ) {
    my $case = $cases[ $_ % @cases ];
    check_each( $corpus, $d, @$case );
}
check_each( $corpus, $d, @{ $cases[$_] } ) for 0 .. 3;

