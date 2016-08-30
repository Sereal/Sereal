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

if (have_encoder_and_decoder()) {
    plan tests => 1004;
}
else {
    plan skip_all => 'Did not find right version of encoder';
}

my $e= Sereal::Encoder->new();
my $d= Sereal::Decoder->new();

for ( 1 .. 1000, [ 'a' .. 'z' ], [ 'A' .. 'Z' ], [ 0 .. 100 ], [ 10000 .. 10512 ] ) {
    my %hash;
    if (ref $_) {
        $hash{$_}++ for @$_;
    } else {
        $hash{rand()}++ for 1..26;
    }
    my $undump= $d->decode($e->encode(\%hash));
    my $count= 0;
    while( my ($h, $k)= each %$undump ) {
        $count++;
    }
    is($count, keys %hash, "Got the expected count of keys: [ @{[ sort keys %hash ]} ]"); 
}

