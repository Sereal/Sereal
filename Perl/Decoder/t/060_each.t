#!perl
use strict;
use warnings;
use Sereal::Decoder;

use Test::More;
if (eval "use Sereal::Encoder; 1") {
    plan tests => 1004;
}
else {
    plan skip_all => 'Requires an encoder';
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

