#!perl
use strict;
use warnings;

use Sereal::Path;
use Sereal::Encoder qw/encode_sereal/;
use Data::Dumper;
use Test::More;

my $arrayref = [ 'foo', 'bar', 'barbar' ];
my $sp = Sereal::Path->new(encode_sereal($arrayref));

my $expected = [ 'foo' ];
foreach my $q ('$[0]', '$.0') {
    my $got = [ $sp->values($q) ];
    is_deeply($got, $expected, " query $q")
        or diag("got:\n" . Dumper($got) . "expected:\n" . Dumper($expected));
}

$expected = [ 'foo', 'bar', 'barbar' ];
foreach my $q ('$[*]', '$.*') {
    my $got = [ $sp->values($q) ];
    is_deeply($got, $expected, " query $q")
        or diag("got:\n" . Dumper($got) . "expected:\n" . Dumper($expected));
}

done_testing();
