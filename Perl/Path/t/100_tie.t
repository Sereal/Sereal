#!perl
use strict;
use warnings;

use Test::More;
use Sereal::Path::Tie;
use Sereal::Encoder qw/encode_sereal/;

my $data = [
    { a => 1 },
    { a => 2 },
];

my $tie = Sereal::Path::Tie::parse(encode_sereal($data));
is_deeply($tie,         $data,          'data');
is_deeply($tie->[0],    $data->[0],     'data[0]');
is_deeply($tie->[1],    $data->[1],     'data[1]');
is_deeply($tie->[0],    { a => 1 },     'data[0]');
is_deeply($tie->[1]{a}, 2,              'data[1]{a}');

done_testing();
