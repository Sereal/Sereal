#!perl
use strict;
use warnings;

use Test::More;
use Sereal::Path::Tie;
use Sereal::Encoder qw/encode_sereal/;

my $data = {
    a => {aa => 1},
    foo => {},
    bar => undef,
};
my $tie = Sereal::Path::Tie::parse(encode_sereal($data));

is_deeply($tie->{foo}{test},    $data->{foo}{test}, 'data->{foo}{test}');
is_deeply($tie->{bar}{test},    $data->{bar}{test}, 'data->{bar}{test}');
is_deeply($tie->{baz}{test},    $data->{baz}{test}, 'data->{baz}{test}');
is_deeply($tie->{baz}{test}{test},    $data->{baz}{test}{test}, 'data->{baz}{test}{test}');
is_deeply($tie->{baz}{test2}[1],    $data->{baz}{test2}[1], 'data->{baz}{test2}[1]');
is_deeply($tie->{baz}{test2}[200],    $data->{baz}{test2}[200], 'data->{baz}{test2}[200]');
is_deeply($tie->{baz}{test2}[200][0],    $data->{baz}{test2}[200][0], 'data->{baz}{test2}[200][0]');
is_deeply($tie->{baz}{test2}[200][300],    $data->{baz}{test2}[200][300], 'data->{baz}{test2}[200][300]');

is_deeply([sort keys %$tie], [sort keys %$data], "sort keys data after vivification");
is_deeply($tie, $data, "full structure after vivification");
#=============================================================================================#

$data = [ 4 ];
$tie = Sereal::Path::Tie::parse(encode_sereal($data));
is_deeply($tie->[1][12],    $data->[1][12], 'data->[1][12]');
is_deeply($tie->[200][12],    $data->[200][12], 'data->[200][12]');
is_deeply($tie->[10]->{test},    $data->[10]->{test}, 'data->[0]->{test}');
is_deeply($tie->[300]->{test},    $data->[300]->{test}, 'data->[300]->{test}');

#=============================================================================================#

done_testing();

