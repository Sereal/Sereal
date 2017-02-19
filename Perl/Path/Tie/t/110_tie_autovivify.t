#!perl
use strict;
use warnings;

use Test::More;
use Sereal::Path::Tie;
use Sereal::Path::Iterator;
use Sereal::Encoder qw/encode_sereal/;

subtest "autovivify and hash", sub {
    my $data = {
        a => {aa => 1},
        foo => {},
        bar => undef,
    };

    my $spi = Sereal::Path::Iterator->new(encode_sereal($data));
    my $tie = Sereal::Path::Tie->new($spi);

    is_deeply($tie->{test},                 $data->{test},                  'data->{test}');
    is_deeply($tie->{foo}{test},            $data->{foo}{test},             'data->{foo}{test}');
    is_deeply($tie->{bar}{test},            $data->{bar}{test},             'data->{bar}{test}');
    is_deeply($tie->{baz}{test},            $data->{baz}{test},             'data->{baz}{test}');
    is_deeply($tie->{baz}{test}{test},      $data->{baz}{test}{test},       'data->{baz}{test}{test}');
    is_deeply($tie->{baz}{test2}[1],        $data->{baz}{test2}[1],         'data->{baz}{test2}[1]');
    is_deeply($tie->{baz}{test2}[200],      $data->{baz}{test2}[200],       'data->{baz}{test2}[200]');
    is_deeply($tie->{baz}{test2}[200][0],   $data->{baz}{test2}[200][0],    'data->{baz}{test2}[200][0]');
    is_deeply($tie->{baz}{test2}[200][300], $data->{baz}{test2}[200][300],  'data->{baz}{test2}[200][300]');

    is_deeply([sort keys %$tie], [sort keys %$data], "sort keys data after vivification");

    my (@pairs_tie, @pairs_data);
    while (my ($k, $v) = each %$tie) { push @pairs_tie, [$k, $v]; }
    @pairs_tie = sort { $a->[0] cmp $b->[0] } @pairs_tie;
    while (my ($k, $v) = each %$data) { push @pairs_data, [$k, $v]; }
    @pairs_data = sort { $a->[0] cmp $b->[0] } @pairs_data;
    is_deeply(\@pairs_tie, \@pairs_tie, "sorted kv pairs");

    is(exists $tie->{baz}{test},    exists $data->{baz}{test},    'exists $data->{baz}{test}');
    is(exists $tie->{baz}{missing}, exists $data->{baz}{missing}, 'exists $data->{baz}{missing}');

    is_deeply($tie, $data, "full structure after vivification");
};

subtest "autovivify and array", sub {
    my $data = [ 4 ];
    my $spi = Sereal::Path::Iterator->new(encode_sereal($data));
    my $tie = Sereal::Path::Tie->new($spi);

    is_deeply($tie->[2][0], $data->[2][0],      'autofifivy data->[2]');
    is(scalar(@$tie),       scalar(@$data),     'scalar(@$data)');
    is(exists($tie->[1]),   exists($data->[1]), 'exists($data->[1])');
    $data->[1] = undef; $tie->[1] = undef;
    is(exists($tie->[1]),   exists($data->[1]), 'exists($data->[1]) after $data->[1] = undef');
    is(scalar(@$tie),       scalar(@$data),     'scalar(@$data) after $data->[1] = undef');

    $data->[3] = 3; $tie->[3] = 3;
    is(scalar(@$tie),       scalar(@$data),     'scalar(@$data) after $data->[3] = 3');

    is_deeply($tie->[7][7],    $data->[7][7],       'data->[7][7]');
    is_deeply($tie->[8][8][8], $data->[8][8][8],    'data->[8][8][8]');
    is(exists($tie->[8][8]),   exists($data->[8][8]), 'exists($data->[8][8])');

    my (@pairs_tie, @pairs_data);
    push @pairs_tie, $_ foreach @$tie;
    push @pairs_data, $_ foreach @$data;
    is_deeply(\@pairs_tie, \@pairs_data, "values");

    is_deeply($tie, $data, "full structure after vivification");
};

done_testing();
