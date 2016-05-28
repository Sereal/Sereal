#!perl
use strict;
use warnings;

use Test::More;
use Sereal::Path::Tie;
use Sereal::Encoder qw/encode_sereal/;

subtest "simple access in array", sub {
    my $data = [
        { a => 1 },
        { a => 2 },
        [ "test" ],
        1,
        "hi",
    ];

    my $tie = Sereal::Path::Tie::parse(encode_sereal($data));
    is_deeply($tie,         $data,          'data array');
    is_deeply($tie->[0],    $data->[0],     'data[0]');
    is_deeply($tie->[1],    $data->[1],     'data[1]');
    is_deeply($tie->[2],    $data->[2],     'data[2]');
    is_deeply($tie->[3],    $data->[3],     'data[3]');
    is_deeply($tie->[4],    $data->[4],     'data[4]');

    is_deeply($tie->[0],    { a => 1 },     'data[0]');
    is_deeply($tie->[1]{a}, 2,              'data[1]{a}');
    is_deeply($tie->[2][0], $data->[2][0],  'data[2][0]');
    is_deeply($tie->[3],    1,              'data[3]');
    is_deeply($tie->[4],    "hi",           'data[4]');
};

subtest "simple access in hash", sub {
    my $data = {
        foo => 1,
        bar => "abcdefg",
        baz => { a => 1 },
    };

    my $tie = Sereal::Path::Tie::parse(encode_sereal($data));

    is_deeply($tie,             $data,              "data hash");
    is_deeply($tie->{foo},      $data->{foo},       "data->{foo}");
    is_deeply($tie->{foo},      1,                  "data->{foo}");
    is_deeply($tie->{bar},      $data->{bar},       "data->{bar}");
    is_deeply($tie->{bar},      "abcdefg",          "data->{bar}");
    is_deeply($tie->{baz},      $data->{baz},       "data->{baz}");
    is_deeply($tie->{baz},      { a => 1},          "data->{baz}");
    is_deeply($tie->{baz}->{a}, $data->{baz}->{a},  "data->{baz}->{a}");
    is_deeply($tie->{baz}->{a}, 1,                  "data->{baz}->{a}");
};

subtest "simple iteration over hash", sub {
    my $data = {
        c1 => { f => 1 },
        c2 => {
            a => {
                b => 1,
            },
        },
    };

    my $tie = Sereal::Path::Tie::parse(encode_sereal($data));
    is_deeply([sort keys %$tie], [sort keys %$data], "sort keys data");

    my @pairs_tie;
    while (my ($k, $v) = each %$tie) { push @pairs_tie, [$k, $v]; }
    @pairs_tie = sort { $a->[0] cmp $b->[0] } @pairs_tie;

    my @pairs_data;
    while (my ($k, $v) = each %$data) { push @pairs_data, [$k, $v]; }
    @pairs_data = sort { $a->[0] cmp $b->[0] } @pairs_data;

    is_deeply(\@pairs_tie, \@pairs_tie, "sorted kv pairs");

    my $data1 = {
        a => 1,
        b => 2,
    };

    my $tie1 = Sereal::Path::Tie::parse(encode_sereal($data1));
    is_deeply([sort values %$tie1], [sort values %$data1], "sort values data");
};

subtest "simple iteration over array", sub {
    my $data = [ 'foo', 'bar', 'baz' ];
    my $tie = Sereal::Path::Tie::parse(encode_sereal($data));
    is_deeply([sort @$tie], [sort @$data], "sort data");

    my @pairs_tie;
    while (my $v = each @$tie) { push @pairs_tie, $v; }

    my @pairs_data;
    while (my $v = each @$data) { push @pairs_data, $v; }

    is_deeply([ sort @pairs_tie ], [ sort @pairs_data ], "sorted values");
};

done_testing();
