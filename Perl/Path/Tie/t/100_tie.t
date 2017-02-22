#!perl
use strict;
use warnings;

use Test::More;
use Test::Differences;
use Sereal::Path::Tie;
use Sereal::Path::Iterator;
use Sereal::Encoder qw/encode_sereal/;

subtest "simple access in array", sub {
    my $data = [
        { a => 1 },
        { a => 2 },
        [ "test" ],
        1,
        "hi",
    ];

    my $spi = Sereal::Path::Iterator->new(encode_sereal($data));
    my $tie = Sereal::Path::Tie->new($spi);
    is_deeply($tie,         $data,          'data array');
    is_deeply($tie->[0],    $data->[0],     'data[0]');
    is_deeply($tie->[1],    $data->[1],     'data[1]');
    is_deeply($tie->[2],    $data->[2],     'data[2]');
    is_deeply($tie->[3],    $data->[3],     'data[3]');
    is_deeply($tie->[4],    $data->[4],     'data[4]');
    is_deeply($tie->[5],    $data->[5],     'data[5]');

    is_deeply($tie->[0],    { a => 1 },     'data[0]');
    is_deeply($tie->[1]{a}, 2,              'data[1]{a}');
    is_deeply($tie->[2][0], "test",         'data[2][0]');
    is_deeply($tie->[3],    1,              'data[3]');
    is_deeply($tie->[4],    "hi",           'data[4]');

    is_deeply(exists $tie->[1], exists $data->[1], 'exists $data->[1]');
    is_deeply(exists $tie->[9], exists $data->[9], 'exists $data->[9]');
};

subtest "simple access in hash", sub {
    my $data = {
        foo => 1,
        bar => "abcdefg",
        baz => { a => 1 },
    };

    my $spi = Sereal::Path::Iterator->new(encode_sereal($data));
    my $tie = Sereal::Path::Tie->new($spi);

    is_deeply($tie,             $data,              "data hash");
    is_deeply($tie->{foo},      $data->{foo},       "data->{foo}");
    is_deeply($tie->{bar},      $data->{bar},       "data->{bar}");
    is_deeply($tie->{baz},      $data->{baz},       "data->{baz}");
    is_deeply($tie->{baz}->{a}, $data->{baz}->{a},  "data->{baz}->{a}");
    is_deeply($tie->{missing},  $data->{missing},   "data->{missing}");

    is_deeply($tie->{foo},      1,                  "data->{foo}");
    is_deeply($tie->{bar},      "abcdefg",          "data->{bar}");
    is_deeply($tie->{baz},      { a => 1},          "data->{baz}");
    is_deeply($tie->{baz}->{a}, 1,                  "data->{baz}->{a}");

    is_deeply(exists $tie->{foo}, exists $data->{foo}, 'exists $data->{foo}');
    is_deeply(exists $tie->{a},   exists $data->{a},   'exists $data->{a}');
};

subtest "simple access in scalar", sub {
    my $s = "string";
    my $data = \\\$s;
    my $spi = Sereal::Path::Iterator->new(encode_sereal($data));
    my $tie = Sereal::Path::Tie->new($spi);

    eq_or_diff($tie,             $data,              '$data');
    eq_or_diff($$tie,            $$data,             '$$data');
    eq_or_diff($$$tie,           $$$data,            '$$$data');
    eq_or_diff($$$$tie,          $$$$data,           '$$$$data');
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

    my $spi = Sereal::Path::Iterator->new(encode_sereal($data));
    my $tie = Sereal::Path::Tie->new($spi);
    is_deeply([sort keys %$tie], [sort keys %$data], "sort keys data");

    my (@pairs_tie, @pairs_data);
    while (my ($k, $v) = each %$tie) { push @pairs_tie, [$k, $v]; }
    @pairs_tie = sort { $a->[0] cmp $b->[0] } @pairs_tie;
    while (my ($k, $v) = each %$data) { push @pairs_data, [$k, $v]; }
    @pairs_data = sort { $a->[0] cmp $b->[0] } @pairs_data;
    is_deeply(\@pairs_tie, \@pairs_tie, "sorted kv pairs");

    my $data1 = {
        a => 1,
        b => 2,
    };

    my $spi1 = Sereal::Path::Iterator->new(encode_sereal($data1));
    my $tie1 = Sereal::Path::Tie->new($spi1);
    is_deeply([sort values %$tie1], [sort values %$data1], "sort values data");
};

subtest "simple iteration over array", sub {
    my $data = [ 'foo', 'bar', 'baz' ];
    my $spi = Sereal::Path::Iterator->new(encode_sereal($data));
    my $tie = Sereal::Path::Tie->new($spi);
    is_deeply([sort @$tie], [sort @$data], "sort data");

    my (@pairs_tie, @pairs_data);
    push @pairs_tie, $_ foreach @$tie;
    push @pairs_data, $_ foreach @$data;
    is_deeply(\@pairs_tie, \@pairs_data, "sorted values");
};

done_testing();
