#!perl
use strict;
use warnings;

use Sereal::Path;
use Sereal::Encoder qw/encode_sereal/;
use Test::More;
use Test::Deep;
use Data::Dumper;

sub hobodecode {
    return unless defined $_[0];
    open my $fh, "| $^X -Mblib=../Encoder -Mblib=../Decoder ../shared/author_tools/hobodecoder.pl -e" or die $!;
    print $fh $_[0];
    close $fh;
}

my $verify_by_json_path = $ENV{VERIFY_BY_JSON_PATH} || 0;
require JSON::Path if $verify_by_json_path;

my %plain_data = (
    'plain array (single element queries)' => {
        dataset => [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ],
        queries => [ '$[0]', '$[3]', '$[9]', '$[-1]', '$[-5]', '$[-10]', '$[10]', '$[0:1]', '$[2:5]', '$[9:]', '$[:3]'  ],
        results => [ [1],     [4],    [10],   [10],    [6],     [1],      [],      [1],     [3,4,5],    [10],   [1,2,3] ],
    },
    'plain array (multiple element queries)' => {
        dataset => [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ],
        queries => [ '$[0,1]', '$[8,9]',  '$[9,8]',  '$[-1,-2]', '$[0,-1,1,-2]', '$[0,1,2,3,4,5,6,7,8,9]' ],
        results => [  [1,2],    [9,10],    [10,9],    [10,9],     [1,10,2,9],     [1,2,3,4,5,6,7,8,9,10]  ],
    },
    'plain array (walking)' => {
        dataset => [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ],
        queries => [ '$[*]' ],
        results => [ [1,2,3,4,5,6,7,8,9,10] ],
    },
    'plain hash (single element queries)' => {
        dataset => { a => 1, b => 2, c => 3 },
        queries => [ '$[a]', '$[b]', '$[c]', '$[d]' ],
        results => [  [1],    [2],    [3],    []    ]
    },
    'plain hash (multiple element queries)' => {
        dataset => { a => 1, b => 2, c => 3 },
        queries => [ '$[a,b,c]', '$[c,b,a]', '$[a,c]' ],
        results => [  [1,2,3],    [3,2,1],    [1,3] ]
    },
    'plaing hash (walking)' => {
        dataset => { a => 1, b => 2, c => 3 },
        queries => [ '$[*]'  ],
        results => [ [1,2,3] ],
        ignore_order => 1,
    },
    'array of hashes (walking)' => {
        dataset => [ { a => 1, b => 2 }, { a => 3, b => 4, c => 5 } ],
        queries => [ '$[*][a]', '$[*][b]', '$[*][a,b]', '$[*][c]', '$[*]' ],
        results => [  [1,3],     [2,4],     [1,2,3,4],   [5],       [ { a => 1, b => 2 }, { a => 3, b => 4, c => 5 } ] ]
    },
    'hash of arrays (walking)' => {
        dataset => { first => [ 0, 1, 2 ], second => [ 4, 5, 6 ], third => [ 7, 8 ] },
        queries => [ '$[*][0]', '$[*][1]', '$[*][0,1]',   '$[*][-1,-2]'  ],
        results => [  [0,4,7],   [1,5,8],   [0,1,4,5,7,8], [2,1,6,5,8,7] ],
        ignore_order => 1,
    },
);

foreach my $name (keys %plain_data) {
    my $ds = $plain_data{$name}{dataset};
    my $queries = $plain_data{$name}{queries};
    my $results = $plain_data{$name}{results};
    my $ignore_order = $plain_data{$name}{ignore_order};
    my $sp = Sereal::Path->new(encode_sereal($ds));

    for (my $i = 0; $i < @$queries; ++$i) {
        my $q = $queries->[$i];
        my $got = [ $sp->values($q) ];
        my $expected = $results->[$i];

        if ($ignore_order) {
            cmp_bag($got, $expected, "dataset '$name' query $q")
                or diag("got:\n" . Dumper($got) . "expected:\n" . Dumper($expected));
        } else {
            is_deeply($got, $expected, "dataset '$name' query $q")
                or diag("got:\n" . Dumper($got) . "expected:\n" . Dumper($expected));
        }

        if ($verify_by_json_path) {
            my @expected_json_path = JSON::Path->new($q)->values($ds);
            if ($ignore_order) {
                cmp_bag($got, \@expected_json_path, "dataset '$name' query $q verify by JSON::Path")
                    or diag("got:\n" . Dumper($got) . "expected_json_path:\n" . Dumper(\@expected_json_path));
            } else {
                is_deeply($got, \@expected_json_path, "dataset '$name' query $q verify by JSON::Path")
                    or diag("got:\n" . Dumper($got) . "expected_json_path:\n" . Dumper(\@expected_json_path));
            }
        }
    }
}

done_testing();
