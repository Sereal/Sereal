#!perl
use strict;
use warnings;

use Test::More;
use Sereal::Path::Tie;
use Sereal::Encoder qw/encode_sereal/;

#=============================================================================================#

my $data = [
    { a => 1 },
    { a => 2 },
];

my $tie = Sereal::Path::Tie::parse(encode_sereal($data));
is_deeply($tie,         $data,          'data array');
is_deeply($tie->[0],    $data->[0],     'data[0]');
is_deeply($tie->[1],    $data->[1],     'data[1]');
is_deeply($tie->[0],    { a => 1 },     'data[0]');
is_deeply($tie->[1]{a}, 2,              'data[1]{a}');

#=============================================================================================#

$data = {
    foo => 1,
    bar => "abcdefg",
    baz => { a => 1 },
};
$tie = Sereal::Path::Tie::parse(encode_sereal($data));

is_deeply($tie,             $data,              "data hash");
is_deeply($tie->{foo},      $data->{foo},       "data->{foo}");
is_deeply($tie->{foo},      1,                  "data->{foo}");
is_deeply($tie->{bar},      $data->{bar},       "data->{bar}");
is_deeply($tie->{bar},      "abcdefg",          "data->{bar}");
is_deeply($tie->{baz},      $data->{baz},       "data->{baz}");
is_deeply($tie->{baz},      { a => 1},          "data->{baz}");
is_deeply($tie->{baz}->{a}, $data->{baz}->{a},  "data->{baz}->{a}");
is_deeply($tie->{baz}->{a}, 1,                  "data->{baz}->{a}");

#=============================================================================================#

TODO: {
    local $TODO = "It looks like there is still a bug there. Test failures are random, not always reproducible";
#    $data = {
#        boo => [1,"hi",[],3,4],
#    };
#    $tie = Sereal::Path::Tie::parse(encode_sereal($data));
#
#    is_deeply($tie,             $data,              "data hash");
#    is_deeply($tie->{boo},      $data->{boo},       "data->{boo}");
#    is_deeply($tie->{boo},      [1,"hi",[],3,4],          "data->{boo}");

    #=============================================================================================#

    $data = {
        c1 => { f => 1 },
        c2 => {
            a => {
                b => 1,
            },
        },
    };
    $tie = Sereal::Path::Tie::parse(encode_sereal($data));


    is_deeply([sort keys %$tie], [sort keys %$data], "sort keys data");
    is_deeply($tie, $data, "all data");
}
done_testing();
