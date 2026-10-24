#!perl
use strict;
use warnings;
use Test::More;
use Sereal::Path;

my $header = "=\xF3rl\x06\x00";

subtest "array range with a large step" => sub {
    # REFN ARRAY with a declared count of I32_MAX, of which the first three
    # elements (1, 2, 3) are present: idx += step overflowed I32
    my $packet = $header . "\x28\x2b\xff\xff\xff\xff\x07\x01\x02\x03";
    my @got = eval { Sereal::Path->new($packet)->values('$[1::2147483647]') };
    is($@, "", "the query does not fail");
    is_deeply(\@got, [2], "and finds only the element at index 1");
};

done_testing;
