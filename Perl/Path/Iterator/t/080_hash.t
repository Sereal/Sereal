#!perl
use strict;
use warnings;

use Test::More;
use Data::Dumper;
use Test::Exception;
use Sereal::Path::Iterator;
use Sereal::Encoder qw/encode_sereal/;

subtest "hash keys", sub {
    my $spi = Sereal::Path::Iterator->new(encode_sereal(
        { map { ($_ => undef) } qw/1 2 3 4 5 6 7 8 9/ },
        { sort_keys => 2 }
    ));

    $spi->step_in();
    foreach (qw/1 2 3 4 5 6 7 8 9/) {
        is($spi->hash_key(), $_, "hash key is $_");
        $spi->next();
    }
};

subtest "dedpulicated hash key", sub {
    my $spi = Sereal::Path::Iterator->new(encode_sereal(
        [
            { foo => 'bar1', foofoo => 'barbar1' },
            { foo => 'bar2', foofoo => 'barbar2' },
        ],
        { sort_keys => 2 },
    ));

    $spi->step_in();
    $spi->next();
    $spi->step_in();
    is($spi->hash_key(), 'foo', "hash key is foo");
    is($spi->decode(), 'bar2', "hash value is bar2");
    $spi->next();
    is($spi->hash_key(), 'foofoo', "hash key is bar");
    is($spi->decode(), 'barbar2', "hash value is barbar2");
};

subtest "hash key exists", sub {
    my $spi = Sereal::Path::Iterator->new(encode_sereal(
        { map { ($_ => undef) } qw/1 2 3 4 5 6 7 8 9/ },
        { sort_keys => 2 }
    ));

    $spi->step_in();
    foreach (qw/1 2 3 4 5 6 7 8 9/) {
        ok($spi->hash_exists($_),  "hash key $_ exists");
    }

    is($spi->hash_exists('nonexistent'), 0, "hash key 'nonexistent' does not exist");
};

done_testing();
