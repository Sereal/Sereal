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
        { sort_keys => 1 }
    ));

    $spi->step_in();
    foreach (qw/1 2 3 4 5 6 7 8 9/) {
        is($spi->hash_key(), $_, "hash key is $_");
        $spi->next();
    }
};

subtest "hash key exists", sub {
    my $spi = Sereal::Path::Iterator->new(encode_sereal(
        { map { ($_ => undef) } qw/1 2 3 4 5 6 7 8 9/ },
        { sort_keys => 1 }
    ));

    $spi->step_in();
    foreach (qw/1 2 3 4 5 6 7 8 9/) {
        ok($spi->hash_exists($_),  "hash key $_ exists");
    }

    is($spi->hash_exists('nonexistent'), 0, "hash key 'nonexistent' does not exist");
};

done_testing();
