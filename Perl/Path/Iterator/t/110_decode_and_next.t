#!perl
use strict;
use warnings;

use Test::More;
use Test::Exception;
use Sereal::Path::Iterator;
use Sereal::Encoder qw/encode_sereal/;

subtest "descode simple item", sub {
    my $spi = Sereal::Path::Iterator->new(encode_sereal(100));
    is($spi->decode_and_next(), 100, 'decode 100');
    is($spi->eof(), 1, "at EOF");
};

subtest "descode simple array", sub {
    my $spi = Sereal::Path::Iterator->new(encode_sereal([1, 2, 3]));
    $spi->step_in();
    is($spi->decode_and_next(), 1, 'decode 1');
    is($spi->decode_and_next(), 2, 'decode 2');
    is($spi->decode_and_next(), 3, 'decode 3');
};

subtest "descode nested array", sub {
    my $spi = Sereal::Path::Iterator->new(encode_sereal([0, [1, 2, 3], 4]));

    $spi->step_in();
    $spi->next();
    $spi->step_in();

    is($spi->decode_and_next(), 1, 'decode 1');
    is($spi->decode_and_next(), 2, 'decode 2');
    is($spi->decode_and_next(), 3, 'decode 3');
    dies_ok(sub { $spi->decode_and_next() }, 'expect next decode_and_next() to die');

    $spi->step_out();
    is($spi->decode_and_next(), 4, 'decode 4');
};

subtest "descode simple hash", sub {
    my $spi = Sereal::Path::Iterator->new(encode_sereal({ foo => 'bar' }));
    $spi->step_in();
    is($spi->decode_and_next(), 'foo', 'decode foo');
    is($spi->decode_and_next(), 'bar', 'decode bar');
};

done_testing();
