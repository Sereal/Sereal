#!perl
use strict;
use warnings;

use Test::More;
use Test::Exception;
use Sereal::Path::Iterator;
use Sereal::Encoder qw/encode_sereal/;

subtest "descode simple item", sub {
    my $spi = Sereal::Path::Iterator->new(encode_sereal(100));
    is($spi->decode(), 100, 'decode 100');
};

subtest "decode REFP", sub {
    my $a = [ 1, 2, 3 ];
    my $spi = Sereal::Path::Iterator->new(encode_sereal(
        [ $a, $a ],
    ));

    $spi->step_in();
    $spi->next();
    is_deeply($spi->decode(), $a, 'decode array');
};

subtest "decode nested REFP", sub {
    my $a = [ 1, 2, 3 ];
    my $b = [ 4, 5, 6, $a ];
    my $spi = Sereal::Path::Iterator->new(encode_sereal(
        [ $a, $b ],
    ));

    $spi->step_in();
    $spi->next();
    is_deeply($spi->decode(), $b, 'decode array');
};

subtest "decode COPY", sub {
    my $spi = Sereal::Path::Iterator->new(encode_sereal(
        [ 'long_test_string', 'long_test_string' ],
        { dedupe_strings => 1 },
    ));

    $spi->step_in(2);
    is($spi->decode(), 'long_test_string', 'decode string');
};

subtest "decode OBJECTV", sub {
    my $spi = Sereal::Path::Iterator->new(encode_sereal(
        [ (bless { a => 1 }, "Foo"), (bless { b => 1 }, "Foo") ],
    ));

    $spi->step_in();
    $spi->next();
    is_deeply($spi->decode(), { b => 1 }, 'decode blessed');
};

subtest "double decode of OBJECTV", sub {
    my $spi = Sereal::Path::Iterator->new(encode_sereal(
        [ (bless {}, "Foo"), (bless {}, "Foo") ],
    ));

    $spi->step_in();
    $spi->next();
    is_deeply($spi->decode(), {}, 'decode blessed');
    is_deeply($spi->decode(), {}, 'decode blessed');
};

subtest "decode ALIAS", sub {
    my $spi = Sereal::Path::Iterator->new(encode_sereal(
        [ 'long_test_string', 'long_test_string', [ 'long_test_string' ] ],
        { dedupe_strings => 1, aliased_dedupe_strings => 1 },
    ));

    $spi->step_in(2);
    is($spi->decode(), 'long_test_string', 'decode aliased string directly');
    $spi->next();
    is_deeply($spi->decode(), [ 'long_test_string' ], 'decode aliased indirectly');
};

done_testing();
