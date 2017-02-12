#!perl
use strict;
use warnings;

use Test::More;
use Data::Dumper;
use Test::Exception;
use Sereal::Path::Iterator;
use Sereal::Encoder qw/encode_sereal/;

subtest "simple rewind", sub {
    my $spi = Sereal::Path::Iterator->new(encode_sereal(70));
    $spi->next();
    lives_ok(sub { $spi->rewind() }, 'expect rewind to live');
    is($spi->decode(), 70, 'decode 70');
};

subtest "rewind in empty array", sub {
    my $spi = Sereal::Path::Iterator->new(encode_sereal([]));
    $spi->step_in();
    lives_ok(sub { $spi->rewind() }, 'expect rewind to live');
};

subtest "rewind in nested array", sub {
    my $spi = Sereal::Path::Iterator->new(encode_sereal(
        [ 100, [ 200, 300 ], 400 ],
    ));
    $spi->step_in(5);
    lives_ok(sub { $spi->rewind() }, 'expect rewind to live');
    is($spi->decode(), 200, 'decode 200');
};

subtest "rewind in reference", sub {
    my $spi = Sereal::Path::Iterator->new(encode_sereal(\'scalar'));
    $spi->step_in(2);
    lives_ok(sub { $spi->rewind() }, 'expect rewind to live');
    is($spi->decode(), 'scalar', 'decode scalar');
};

subtest "rewind in REFP", sub {
    my $a = [ 100, 200, 300 ];
    my $spi = Sereal::Path::Iterator->new(encode_sereal(
        [ $a, $a ],
    ));

    $spi->step_in();
    $spi->next();
    $spi->step_in();

    lives_ok(sub { $spi->rewind() }, 'expect rewind to live');
    is($spi->decode(), 100, 'decode 100');
};

subtest "rewind with step out", sub {
    my $spi = Sereal::Path::Iterator->new(encode_sereal(
        [ 100, [ 200, 300 ], 400 ],
    ));

    $spi->step_in(5);
    lives_ok(sub { $spi->rewind(1) }, 'expect rewind to live');
    is($spi->decode(), 100, 'decode 100');
};

done_testing();
