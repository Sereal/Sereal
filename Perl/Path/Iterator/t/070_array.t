#!perl
use strict;
use warnings;

use Test::More;
use Data::Dumper;
use Test::Exception;
use Sereal::Path::Iterator;
use Sereal::Encoder qw/encode_sereal/;

subtest "simple document", sub {
    my $spi = Sereal::Path::Iterator->new(encode_sereal(100));
    lives_ok(sub { $spi->array_exists(0) }, 'expect array_exists() to live');
};

subtest "exists", sub {
    my $spi = Sereal::Path::Iterator->new(encode_sereal(
        [ 1, 2, 3, 4, 5 ],
    ));

    my @indexes = (
        [ 0, 1 ],
        [ 1, 1 ],
        [ 2, 1 ],
        [ 3, 1 ],
        [ 4, 1 ],

        [ -1, 1 ],
        [ -2, 1 ],
        [ -3, 1 ],
        [ -4, 1 ],
        [ -5, 1 ],

        [ 5, 0 ],
        [ -6, 0 ],
    );

    $spi->step_in();
    foreach (@indexes) {
        is(
            $spi->array_exists($_->[0]),
            $_->[1],
            $_->[1] ? "index $_->[0] exists" : "index $_->[0] does not exist"
        );
    }
};

subtest "go to positive indexes", sub {
    my $spi = Sereal::Path::Iterator->new(encode_sereal(
        [ 1, 2, 3, 4, 5 ],
    ));

    $spi->step_in();
    lives_ok(sub { $spi->array_goto(1) }, 'expect array_goto() to live');
    is($spi->decode(), 2, 'decode 2');
    lives_ok(sub { $spi->array_goto(2) }, 'expect array_goto() to live');
    is($spi->decode(), 3, 'decode 3');
    lives_ok(sub { $spi->array_goto(4) }, 'expect array_goto() to live');
    is($spi->decode(), 5, 'decode 5');
};

subtest "go to negative indexes", sub {
    my $spi = Sereal::Path::Iterator->new(encode_sereal(
        [ 1, 2, 3, 4, 5 ],
    ));

    $spi->step_in();
    lives_ok(sub { $spi->array_goto(-4) }, 'expect array_goto() to live');
    is($spi->decode(), 2, 'decode 2');
    lives_ok(sub { $spi->array_goto(-3) }, 'expect array_goto() to live');
    is($spi->decode(), 3, 'decode 3');
    lives_ok(sub { $spi->array_goto(-1) }, 'expect array_goto() to live');
    is($spi->decode(), 5, 'decode 5');
};

subtest "go to backward", sub {
    my $spi = Sereal::Path::Iterator->new(encode_sereal(
        [ 1, 2, 3, 4, 5 ],
    ));

    $spi->step_in();
    lives_ok(sub { $spi->array_goto(3) }, 'expect array_goto() to live');
    lives_ok(sub { $spi->array_goto(1) }, 'expect array_goto() to live');
    is($spi->decode(), 2, 'decode 2');
};

subtest "go to non existent indexes", sub {
    my $spi = Sereal::Path::Iterator->new(encode_sereal([]));
    $spi->step_in();
    dies_ok(sub { $spi->array_goto(0) },  'expect array_goto to die on 0th index');
    dies_ok(sub { $spi->array_goto(-1) }, 'expect array_goto to die on -1th index');
};

done_testing();
