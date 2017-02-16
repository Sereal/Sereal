#!perl
use strict;
use warnings;

use Test::More;
use Data::Dumper;
use Test::Exception;
use Sereal::Path::Iterator;
use Sereal::Encoder qw/encode_sereal/;

subtest "step over simple item", sub {
    my $spi = Sereal::Path::Iterator->new(encode_sereal(
        [ 'abc', 123, undef, 1.5 ],
    ));

    $spi->step_in();
    is($spi->decode(), 'abc', 'decode abc');
    lives_ok(sub { $spi->next() }, 'expect step next to live');
    is($spi->decode(), 123, 'decode 123');
    lives_ok(sub { $spi->next(2) }, 'expect step next to live');
    is($spi->decode(), 1.5, 'decode 1.5');
    lives_ok(sub { $spi->next() }, 'expect step next to live');
    dies_ok(sub { $spi->next() }, 'expect step next to die');
};

subtest "step over regexp", sub {
    my $spi = Sereal::Path::Iterator->new(encode_sereal(qr/test/));
    lives_ok(sub { $spi->next() }, 'expect step next to live');
    dies_ok(sub { $spi->next() }, 'expect step next to die');
};

subtest "step over references", sub {
    my $spi = Sereal::Path::Iterator->new(encode_sereal(
        [
            { foo => 'bar' },
            [],
            \\\\'scalar',
            'end'
        ],
    ));

    $spi->step_in();
    lives_ok(sub { $spi->next(3) }, 'expect step next to live');
    is($spi->decode(), 'end', 'decode end');
    lives_ok(sub { $spi->next() }, 'expect step next to live');
    dies_ok(sub { $spi->next() }, 'expect step next to die');
};

subtest "step over REFP and COPY", sub {
    my $a = [ 1, 2, 3 ];
    my $h = { foo => 'bar' };
    my $spi = Sereal::Path::Iterator->new(encode_sereal(
        [
            $a,
            $a,
            $h,
            $h,
            'end'
        ],
    ));

    $spi->step_in();
    lives_ok(sub { $spi->next(4) }, 'expect step next to live');
    is($spi->decode(), 'end', 'decode end');
    lives_ok(sub { $spi->next() }, 'expect step next to live');
    dies_ok(sub { $spi->next() }, 'expect step next to die');
};

subtest "step over ALIAS", sub {
    my $spi = Sereal::Path::Iterator->new(encode_sereal(
        [ 'long_test_string', 'long_test_string', 'another_string' ],
        { dedupe_strings => 1, aliased_dedupe_strings => 1 },
    ));

    $spi->step_in();
    lives_ok(sub { $spi->next(2) }, 'expect step next to live');
    is($spi->decode(), 'another_string', 'decode another_string');
};

subtest "step over blessed references", sub {
    my $spi = Sereal::Path::Iterator->new(encode_sereal(
        [
            bless([], "Foo"),
            bless({}, "Foo"),
            'end',
        ]
    ));

    $spi->step_in();
    lives_ok(sub { $spi->next(2) }, 'expect step next to live');
    is($spi->decode(), 'end', 'decode end');
    lives_ok(sub { $spi->next() }, 'expect step next to live');
    dies_ok(sub { $spi->next() }, 'expect step next to die');
};

done_testing();
