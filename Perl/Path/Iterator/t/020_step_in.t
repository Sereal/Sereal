#!perl
use strict;
use warnings;

use Test::More;
use Data::Dumper;
use Test::Exception;
use Sereal::Path::Iterator;
use Sereal::Encoder qw/encode_sereal/;

subtest "step over simple item", sub {
    my $spi = Sereal::Path::Iterator->new(encode_sereal(70));
    lives_ok(sub { $spi->step_in() }, 'expect step in to live');
};

subtest "step into ARRAY", sub {
    my $spi = Sereal::Path::Iterator->new(encode_sereal(
        [ 70, 71 ],
    ));

    lives_ok(sub { $spi->step_in() }, 'expect step in to live');
    is($spi->decode(), 70, 'decode 70');
    lives_ok(sub { $spi->step_in() }, 'expect step in to live');
    is($spi->decode(), 71, 'decode 71');
    lives_ok(sub { $spi->step_in() }, 'expect step in to live');
    dies_ok(sub { $spi->step_in() }, 'expect step in to die');
};

subtest "step into HASH", sub {
    my $spi = Sereal::Path::Iterator->new(encode_sereal(
        { foo => 'bar' }
    ));

    lives_ok(sub { $spi->step_in() }, 'expect step in to live');
    is($spi->decode(), 'foo', 'decode foo');
    lives_ok(sub { $spi->step_in() }, 'expect step in to live');
    is($spi->decode(), 'bar', 'decode bar');
    lives_ok(sub { $spi->step_in() }, 'expect step in to live');
    dies_ok(sub { $spi->step_in() }, 'expect step in to die');
};

subtest "step into long ARRAY", sub {
    my $spi = Sereal::Path::Iterator->new(encode_sereal(
        [ 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30 ],
    ));

    for (10..30) {
        lives_ok(sub { $spi->step_in() }, 'expect step in to live');
        is($spi->decode(), $_, "decode $_");
    }

    lives_ok(sub { $spi->step_in() }, 'expect step in to live');
    dies_ok(sub { $spi->step_in() }, 'expect step in to die');
};

subtest "step into long HASH", sub {
    my $spi = Sereal::Path::Iterator->new(encode_sereal(
        {
            10 => 110,
            11 => 111,
            12 => 112,
            13 => 113,
            14 => 114,
            15 => 115,
            16 => 116,
            17 => 117,
            18 => 118,
            19 => 119,
            20 => 120,
            21 => 121,
            22 => 122,
            23 => 123,
            24 => 124,
            25 => 125,
            26 => 126,
            27 => 127,
            28 => 128,
            29 => 129,
            30 => 130,
        },
        { sort_keys => 2 },
    ));

    for (10..30) {
        my ($k, $v) = ($_, 100+$_);
        lives_ok(sub { $spi->step_in() }, 'expect step in to live');
        is($spi->decode(), $k, "decode key $k");
        lives_ok(sub { $spi->step_in() }, 'expect step in to live');
        is($spi->decode(), $v, "decode value $v");
    }

    lives_ok(sub { $spi->step_in() }, 'expect step in to live');
    dies_ok(sub { $spi->step_in() }, 'expect step in to die');
};

subtest "step into reference", sub {
    my $spi = Sereal::Path::Iterator->new(encode_sereal(
        [ 'a', \[ 'b', \\'c' ] ]
    ));

    $spi->step_in();
    is($spi->decode(), 'a', 'decode a');
    lives_ok(sub { $spi->step_in(3) }, 'expect step in to live');
    is($spi->decode(), 'b', 'decode b');
    lives_ok(sub { $spi->step_in(3) }, 'expect step in to live');
    is($spi->decode(), 'c', 'decode c');
    lives_ok(sub { $spi->step_in() }, 'expect step in to live');
    dies_ok(sub { $spi->step_in() }, 'expect step in to die');
};

subtest "step into REFP", sub {
    my $a = [ 100 ];
    push @{ $a }, $a;
    my $spi = Sereal::Path::Iterator->new(encode_sereal($a));

    $spi->step_in();
    is($spi->decode(), '100', 'decode 100');
    lives_ok(sub { $spi->step_in(2) }, 'expect step in to live');
    is($spi->decode(), '100', 'decode 100');
    lives_ok(sub { $spi->step_in(2) }, 'expect step in to live');
    is($spi->decode(), '100', 'decode 100');
    # and so on, cyclic reference
};

subtest "step over COPY", sub {
    my $spi = Sereal::Path::Iterator->new(encode_sereal(
        { foo => { foo => 'bar' } }
    ));

    $spi->step_in();
    is($spi->decode(), 'foo', 'decode foo');
    lives_ok(sub { $spi->step_in(2) }, 'expect step in to live');
    is($spi->decode(), 'foo', 'decode foo');
    lives_ok(sub { $spi->step_in() }, 'expect step in to live');
    is($spi->decode(), 'bar', 'decode bar');
    lives_ok(sub { $spi->step_in() }, 'expect step in to live');
    dies_ok(sub { $spi->step_in() }, 'expect step in to die');
};

subtest "step into blessed ARRAY", sub {
    my $spi = Sereal::Path::Iterator->new(encode_sereal(
        bless [ 200 ], "Foo"
    ));

    lives_ok(sub { $spi->step_in() }, 'expect step in to live');
    is($spi->decode(), '200', 'decode 200');
    lives_ok(sub { $spi->step_in() }, 'expect step in to live');
    dies_ok(sub { $spi->step_in() }, 'expect step in to die');
};

subtest "step into blessed HASH", sub {
    my $spi = Sereal::Path::Iterator->new(encode_sereal(
        bless { foo => 'bar' }, "Foo"
    ));

    lives_ok(sub { $spi->step_in() }, 'expect step in to live');
    is($spi->decode(), 'foo', 'decode foo');
    lives_ok(sub { $spi->step_in() }, 'expect step in to live');
    is($spi->decode(), 'bar', 'decode bar');
    lives_ok(sub { $spi->step_in() }, 'expect step in to live');
    dies_ok(sub { $spi->step_in() }, 'expect step in to die');
};

subtest "step into reference to blessed object", sub {
    my $b = bless [ 200 ], "Foo";
    my $spi = Sereal::Path::Iterator->new(encode_sereal(\$b));
    lives_ok(sub { $spi->step_in(2) }, 'expect step in to live');
    is($spi->decode(), '200', 'decode 200');
    lives_ok(sub { $spi->step_in() }, 'expect step in to live');
    dies_ok(sub { $spi->step_in() }, 'expect step in to die');
};

subtest "step into blessed blessed ARRAY (OBJECTV)", sub {
    my $spi = Sereal::Path::Iterator->new(encode_sereal(
        bless(\(bless [ 200 ], "Foo"), "Foo")
    ));

    lives_ok(sub { $spi->step_in(2) }, 'expect step in to live');
    is($spi->decode(), '200', 'decode 200');
    lives_ok(sub { $spi->step_in() }, 'expect step in to live');
    dies_ok(sub { $spi->step_in() }, 'expect step in to die');
};

subtest "step into ALIAS of scalar", sub {
    my $spi = Sereal::Path::Iterator->new(encode_sereal(
        [ 'long_test_string', 'long_test_string', 'another_string' ],
        { dedupe_strings => 1, aliased_dedupe_strings => 1 },
    ));

    lives_ok(sub { $spi->step_in(3) }, 'expect step in to live');
    is($spi->decode(), 'another_string', 'decode another_string');
};

# TODO step into ALIAS of reference/object

done_testing();
