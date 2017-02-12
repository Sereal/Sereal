#!perl
use strict;
use warnings;

use Test::More;
use Data::Dumper;
use Test::Exception;
use Sereal::Path::Iterator;
use Sereal::Encoder qw/encode_sereal/;

subtest "step out from simple item", sub {
    my $spi = Sereal::Path::Iterator->new(encode_sereal(70));
    dies_ok(sub { $spi->step_out() }, 'expect step out to die');
};

subtest "step out from simple ARRAY", sub {
    my $spi = Sereal::Path::Iterator->new(encode_sereal([]));
    $spi->step_in();
    lives_ok(sub { $spi->step_out() }, 'expect step out to live');
    dies_ok(sub { $spi->next() }, 'expect step next to die');
};

subtest "step out from simple HASH", sub {
    my $spi = Sereal::Path::Iterator->new(encode_sereal({ foo => 'bar' }));
    $spi->step_in();
    lives_ok(sub { $spi->step_out() }, 'expect step out to live');
    dies_ok(sub { $spi->next() }, 'expect step next to die');
};

subtest "step out from empty HASH", sub {
    my $spi = Sereal::Path::Iterator->new(encode_sereal({}));
    $spi->step_in();
    lives_ok(sub { $spi->step_out() }, 'expect step out to live');
};

subtest "step out from nested ARRAY", sub {
    my $spi = Sereal::Path::Iterator->new(encode_sereal(
        [ [ [ 100 ] ], 200 ]
    ));

    $spi->step_in(3);
    lives_ok(sub { $spi->step_out(2) }, 'expect step out to live');
    is($spi->decode(), 200, 'decode 200');
};

subtest "step out from reference", sub {
    my $spi = Sereal::Path::Iterator->new(encode_sereal(
        [ 100, \\\'scalar', 'end' ]
    ));

    $spi->step_in();
    $spi->next();
    $spi->step_in(3);
    lives_ok(sub { $spi->step_out(3) }, 'expect step out to live');
    is($spi->decode(), 'end', 'decode end');
};

subtest "step out from REFP", sub {
    my $a = [ 200, 300, 400 ];
    my $spi = Sereal::Path::Iterator->new(encode_sereal(
        [ $a, $a, 1000 ]
    ));

    $spi->step_in();
    $spi->next();
    $spi->step_in();
    is($spi->decode(), 200, 'decode 200');
    lives_ok(sub { $spi->step_out() }, 'expect step out to live');
    is($spi->decode(), 1000, 'decode 1000');
};

subtest "step out from blessed reference", sub {
    my $spi = Sereal::Path::Iterator->new(encode_sereal(
        [ bless({}, "Foo"), 'end' ]
    ));

    $spi->step_in(2);
    lives_ok(sub { $spi->step_out() }, 'expect step out to live');
    is($spi->decode(), 'end', 'decode end');
};

subtest "step out from nested blessed reference", sub {
    my $spi = Sereal::Path::Iterator->new(encode_sereal(
        [ bless([[[]]], "Foo"), 'end' ]
    ));

    $spi->step_in(4);
    lives_ok(sub { $spi->step_out(3) }, 'expect step out to live');
    is($spi->decode(), 'end', 'decode end');
};

done_testing();
