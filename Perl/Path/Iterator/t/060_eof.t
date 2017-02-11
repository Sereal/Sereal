#!perl
use strict;
use warnings;

use Test::More;
use Data::Dumper;
use Sereal::Path::Iterator;
use Sereal::Encoder qw/encode_sereal/;

subtest "simple", sub {
    my $spi = Sereal::Path::Iterator->new(encode_sereal(70));
    $spi->next();
    is($spi->eof(), 1, 'expect eof');
};

subtest "more comprex", sub {
    my $a = [ 1, 2, 3 ];
    my $spi = Sereal::Path::Iterator->new(encode_sereal(
        [ $a, $a ]
    ));

    $spi->step_in();
    $spi->next();
    $spi->step_in();
    $spi->step_out();
    is($spi->eof(), 1, 'expect eof');
};

done_testing();
