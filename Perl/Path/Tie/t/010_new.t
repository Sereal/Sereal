#!perl

use strict;
use warnings;

use Test::More;
use Test::Exception;
use Sereal::Path::Tie;
use Sereal::Path::Iterator;
use Sereal::Encoder qw/encode_sereal/;

subtest "create new Sereal::Path::Tie", sub {
    my $spi = Sereal::Path::Iterator->new(encode_sereal({}));
    lives_ok(sub { Sereal::Path::Tie->new($spi); }, "expect ->new() to live");
};

done_testing;
