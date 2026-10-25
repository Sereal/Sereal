#!perl
use strict;
use warnings;
use Data::Dumper;
use File::Spec;

use lib File::Spec->catdir(qw(t lib));

BEGIN {
    lib->import('lib')
        if !-d 't';
}

use Sereal::TestSet qw(:all);
use Test::More;

run_roundtrip_profile_tests(
    'zstd_force',
    sub { { compress => Sereal::Encoder::SRL_ZSTD(), compress_threshold => 0 } },
);

pass();
done_testing();

