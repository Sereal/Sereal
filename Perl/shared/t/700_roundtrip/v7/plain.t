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

use Sereal::TestSet qw(run_roundtrip_version_tests);
use Test::More;

run_roundtrip_version_tests('plain');
pass();
done_testing();
