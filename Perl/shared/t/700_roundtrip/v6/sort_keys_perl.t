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

run_roundtrip_profile_tests( 'sort_keys_perl', { sort_keys => 2 } );

pass();
done_testing();

