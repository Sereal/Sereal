#!perl
use strict;
use warnings;
use Test::More tests => 1;
use File::Spec;
use lib File::Spec->catdir(qw(t lib));
BEGIN {
    lib->import('lib')
        if !-d 't';
}
use Sereal::TestSet;
use Sereal::Encoder qw(encode_sereal);

eval { encode_sereal(\1, { sort_keys => 1, stringify_unknown => 1 }); };
ok !$@, "We shouldn't die on sort_keys combined with stringify_unknown";


