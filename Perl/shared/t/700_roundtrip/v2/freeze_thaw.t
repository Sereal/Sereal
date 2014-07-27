#!perl
use strict;
use warnings;
use Sereal::Decoder;
use Data::Dumper;
use File::Spec;

# These tests use an installed Decoder (or respectively Encoder) to do
# round-trip testing. There are two strategies, both with drawbacks:
# - Test::More's is_deeply is waaaay too lenient to catch all the
#   subtleties that Sereal is supposed to encode.
# - Serialize - Deserialize - Serialize, then do a string compare.
#   This won't catch if the first serialization has bogus output
#   but the subsequent de- & serialization work for the already
#   bogus output.
# These tests can't replace carefully crafted manual tests, I fear.

use lib File::Spec->catdir(qw(t lib));
BEGIN {
    lib->import('lib')
        if !-d 't';
}

use Sereal::TestSet qw(:all);
use Test::More;

my $ok = have_encoder_and_decoder();
if (not $ok) {
    plan skip_all => 'Did not find right version of encoder';
}
else {
    run_roundtrip_tests(
        'freeze-thaw',    { freeze_callbacks => 1 }
    );
}


pass();
done_testing();

