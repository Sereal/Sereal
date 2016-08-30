#!perl
use strict;
use warnings;
use Data::Dumper;
use File::Spec;

# These tests use an installed Decoder (or respectively Encoder) to do
# bulk data testing.

use lib File::Spec->catdir(qw(t lib));
BEGIN {
    lib->import('lib')
        if !-d 't';
}

use Sereal::TestSet qw(:all);
use Sereal::BulkTest qw(:all);
use Test::More;
use Sereal::Encoder;

my $ok = have_encoder_and_decoder();
if (not $ok) {
    plan skip_all => 'Did not find right version of decoder';
}
else {
    my %opt = (
        bench => scalar(grep /^--bench$/, @ARGV),
    );
    run_bulk_tests(%opt);
}

pass();
done_testing();

