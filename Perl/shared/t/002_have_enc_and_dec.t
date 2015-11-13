use strict;
use warnings;

use Test::More;
use File::Spec;
use lib File::Spec->catdir(qw(t lib));
BEGIN {
    lib->import('lib')
        if !-d 't';
}
use Sereal::TestSet qw(:all);

if (have_encoder_and_decoder()) {
    plan tests => 1;
} else {
    plan skip_all => 'Must have both encoder and decoder to run this test.';
}
diag "Testing with both encoder and decoder.";
diag "Sereal::Decoder v$Sereal::Decoder::VERSION";
diag "Sereal::Encoder v$Sereal::Encoder::VERSION";
ok(1);

