#!perl
use strict;
use warnings;
use Sereal::Encoder qw(:all);
use File::Spec;
use Scalar::Util qw( blessed );
use lib File::Spec->catdir(qw(t lib));
BEGIN {
    lib->import('lib')
        if !-d 't';
}

use Sereal::TestSet qw(:all);
use Test::More;

my $ref = Header(2, chr(0b0000_1100)) . chr(0b0001_0000); # -16 in body, 12 in header
is(encode_sereal_with_header_data(-16, 12), $ref, "Encode 12 in header, -16 in body");

my $ok = have_encoder_and_decoder();
if (not $ok) {
    #plan skip_all => 'Did not find right version of encoder';
}
else {
}

done_testing();

