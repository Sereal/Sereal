#!perl
use strict;
use warnings;

use File::Spec;
use Scalar::Util qw(refaddr reftype);
use lib File::Spec->catdir(qw(t lib));
BEGIN {
  lib->import('lib')
    if !-d 't';
}
use Sereal::TestSet qw(:all);

use Sereal::Encoder qw(encode_sereal);
use Sereal::Encoder::Constants qw(:all);
use Data::Dumper; # must be loaded AFTER the test set (bug in perl)
use Test::More;

my $ok = have_encoder_and_decoder();
if (not $ok) {
    plan skip_all => 'Did not find right version of decoder';
}
else {
    my $dup  = "bad" x 100;
    my $dup2 = "beef" x 100;
    my $enc = Sereal::Encoder->new({aliased_dedupe_strings => 1});
    my $encoded = $enc->encode([$dup,"a",$dup2,"b",$dup,"c",$dup2,"d"]);
    my $decoded = Sereal::Decoder::decode_sereal($encoded);
    is($decoded->[0],$dup);
    is($decoded->[2],$dup2);
    is(refaddr(\$decoded->[0]),refaddr(\$decoded->[4]),"expected same reference for decoded->[0] and decoded->[2]");
    is(refaddr(\$decoded->[2]),refaddr(\$decoded->[6]),"expected same reference for decoded->[2] and decoded->[6]");
}
done_testing();
