#!perl
use strict;
use warnings;
use Sereal::Decoder qw(decode_sereal);
use Sereal::Decoder::Constants qw(:all);
use File::Spec;

use lib File::Spec->catdir(qw(t lib));
BEGIN {
    lib->import('lib')
        if !-d 't';
}

use Sereal::TestSet qw(:all);

use Test::More;

my $recur_depth = 1000;
my $s = Header();
$s .= chr(SRL_HDR_ARRAYREF + 1) for 1..$recur_depth;
$s .= integer(1);

my $out = decode_sereal($s, {max_recursion_depth => $recur_depth+1});
pass("alive");
my $no_exception = eval {
    $out = decode_sereal($s, {max_recursion_depth => $recur_depth-1});
    1
};
ok(!$no_exception);

done_testing();
note("All done folks!");

