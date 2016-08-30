#!perl
use strict;
use warnings;
use File::Spec;

use lib File::Spec->catdir(qw(t lib));
BEGIN {
    lib->import('lib')
        if !-d 't';
}

use Test::More;
use Sereal::TestSet;
use Sereal::Encoder qw(encode_sereal);

my $recur_depth = 1000;
my $ref = [];
my $pos = $ref;
$pos = $pos->[0] = [] for 1..$recur_depth-1;

my $out = encode_sereal($ref, {max_recursion_depth => $recur_depth+1});
pass("alive");
my $no_exception = eval {
    $out = encode_sereal($ref, {max_recursion_depth => $recur_depth-1});
    1
};
ok(!$no_exception);

done_testing();
note("All done folks!");

