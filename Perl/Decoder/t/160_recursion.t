#!perl
use strict;
use warnings;
use Sereal::Decoder            qw(decode_sereal);
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
my $s           = Header();
$s .= chr( SRL_HDR_ARRAYREF + 1 ) for 1 .. $recur_depth;
$s .= integer(1);

my $out = decode_sereal( $s, { max_recursion_depth => $recur_depth + 1 } );
pass("alive");
my $no_exception = eval {
    $out = decode_sereal( $s, { max_recursion_depth => $recur_depth - 1 } );
    1;
};
ok( !$no_exception );

# Plain REFN+ARRAY pairs also recurse, even though they are not the compact
# ARRAYREF tags emitted for ordinary Perl array references. Every value-reader
# call must count toward the limit so malformed packets cannot bypass it.
# (A bare ARRAY tag without its REFN is rejected outright, see t/933.)
my $plain_arrays = Header();
$plain_arrays .= chr(SRL_HDR_REFN) . chr(SRL_HDR_ARRAY) . chr(1) for 1 .. 1000;
$plain_arrays .= integer(1);
my $plain_arrays_ok = eval {
    decode_sereal( $plain_arrays, { max_recursion_depth => 1 } );
    1;
};
ok( !$plain_arrays_ok, 'plain ARRAY nesting is subject to recursion limit' );
like( $@, qr/Reached recursion limit/, 'plain ARRAY nesting reports the recursion limit' );

done_testing();
note("All done folks!");

