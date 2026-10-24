#!perl
use strict;
use warnings;
use Test::More;

use Sereal::Merger qw(:all);
use Sereal::Encoder;
use Sereal::Decoder qw(decode_sereal);

# REFN, WEAKEN, OBJECTV and PAD continue with the next tag in the same
# call; they must not count as another level of nesting that is never
# given back, or a wide array of references hits the recursion limit
my $data = [ map { \(my $x = $_) } 1 .. 300 ];
my $doc  = Sereal::Encoder->new->encode($data);

my $mrg = Sereal::Merger->new({ max_recursion_depth => 10 });
ok(eval { $mrg->append($doc); 1 }, "an array of 300 references merges with a recursion limit of 10")
    or diag $@;
is_deeply(decode_sereal($mrg->finish()), [ $data ], "and decodes");

# nesting is still limited
my $deep = Sereal::Encoder->new->encode([ [ [ [ [ [ [ [ [ [ [ 1 ] ] ] ] ] ] ] ] ] ] ]);
$mrg = Sereal::Merger->new({ max_recursion_depth => 10 });
ok(!eval { $mrg->append($deep); 1 }, "eleven nested arrays exceed a recursion limit of 10");
like($@, qr/recursion limit/, "with the recursion limit error");

done_testing;
