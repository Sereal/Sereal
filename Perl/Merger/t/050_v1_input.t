#!perl
use strict;
use warnings;
use Test::More;

use Sereal::Merger qw(:all);
use Sereal::Encoder;
use Sereal::Decoder qw(decode_sereal);

# protocol v1 offsets are relative to the start of the packet rather than
# the body, so the merge pass has to start at the first body tag, not at
# body_pos + 1 as it does for v2 and later
my $v1 = pack("H*", "3d73726c01006161"); # =srl, v1, no header, SHORT_BINARY_1 "a"
my $shared = [ 1, 2 ];
my $v1_struct = Sereal::Encoder->new({ protocol_version => 1 })
    ->encode([ "foo", { bar => $shared }, $shared ]); # REFP with a v1 offset

for my $dedupe (0, 1) {
    my $label = $dedupe ? "with dedupe_strings" : "plain";

    my $mrg = Sereal::Merger->new({ dedupe_strings => $dedupe });
    $mrg->append($v1);
    $mrg->append($v1_struct);
    is_deeply(decode_sereal($mrg->finish()), [ "a", [ "foo", { bar => [ 1, 2 ] }, [ 1, 2 ] ] ],
              "v1 input is merged ($label, append)");

    $mrg = Sereal::Merger->new({ dedupe_strings => $dedupe });
    $mrg->append_all([ $v1, $v1_struct ]);
    is_deeply(decode_sereal($mrg->finish()), [ "a", [ "foo", { bar => [ 1, 2 ] }, [ 1, 2 ] ] ],
              "v1 input is merged ($label, append_all)");
}

done_testing;
