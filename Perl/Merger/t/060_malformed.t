#!perl
use strict;
use warnings;
use Test::More;

use Sereal::Merger qw(:all);
use Sereal::Decoder qw(decode_sereal);

# merge the packets into one document; returns the document, or undef with
# the error in $@
sub merge_packets {
    my ($opt, @packets) = @_;
    my $mrg = Sereal::Merger->new($opt);
    return eval { $mrg->append($_) for @packets; $mrg->finish() };
}

sub merge_fails {
    my ($hex, $error, $name) = @_;
    my $out = merge_packets({}, pack("H*", $hex));
    ok(!defined $out, "$name is rejected");
    like($@, $error, "$name: error message");
}

# =srl, v6, no header
my $v6 = "3df3726c0600";

subtest "regexp modifiers" => sub {
    # REFN REGEXP "a", then the modifiers tag 0xdf: a HASHREF_15 with the
    # track bit set, not a SHORT_BINARY, but it used to pass for one and
    # 32 bytes were copied from the 11 byte input
    merge_fails("${v6}28316161df", qr/while expecting SRL_HDR_SHORT_BINARY/,
                "track bit set on a non-string modifiers tag");
    # REFN REGEXP "a", end of input instead of the modifiers
    merge_fails("${v6}28316161", qr/Unexpected termination|Premature end/,
                "missing regexp modifiers");

    # a tracked SHORT_BINARY is still a SHORT_BINARY
    my $out = merge_packets({ top_level_element => SRL_TOP_LEVEL_SCALAR },
                            pack("H*", "${v6}28316161e169"));
    ok(defined $out, "tracked modifiers are accepted") or diag $@;
    like(decode_sereal($out), qr/^\(\?\^?i/, "and keep their flags");
};

subtest "end of input after OBJECT" => sub {
    # OBJECT tag as the last byte
    merge_fails("${v6}2c", qr/Unexpected termination|Premature end/, "OBJECT without a class name");
};

subtest "references into earlier documents" => sub {
    # REFN [ 0, [] (tracked at offset 4), REFP to 4 ], valid
    my $doc1 = pack("H*", "${v6}284300c02904");
    # [ "xy", REFP to 4 ]: offset 4 is inside "xy", but was tracked in doc1
    my $doc2 = pack("H*", "${v6}42627879" . "2904");
    # ARRAYREF_4 with three elements, which fails after tracking offset 3
    my $docF = pack("H*", "${v6}4400c02903");
    # [ "AAAAAAAA", REFP to 3 ]: offset 3 is inside the string
    my $doc3 = pack("H*", "${v6}42684141414141414141" . "2903");

    my $mrg = Sereal::Merger->new();
    $mrg->append($doc1);
    ok(!eval { $mrg->append($doc2); 1 }, "a REFP to an offset only tracked in an earlier document is rejected");
    like($@, qr/bad target offset/, "with a bad offset error");
    is_deeply(eval { decode_sereal($mrg->finish()) }, [ decode_sereal($doc1) ], "and the output holds the first document only");

    $mrg = Sereal::Merger->new();
    $mrg->append($doc1);
    ok(!eval { $mrg->append($docF); 1 }, "a truncated document is rejected");
    ok(!eval { $mrg->append($doc3); 1 }, "a REFP to an offset tracked by a failed document is rejected");
    like($@, qr/bad target offset/, "with a bad offset error");
    is_deeply(eval { decode_sereal($mrg->finish()) }, [ decode_sereal($doc1) ], "and the output holds the first document only");
};

subtest "COPY targets" => sub {
    # ARRAYREF_3 [ [] (tracked), REFP to it, { COPY of the tracked [] => 1 } ]:
    # with the track bit set the target tag looked like a SHORT_BINARY
    merge_fails("${v6}43c02902512f0201", qr/bad COPY tag/, "hash key COPY of a tracked non-string");
    # ARRAYREF_3 [ "abcd", COPY of "abcd", COPY of that COPY ]
    merge_fails("${v6}4364616263642f022f07", qr/bad COPY tag/, "COPY of a COPY");

    # a COPY of a string is fine
    my $out = merge_packets({}, pack("H*", "${v6}4264616263642f02"));
    is_deeply(eval { decode_sereal($out) }, [ [ "abcd", "abcd" ] ], "COPY of a string is merged");
};

done_testing;
