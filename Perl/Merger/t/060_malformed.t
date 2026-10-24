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

done_testing;
