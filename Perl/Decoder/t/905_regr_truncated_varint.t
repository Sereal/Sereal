#!perl
use strict;
use warnings;
use File::Spec;
use lib File::Spec->catdir(qw(t lib));

BEGIN {
    lib->import('lib')
        if !-d 't';
}

use Test::More tests => 14;
use Sereal::Decoder qw(decode_sereal);

# Header for protocol version 4, no flags
my $header_v4 = "=\xF3rl" . chr(4) . chr(0);

# Test 1-5: Truncated packets right after a tag expecting a varint,
# where the tag byte itself has bit 7 clear (< 0x80).
# Without the bounds check in srl_read_varint_uv, the decoder checked
# !(*(buf->end - 1) & 0x80) without ensuring buf->pos < buf->end,
# invoking srl_read_varint_u64_nocheck at buf->end and reading out of bounds.

my %tag_tests = (
    'SRL_HDR_VARINT'   => chr(0x20),
    'SRL_HDR_ARRAY'    => chr(0x2B),
    'SRL_HDR_HASH'     => chr(0x2A),
    'SRL_HDR_BINARY'   => chr(0x26),
    'SRL_HDR_STR_UTF8' => chr(0x27),
);

for my $name ( sort keys %tag_tests ) {
    my $tag    = $tag_tests{$name};
    my $packet = $header_v4 . $tag;
    eval { decode_sereal($packet); 1 };
    my $err = $@;
    ok( $err, "Truncated packet ending at tag $name fails cleanly" );
    like(
        $err, qr/end of packet|corrupt|near \d+/i,
        "Error message for $name indicates packet truncated or corrupt"
    );
}

# Test 11-12: Truncated multi-byte varint where continuation bit is set
# on byte 0 and packet ends immediately.
{
    my $packet = $header_v4 . chr(0x20) . chr(0x85);
    eval { decode_sereal($packet); 1 };
    my $err = $@;
    ok( $err, "Truncated multi-byte varint fails cleanly" );
    like(
        $err, qr/end of packet/i,
        "Error indicates end of packet reached before varint parsed"
    );
}

# Test 13-14: Tiny valid packet (< 11 bytes total) decodes correctly via safe reader.
# Total size: 6 bytes header + 1 byte tag (0x20) + 1 byte varint (0x2A = 42) = 8 bytes.
{
    my $packet = $header_v4 . chr(0x20) . chr(42);
    my $val;
    eval { $val = decode_sereal($packet); 1 };
    is( $@,   '', "Tiny packet (< 11 bytes) decodes without error" );
    is( $val, 42, "Tiny packet correctly decoded varint value 42" );
}
