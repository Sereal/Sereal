#!perl
use strict;
use warnings;
use File::Spec;
use lib File::Spec->catdir(qw(t lib));

BEGIN {
    lib->import('lib')
        if !-d 't';
}

use Test::More tests => 2;
use Sereal::Decoder qw(decode_sereal);

my $base = "Corrupted COPY tag, offset %d to %d must precede the tags own offset";
$base =~ s/%d/\\d+/g;

# Regression coverage for the CWE-125 heap OOB read via a COPY tag whose
# offset points one byte inside a previously-decoded BINARY's content,
# where the planted byte matches the SHORT_BINARY tag pattern
# (0x60..0x7F). srl_read_object (class name) and srl_read_hash (key)
# both had unchecked SHORT_BINARY-inline COPY branches; the fix bounds
# `from + key_len` against `dec->buf.end` and croaks on overrun.

sub build_packet {
    my ($container_bytes) = @_;
    my $header = "=\xF3rl" . chr(0x05) . chr(0x00);

    # 30-byte BINARY whose final byte is 0x7F (SHORT_BINARY-len-31
    # forgery). The COPY offset 34 lands on this byte: from there the
    # decoder reads 31 bytes for the inline SHORT_BINARY length,
    # overrunning buf.end by 26 bytes without the fix.
    my $binary_content = ( chr(0x42) x 29 ) . chr(0x7F);
    my $body           = chr(0x2B) . chr(0x02)             # ARRAY count=2
        . chr(0x26) . chr(0x1E)                            # BINARY length=30
        . $binary_content . $container_bytes;
    return $header . $body;
}

# OBJECT variant: OBJECT + COPY(34) + REFN + UNDEF. The COPY-target
# byte is decoded as the OBJECT's class name.
{
    my $packet = build_packet( chr(0x2C) . chr(0x2F) . chr(34) . chr(0x28) . chr(0x25) );
    eval { decode_sereal($packet) };
    like(
        $@,
        qr/$base while reading classname/,
        "srl_read_object SHORT_BINARY-COPY OOB rejected with bounds error"
    );
}

# HASH variant: HASH(1) + COPY(34) + UNDEF. The COPY-target byte is
# decoded as the hash key.
{
    my $packet = build_packet( chr(0x2A) . chr(0x01) . chr(0x2F) . chr(34) . chr(0x25) );
    eval { decode_sereal($packet) };
    like(
        $@,
        qr/$base while reading key/,
        "srl_read_hash SHORT_BINARY-COPY OOB rejected with bounds error"
    );
}
