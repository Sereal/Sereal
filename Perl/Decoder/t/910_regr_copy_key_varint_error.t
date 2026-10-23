#!perl
use strict;
use warnings;
use Test::More tests => 1;
use Sereal::Decoder qw(decode_sereal);

# The bytes inside the first string are crafted so a later COPY key points
# at a BINARY tag whose length varint starts with a continuation byte. The
# helper's bounded varint reader must report the malformed length without
# doing pointer arithmetic relative to a NULL buffer start.
my $header = "=\xF3rl" . chr(4) . chr(0);
my $body   = chr(0x28) . chr(0x2B) . chr(2)         # REFN ARRAY(2)
    . chr(0x26) . chr(2) . chr(0x26) . chr(0x80)    # BINARY payload (body offset 6)
    . chr(0x28) . chr(0x2A) . chr(1)                # REFN HASH(1)
    . chr(0x2F) . chr(6)                            # COPY(6): key copied from the BINARY tag
    . chr(1);                                       # value

eval { decode_sereal( $header . $body ); 1 };
like( $@, qr/while reading binary key via COPY/, 'malformed COPY key varint is rejected cleanly' );
