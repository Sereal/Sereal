#!perl
use strict;
use warnings;
use File::Spec;
use lib File::Spec->catdir(qw(t lib));

BEGIN { lib->import('lib') if !-d 't' }

use Test::More;
use Sereal::Decoder;
use Sereal::TestSet qw(Header varint);

# Final dynamic DEFLATE blocks with a one-symbol literal/length alphabet.
# The code-length alphabet has symbols 0 and 1, each with a one-bit code.
# In the first stream only end-of-block (256) has a code; the data starts
# with the unused bit 1. In the second only literal 'A' has a code; a valid
# literal (bit 0) precedes the unused bit 1. Padding keeps both cases in
# tinfl's two-symbol fast path, exercising its first and second lookups.
# Before miniz 3.1.2 these zero-length lookups filled the destination with
# bogus bytes and returned a buffer error (-5), instead of a data error
# (-3). Either error is acceptable here. Older external miniz libraries
# must also reject the packet without leaving the decoder unusable.
my @streams = ( [
        first =>
            '780105c001040000000010000000000000000000000000000000000000000000000000000000000000008002'
    ],
    [
        second =>
            '780105c001040000000010000000000000000001000000000000000000000000000000000000000000000004'
    ],
);

for my $case (@streams) {
    my ( $name, $hex ) = @$case;
    my $stream = pack( 'H*', $hex ) . ( "\0" x 16 );
    my $header = Header(6);
    substr( $header, 4, 1 ) = chr(0x36);    # v6, zlib encoding
    my $packet  = $header . varint(1024) . varint( length $stream ) . $stream;
    my $decoder = Sereal::Decoder->new;
    my $ok      = eval { $decoder->decode($packet); 1 };
    ok( !$ok, "$name lookup: invalid Huffman code is rejected" );
    like(
        $@, qr/ZLIB decompression .* failed with error -\d+!/,
        "$name lookup: reports a decompression error"
    );
    is( $decoder->decode( Header(6) . "\x07" ), 7, "$name lookup: decoder remains usable" );
}

done_testing();
