#!perl
use strict;
use warnings;
use File::Spec;
use lib File::Spec->catdir(qw(t lib));

BEGIN { lib->import('lib') if !-d 't' }

use Test::More;
use Sereal::TestSet qw(Header);
use Sereal::Decoder;

# decode_with_offset() with an offset past the end of the input must fail
# cleanly. The error used to be reported through the input buffer before
# it had been set up for the call, doing pointer arithmetic on a NULL
# buffer (fresh decoder) or on the previous call's buffer (reused decoder).
# It now names the offset and the input length explicitly.

for my $version ( 1 .. 6 ) {
    my $packet = Header($version) . chr(1);
    my $len    = length $packet;
    my $dec    = Sereal::Decoder->new;
    for my $round ( 'fresh', 'reused' ) {
        my $ok = eval { $dec->decode_with_offset( $packet, 100 ); 1 };
        ok( !$ok, "v$version $round decoder: start offset beyond the input is rejected" );
        like(
            $@,
            qr/^Sereal: Error: Start offset 100 is beyond input string length $len at offset 1 of input/,
            "v$version $round decoder: the error names the offset and the input length"
        );
        is( $dec->decode($packet), 1, "v$version $round decoder: still decodes afterwards" );
    }
    is( $dec->decode_with_offset( "x" x 3 . $packet, 3 ), 1, "v$version an in-range offset still decodes" );
}

done_testing;
