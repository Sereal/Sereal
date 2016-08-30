#!perl
use strict;
use warnings;
use File::Spec;
use Scalar::Util qw( blessed );
use lib File::Spec->catdir(qw(t lib));
BEGIN {
    lib->import('lib')
        if !-d 't';
}

use Sereal::TestSet qw(:all);
use Sereal::Encoder qw(:all);
use Sereal::Encoder::Constants qw(:all);
use Test::More;

my $ref = Header(SRL_PROTOCOL_VERSION, chr(0b0000_1100)) . chr(0b0001_0000); # -16 in body, 12 in header
is(encode_sereal_with_header_data(-16, 12), $ref, "Encode 12 in header, -16 in body");
is(Sereal::Encoder->new->encode(-16, 12), $ref, "OO: Encode 12 in header, -16 in body");

my $ok = have_encoder_and_decoder();
if (not $ok) {
    SKIP: { skip 'Did not find right version of decoder' => 1 }
}
else {
    my $dec = Sereal::Decoder->new;
    my $encoded = encode_sereal_with_header_data(-16, 12);
    my $decoded = $dec->decode($encoded);
    is($decoded, -16, "-16 decoded correctly");
    $decoded = $dec->decode_only_header($encoded);
    is($decoded, 12, "12 decoded correctly");

    my $munged = "X" . $encoded;
    $decoded = $dec->decode_with_offset($munged, 1);
    is($decoded, -16, "-16 decoded correctly (offset)");
    $decoded = $dec->decode_only_header_with_offset($munged, 1);
    is($decoded, 12, "12 decoded correctly (offset)");
}

pass("Alive at end");
done_testing();

