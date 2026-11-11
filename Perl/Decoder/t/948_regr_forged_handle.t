use strict;
use Test::More tests => 3;

use Sereal::Decoder qw(sereal_decode_with_object);

# A document holding bless(\-1, 'Sereal::Decoder').
my $doc = "=\xF3rl\x03\x00" . "\x2c\x6fSereal::Decoder" . "\x28\x1f";

local $SIG{__WARN__} = sub { };
{
    my $obj = Sereal::Decoder->new->decode($doc);
    is( $obj->bytes_consumed, undef, "a decoded Sereal::Decoder object is not a handle" );
    eval { sereal_decode_with_object( $obj, $doc ) };
    like( $@, qr/not a Sereal::Decoder handle/, "sereal_decode_with_object refuses it" );
}
pass("freeing it does not crash");
