use strict;
use Test::More tests => 3;

use Sereal::Encoder qw(sereal_encode_with_object);

local $SIG{__WARN__} = sub { };
{
    my $obj = bless \( my $iv = -1 ), 'Sereal::Encoder';
    is( $obj->flags, undef, "a blessed integer is not a handle" );
    eval { sereal_encode_with_object( $obj, "x" ) };
    like( $@, qr/not a Sereal::Encoder handle/, "sereal_encode_with_object refuses it" );
}
pass("freeing it does not crash");
