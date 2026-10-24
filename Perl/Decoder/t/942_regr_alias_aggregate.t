#!perl
use strict;
use warnings;
use File::Spec;
use lib File::Spec->catdir(qw(t lib));

BEGIN { lib->import('lib') if !-d 't' }

use Test::More;
use Sereal::Decoder;
use Sereal::Encoder;

# The TRACK flag on an ARRAY or HASH tag behind a REFN tracks the referent,
# an AV or HV. ALIAS stored the tracked value directly into its container,
# so an array element or hash value became a bare AV/HV body: reading it
# croaked with "Bizarre copy" and assigning to it crashed perl. Encoders
# only emit ALIAS for scalars, so aliases to aggregates are now rejected.

my $dec = Sereal::Decoder->new;

for my $case (
    [ '3df3726c0300282b0228ab002e05',         'array element aliasing a tracked ARRAY' ],
    [ '3df3726c030028ab012e02',               'element of a tracked ARRAY aliasing that ARRAY' ],
    [ '3df3726c0300282a02616128aa0061622e07', 'hash value aliasing a tracked HASH' ],
    )
{
    my ( $hex, $name ) = @$case;
    my $ok = eval { $dec->decode( pack 'H*', $hex ); 1 };
    ok( !$ok, "$name is rejected" );
    like( $@, qr/ALIAS.*(?:ARRAY|HASH)/, "$name names the aliased aggregate" );
}

# Aliased scalars, as encoded with aliased_dedupe_strings, keep working.
for my $version ( 3, 6 ) {
    my $blob = Sereal::Encoder->new( { protocol_version => $version, aliased_dedupe_strings => 1 } )
        ->encode( [ ('abcdef') x 4 ] );
    like( unpack( 'H*', $blob ), qr/2e/, "v$version the document uses ALIAS" );
    my $r = $dec->decode($blob);
    is_deeply( $r, [ ('abcdef') x 4 ], "v$version aliased strings decode" );
    $r->[1] = 5;
    $_ .= 'z' for @$r;
    is(
        "@$r", join( ' ', ('5zzzz') x 4 ),
        "v$version aliased elements are assignable and stay aliased"
    );
}

done_testing;
