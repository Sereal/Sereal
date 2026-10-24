#!perl
use strict;
use warnings;
use File::Spec;
use lib File::Spec->catdir(qw(t lib));

BEGIN { lib->import('lib') if !-d 't' }

use Test::More;
use Sereal::TestSet qw(Header);
use Sereal::Decoder;
use Scalar::Util ();

# With alias_smallint or alias_varint_under, small integers decode to
# read-only scalars shared by every decode on that decoder. A REFP to a
# tracked small integer is a reference to the shared scalar, and an OBJECT
# wrapping it queued the shared scalar for blessing. With set_readonly the
# decoder lifted the read-only flag to bless it, so the shared integer
# stayed blessed for all later decodes. Blessing a shared value is now
# refused.

my %packets = (
    'OBJECT'  => pack( 'H*', '3df3726c0300282b02812c63466f6f2904' ),        # [ 1|TRACK, OBJECT "Foo" REFP(4) ]
    'OBJECTV' => pack( 'H*', '3df3726c0300282b03812c63466f6f282b002d062904' ), # ... OBJECT "Foo" [], OBJECTV(6) REFP(4) ]
);
my $one    = Header(3) . "\x28\x2b\x01\x01";    # [1]

for my $tag ( sort keys %packets ) {
    my $packet = $packets{$tag};
    for my $alias ( [ alias_smallint => 1 ], [ alias_varint_under => 20 ] ) {
        for my $ro ( [], [ set_readonly => 1 ], [ set_readonly_scalars => 1 ] ) {
            my $name = join ' ', $tag, "@$alias", @$ro ? "@$ro" : 'no readonly option';
            my $dec  = Sereal::Decoder->new( {@$alias, @$ro} );
            my $ok   = eval { $dec->decode($packet); 1 };
            ok( !$ok, "$name: blessing a shared integer is refused" );
            like( $@, qr/shared alias value/, "$name: with a clear error" );
            my $r = $dec->decode($one);
            is( ref \$r->[0], 'SCALAR', "$name: a later decode yields an unblessed integer" );
            is( $r->[0], 1, "$name: with the right value" );
            ok( !Scalar::Util::blessed( \$r->[0] ), "$name: and it is not an object" );
        }
    }

    # Without aliasing the same document decodes to an object.
    my $r = Sereal::Decoder->new->decode($packet);
    is( ref $r->[-1], 'Foo', "$tag without aliasing the referenced integer is blessed" );
    is( ${ $r->[-1] }, 1, "$tag and holds its value" );
}

done_testing;
