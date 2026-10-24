#!perl
use strict;
use warnings;
use File::Spec;
use lib File::Spec->catdir(qw(t lib));

BEGIN { lib->import('lib') if !-d 't' }

use Test::More;
use Sereal::Decoder;

# A COPY whose target is an OBJECT tag with an inline class name re-reads
# the class name at the same offset. That registered the class a second
# time, replacing the list of objects already queued against it, so the
# objects decoded before the COPY were never blessed.

my $dec = Sereal::Decoder->new;

# [ OBJECT "Foo" REFN ARRAY(0), COPY(4) ] - offset 4 is the OBJECT tag
my $r = $dec->decode( pack 'H*', '3df3726c0300282b022c63466f6f282b002f04' );
is( ref $r->[0], 'Foo', 'the object before the COPY is blessed' );
is( ref $r->[1], 'Foo', 'the object decoded through the COPY is blessed' );
isnt( $r->[0], $r->[1], 'they are distinct objects' );

# [ OBJECT "Foo" REFN ARRAY(0), COPY(4), OBJECTV(5) REFN ARRAY(0) ]
$r = $dec->decode( pack 'H*', '3df3726c0300282b032c63466f6f282b002f042d05282b00' );
is_deeply( [ map ref, @$r ], [ ('Foo') x 3 ], 'OBJECTV after the COPY blesses every object' );

done_testing;
