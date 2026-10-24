#!perl
use strict;
use warnings;
use File::Spec;
use lib File::Spec->catdir(qw(t lib));

BEGIN { lib->import('lib') if !-d 't' }

use Test::More;
use Sereal::TestSet qw(Header varint);
use Sereal::Decoder;
use Scalar::Util ();

# Every OBJECT class name used to create a package (stash) as soon as it
# was read, even with no_bless_objects, so a document with many distinct
# class names grew the symbol table permanently. Stashes are now created
# only when an object is actually blessed into them.

sub str {
    length $_[0] < 32
        ? chr( 0x60 + length $_[0] ) . $_[0]
        : "\x26" . varint( length $_[0] ) . $_[0];
}
sub object { "\x2c" . str( $_[0] ) . "\x28\x2b\x00" }    # OBJECT name REFN ARRAY(0)

# Checks whether a package exists without creating it (or its parents).
sub package_exists {
    my $table = \%main::;
    for my $part ( split /::/, shift ) {
        return 0 unless exists $table->{"${part}::"};
        $table = \%{ $table->{"${part}::"} };
    }
    return 1;
}

sub packets {
    my $name = shift;
    return (
        'OBJECT'  => Header(3) . object($name),
        'OBJECTV' => Header(3) . "\x42" . object($name) . "\x2d" . varint(3) . "\x28\x2b\x00",
        'OBJECT of COPY' => Header(3) . "\x42" . object($name) . "\x2c\x2f\x03\x28\x2b\x00",
    );
}

my $nobless = Sereal::Decoder->new( { no_bless_objects => 1 } );
my $bless   = Sereal::Decoder->new;
my $n       = 0;
for my $base ( 'SerealTestNoStash', 'Sereal::Test::Nonexistent::Foo' ) {
    my %p = packets($base);
    for my $kind ( sort keys %p ) {
        ok( !package_exists($base), "$base does not exist yet" );
        my $r       = $nobless->decode( $p{$kind} );
        my @objects = $kind eq 'OBJECT' ? ($r) : @$r;
        is_deeply(
            [ map ref, @objects ], [ ('ARRAY') x @objects ],
            "$kind with no_bless_objects: nothing is blessed"
        );
        ok( !package_exists($base), "$kind with no_bless_objects: $base was not created" );
    }
    for my $kind ( sort keys %p ) {
        my $name = $base . 'Blessed' . ++$n;
        my %q    = packets($name);
        ok( !package_exists($name), "$name does not exist yet" );
        my $r       = $bless->decode( $q{$kind} );
        my @objects = $kind eq 'OBJECT' ? ($r) : @$r;
        is_deeply(
            [ map ref, @objects ], [ ($name) x @objects ],
            "$kind: every object is blessed into $name"
        );
        ok( package_exists($name), "$kind: blessing created $name" );
    }
}

# A class first seen on a plain OBJECT and then referenced by OBJECTV_FREEZE
# still reaches the THAW handling with its name.
{
    my $name   = 'SerealTestNoStashFrozen';
    my $packet = Header(3) . "\x42" . object($name) . "\x33" . varint(3) . "\x28\x2b\x00";
    my $r =
        Sereal::Decoder->new( { no_bless_objects => 1, no_thaw_objects => 1 } )->decode($packet);
    is( ref $r->[1], 'Sereal::Decoder::THAW_args', 'the frozen object is left unthawed' );
    is( $r->[1][-1], $name,                        'with its class name recorded' );
}

done_testing;
