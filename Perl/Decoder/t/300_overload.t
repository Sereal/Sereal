#!perl
use strict;
use warnings;
use Data::Dumper;
use File::Spec;

# Purpose: See if we correctly set overload magic on deserialized
#          objects in funny circumstances.

use lib File::Spec->catdir(qw(t lib));

BEGIN {
    lib->import('lib')
        if !-d 't';
}

use Sereal::TestSet qw(:all);
use Sereal::TestSet::Corpus qw(read_packet);
use Test::More;
use Sereal::Decoder;

my $corpus = File::Spec->catdir(qw(t data packet_corpus));
my $s = read_packet( $corpus, 'decoder-features', 'overload/unknown_object' );

my $decoder = Sereal::Decoder->new();

# "load" the object's class
eval <<'HERE';
package Str;
use vars '$Called';
$Called = 0;
use overload '""' => sub {
    $Called++;
    return $_[0]->{foo};
};
HERE

# decode object
# Yves: Move this to where the class wasn't loaded yet, and the tests fail
#       (which indicates overload hooking into bless and adding magic to the
#        object {{citation required}})
my $obj = $decoder->decode($s);

# FOR YVES:
#use Devel::Peek;
#Dump($obj);

# see if overload magic is on object
is( "$obj", 123, "Deserialized object serializes fine" );
$Str::Called = $Str::Called;    # silence warning
is( $Str::Called, 1, "overload invoked once" );

# Second try at breaking things
SCOPE: {
    my $dec = Sereal::Decoder->new;

    package Foo;
    use overload '""' => sub { return $_[0]->{str} };

    package main;

    my $s = read_packet( $corpus, 'decoder-features', 'overload/repeated_object' );
    my $d = $dec->decode($s);

    #warn "$_" for @$d;
    my $x = join ",", @$d;
    is( $x, "asd,asd", "overload stringification works for second object occurrence" );

    #warn $x;
}

pass("Alive");
done_testing();

