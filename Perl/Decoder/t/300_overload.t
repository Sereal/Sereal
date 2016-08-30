#!perl
use strict;
use warnings;
use Data::Dumper;
use File::Spec;

# These tests use an installed Encoder.
# Purpose: See if we correctly set overload magic on deserialized
#          objects in funny circumstances.

use lib File::Spec->catdir(qw(t lib));
BEGIN {
    lib->import('lib')
        if !-d 't';
}

use Sereal::TestSet qw(:all);
use Test::More;
use Sereal::Decoder;

my $ok = have_encoder_and_decoder();
if (not $ok) {
    plan skip_all => 'Did not find right version of encoder';
    done_testing();
    exit(0);
}

require Sereal::Encoder;

my $encoder = Sereal::Encoder->new({
    stringify_unknown => 1,
    warn_unknown => 1,
});

# encode before any overload is known
my $s = $encoder->encode(bless({foo => "123"} => "Str"));

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
is("$obj", 123, "Deserialized object serializes fine");
$Str::Called = $Str::Called; # silence warning
is($Str::Called, 1, "overload invoked once");


# Second try at breaking things
SCOPE: {
    my $enc = Sereal::Encoder->new({
        warn_unknown => 1,
        stringify_unknown => 1,
    });

    my $dec = Sereal::Decoder->new;

    package Foo;
    use overload '""' => sub {return $_[0]->{str}};

    package main;

    my $p = bless({str => "asd"} => 'Foo');
    my $h = [ $p, $p ];
    my $s = $enc->encode($h);
    my $d = $dec->decode($s);

    #warn "$_" for @$d;
    my $x = join ",", @$d;
    is($x, "asd,asd", "overload stringification works for second object occurrence");

    #warn $x;
}

pass("Alive");
done_testing();

