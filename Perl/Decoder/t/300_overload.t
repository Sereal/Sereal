#!perl
use strict;
use warnings;
use Sereal::Decoder;
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
is($Str::Called, 1, "overload invoked once");

pass();
done_testing();

