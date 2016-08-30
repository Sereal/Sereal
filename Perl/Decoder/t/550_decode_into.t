use strict;
use warnings;
use Test::More tests => 8;
use File::Spec;
use lib File::Spec->catdir(qw(t lib));
BEGIN {
    lib->import('lib')
        if !-d 't';
}
use Sereal::TestSet qw(have_encoder_and_decoder);

use Sereal::Decoder;
my $decoder= Sereal::Decoder->new();
my $enc_ref= "=\363rl\3\0Qcfoo\1";
my $enc_str= "=\363rl\3\0}blah blah blah blah blah blah";

# Repeatedly decode "into" the $into variable.
# Overwrting/reusing the data it contains.
# We alternate between ref and scalar to see if we can trigger a segfault.

my $into;
$decoder->decode($enc_ref, $into);
ok(ref $into, "first decode was a reference");
$decoder->decode($enc_str, $into);
ok(!ref $into, "second decode was a string");

$decoder->decode($enc_ref, $into);
ok(ref $into, "third decode was a reference");
$decoder->decode($enc_str, $into);
ok(!ref $into, "fourth decode was a string (and did not segfault)");

$decoder->decode($enc_ref, $into);
ok(ref $into, "fifth decode was a reference - and did not segault");
$decoder->decode($enc_str, $into);
ok(!ref $into, "sixth decode was a string  - and did not segfault, probably ok");

$decoder->decode($enc_ref, $into);
ok(ref $into, "seventh decode was a reference - maybe overkill");
$decoder->decode($enc_str, $into);
ok(!ref $into, "eight decode was a string - maybe overkill");

