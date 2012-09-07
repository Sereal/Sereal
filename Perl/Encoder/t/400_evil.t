#!perl
use strict;
use warnings;
use Sereal::Encoder qw(:all);
use Data::Dumper;
use File::Spec;

# These tests use an installed Decoder to do testing on horrific
# Perl data structures such as overloaded and tied structures.

use lib File::Spec->catdir(qw(t lib));
BEGIN {
  lib->import('lib')
    if !-d 't';
}

use Sereal::TestSet qw(:all);
use Test::More;

if (not have_encoder_and_decoder()) {
  plan skip_all => 'Did not find right version of decoder';
}
else {
  require Sereal::Decoder;
  Sereal::Decoder->import(":all");
}

SCOPE: {
  package TiedHash;
  require Tie::Hash;
  our @ISA = qw(Tie::StdHash);
}

my %testhash = (
  foo => [qw(a b c)],
  baz => 123,
  dfvgbnhmjk => "345ty6ujh",
  a => undef,
);

my %tied_hash;
tie %tied_hash => 'TiedHash';
%{tied(%tied_hash)} = %testhash;
is_deeply(\%tied_hash, \%testhash);

my ($out, $ok, $err, $data);

$ok = eval {$out = encode_sereal(\%tied_hash); 1};
$err = $@ || 'Zombie error';
ok($ok, "serializing tied hash did not die")
  or note("Error was '$err'");
ok(defined $out, "serializing tied hash returns string");

hobodecode($out) if $ENV{DEBUG_SEREAL};

$ok = eval {$data = decode_sereal($out); 1;};
$err = $@ || 'Zombie error';
ok($ok, "deserializing tied hash did not die")
  or note("Error was '$err', data was:\n"), hobodecode($out);
ok(defined $data, "deserializing tied hash yields defined output");
is_deeply($data, \%testhash, "deserializing tied hash yields expected output");

pass("Alive at end");
done_testing();

