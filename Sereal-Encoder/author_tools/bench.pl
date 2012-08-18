use strict;
use warnings;
use blib;
use Sereal::Encoder qw(encode_sereal);
use Benchmark qw(cmpthese);
use JSON::XS qw(encode_json);
use Data::Dumper qw(Dumper);
use Data::Dumper::Limited qw(DumpLimited);
use Storable qw(nfreeze thaw);
use Data::MessagePack;

our $mpo = Data::MessagePack->new();

my @str;
push @str, join("", map chr(65+int(rand(57))), 1..10) for 1..1000;

our $data = {
  [1..10000],
  {@str},
  {@str},
  [1..10000],
  {@str},
  {@str},
  {@str},
  {@str},
};

my $json_xs  = encode_json($data);
my $dd       = Dumper($data);
my $ddl      = DumpLimited($data);
my $sereal   = encode_sereal($data);
my $storable = nfreeze($data);
my $mp       = $mpo->pack($data);

require bytes;
print "JSON::XS:              " . bytes::length($json_xs) . " bytes\n";
print "Data::Dumper::Limited: " . bytes::length($ddl) . " bytes\n";
print "Data::Dumper:          " . bytes::length($dd) . " bytes\n";
print "Sereal::Encoder:       " . bytes::length($sereal) . " bytes\n";
print "Storable:              " . bytes::length($storable) . " bytes\n";
print "Data::MessagePack:     " . bytes::length($mp) . " bytes\n";

our $x;
cmpthese(
  -3,
  {
    json_xs => '$::x = encode_json($::data) for 1..10;',
    ddl => '$::x = DumpLimited($::data) for 1..10;',
    dd => '$::x = Dumper($::data) for 1..100;',
    sereal => '$::x = encode_sereal($::data) for 1..10;',
    storable => '$::x = nfreeze($::data) for 1..10;',
    mp => '$::x = $::mpo->pack($::data) for 1..10;',
  }
);

