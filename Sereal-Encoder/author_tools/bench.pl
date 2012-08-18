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

our $data = [
  [1..10000],
  {@str},
  {@str},
  [1..10000],
  {@str},
  [map rand,1..1000],
  {@str},
  {@str},
];

my $json_xs  = encode_json($data);
my $dd1      = Data::Dumper->new([$data])->Indent(0)->Dump();
my $dd2      = Dumper($data);
my $ddl      = DumpLimited($data);
my $sereal   = encode_sereal($data);
my $storable = nfreeze($data);
my $mp       = $mpo->pack($data);


if(@ARGV){
    print $sereal;
    exit;
}
require bytes;
print "JSON::XS:              " . bytes::length($json_xs) . " bytes\n";
print "Data::Dumper::Limited: " . bytes::length($ddl) . " bytes\n";
print "Data::Dumper (1):      " . bytes::length($dd1) . " bytes\n";
print "Data::Dumper (2):      " . bytes::length($dd2) . " bytes\n";
print "Sereal::Encoder:       " . bytes::length($sereal) . " bytes\n";
print "Storable:              " . bytes::length($storable) . " bytes\n";
print "Data::MessagePack:     " . bytes::length($mp) . " bytes\n";

our $x;
cmpthese(
  -3,
  {
    json_xs => '$::x = encode_json($::data);',
    ddl => '$::x = DumpLimited($::data);',
    dd1 => '$::x = Data::Dumper->new([$::data])->Indent(0)->Dump();',
    dd2 => '$::x = Dumper($::data);',
    sereal => '$::x = encode_sereal($::data);',
    storable => '$::x = nfreeze($::data);',
    mp => '$::x = $::mpo->pack($::data);',
  }
);

