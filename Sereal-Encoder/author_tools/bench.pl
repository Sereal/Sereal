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
use Getopt::Long qw(GetOptions);

GetOptions(
  'dump|d' => \(my $dump),
);

our %opt = @ARGV;

use constant SEREAL_ONLY => 0;

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

my ($json_xs, $dd1, $dd2, $ddl, $sereal, $storable, $mp);
if (!SEREAL_ONLY) {
  $json_xs  = encode_json($data);
  $dd1      = Data::Dumper->new([$data])->Indent(0)->Dump();
  $dd2      = Dumper($data);
  $ddl      = DumpLimited($data);
  $storable = nfreeze($data);
  $mp       = $mpo->pack($data);
}
$sereal   = encode_sereal($data, \%opt);

warn $sereal if $dump;

require bytes;
if (!SEREAL_ONLY) {
  print "JSON::XS:              " . bytes::length($json_xs) . " bytes\n";
  print "Data::Dumper::Limited: " . bytes::length($ddl) . " bytes\n";
  print "Data::Dumper (1):      " . bytes::length($dd1) . " bytes\n";
  print "Data::Dumper (2):      " . bytes::length($dd2) . " bytes\n";
  print "Storable:              " . bytes::length($storable) . " bytes\n";
  print "Data::MessagePack:     " . bytes::length($mp) . " bytes\n";
}
print "Sereal::Encoder:       " . bytes::length($sereal) . " bytes\n";

our $x;
cmpthese(
  -3,
  {
    (!SEREAL_ONLY
      ? (
        json_xs => '$::x = encode_json($::data);',
        ddl => '$::x = DumpLimited($::data);',
        dd1 => '$::x = Data::Dumper->new([$::data])->Indent(0)->Dump();',
        dd2 => '$::x = Dumper($::data);',
        storable => '$::x = nfreeze($::data);',
        mp => '$::x = $::mpo->pack($::data);',
      ) : ()),
    sereal => '$::x = encode_sereal($::data, \%::opt);',
  }
);

