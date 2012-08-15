#!perl
use strict;
use warnings;
use Data::Dumper::Limited qw(DumpLimited);
use Scalar::Util qw(weaken);
use Data::Undump qw(undump);

use Test::More;

my $aryref = [{foo => 'bar'}, 2, "asd"];
my $latin1= "Ba\xDF";
my $uni= $latin1;
utf8::upgrade($uni);
my @tests = (
  [undef, "undef"],
  ["", "empty string"],
  [0, "zero"],
  [1, "one"],
  [-1e10, "large int"],
  [1.1, "float"], # FIXME is this ok (float comparison)?
  [-1.1e12, "large float"], # FIXME is this ok (float comparison)?

  ["fghjk", "string"],
  ["fghüjk", "non-ascii string"],
  do {use utf8; ["搜索酒店", "utf8 string"] },
  [$latin1, "latin1 string"],
  [$uni, "unicode string"],
  ["a\nb", "string with newline"],

  [\undef, "constant scalar undef ref"],
  [\"foo", "constant scalar string ref"],

  [[], "empty array ref"],
  [{}, "empty hash ref"],

  [[1, 2, 3], "array ref"],
  [{a => 'b'}, "hash ref"],
  [{3 => 'b', sdfghjk => 345}, "hash ref2"],
  [{"fghüjk" => 345}, "non-ascii hash key"],
  do {use utf8; [{"搜索酒店" => "asd"}, "utf8 hash key"] },
  do {use utf8; [{"搜索酒店" => "搜索酒店"}, "utf8 hash all around"] },

  [[[1], [2, [3]]], "nested array ref"],
  [[[1], { foo => [2, [3, {bar => 'baz'}]]}], "nested array and hash refs"],
  [[[1], { foo => [2, [3, {bar => \'baz'}, \[]]]}], "nested array/hash/scalar refs"],

  [[$aryref, $aryref], "repeated substructure"],
  do {my $x = [$aryref, $aryref]; weaken($x->[0]); [$x, "repeated substructure, weaken"] },

  [qr/foo/, "regexp", "this is treated as an object. Not implemented in DDL"],
  [[qr/foo/], "aryref to regexp", "this is treated as an object. Not implemented in DDL"],

  [bless({a => 'b'}, "Foo"), "Simple hash-based object", "Not implemented in Data::Undump", {dump_objects => 1}],
  [[bless({a => bless([1,2,3] => 'fghüjk')}, "Foo")], "Nested objects", "Not implemented in Data::Undump", {dump_objects => 1}],
);

foreach my $test (@tests) {
  does_roundtrip(@$test);
}

sub does_roundtrip {
  my ($src_structure, $name, $todo, $opt) = @_;

  local $TODO;
  $TODO = $todo if defined $todo and $todo ne '';

  my $serialized;
  eval {$serialized = DumpLimited($src_structure, $opt ? ($opt) : ()); 1}
  or do {
    note("DumpLimited() failed with exception '$@'");
    fail("DumpLimited(): $name");
    return;
  };
  pass("DumpLimited(): $name");
  print "#" . $serialized, "\n";
  use Data::Dumper;
  print Data::Dumper::Dumper($src_structure);

  my $out = undump($serialized);
  my $err = $@;
  if ($err or defined($src_structure) && !defined($out)) {
    note("undump() threw an exception: $err");
    note("The data trying to undump was: '$serialized'");
    fail("undump: $name");
  }
  else {
    is_deeply($out, $src_structure, "undump: $name")
        or diag("serialized data was: $serialized");
  }

  $out = undef;
  eval "\$out = $serialized; 1"
  or do {
    my $err = $@ || 'Zombie error';
    note("eval \"\" threw an exception: $err");
    note("The code executed was: '\$out = $serialized; 1'");
    fail("eval: $name");
    return;
  };
  is_deeply($out, $src_structure, "eval \"\": $name");
}

done_testing();
