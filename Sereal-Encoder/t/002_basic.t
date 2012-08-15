#!perl
use strict;
use warnings;
use Data::Dumper::Limited qw(DumpLimited);
use Data::Dumper;
use Scalar::Util qw(weaken);
use Test::More;
*qquote= \&Data::Dumper::qquote;
# silly thing, but tests that it doesn't crash and burn
is(DumpLimited(undef), "undef");

is(DumpLimited(1), "1", "integer");
is(DumpLimited(-1), "-1", "negative int");
is(DumpLimited(2), "2", "integer (2)");
is(DumpLimited(2e10), "2". ("0" x 10), "large integer");
is(DumpLimited(99999.9881), "99999.9881", "float");
is(DumpLimited(-2.1111), "-2.1111", "negative float");

{
    my $latin1= "Ba\xDF";
    my $uni= $latin1;
    utf8::upgrade($uni);
    is(DumpLimited("foo"), "'foo'", "string - simple");
    is(DumpLimited($latin1), '"Ba\\337"', "0xDF - string latin");
    is(DumpLimited($uni), '"Ba\\x{df}"', "0xDF - string uni");
    for my $want (
        '"\\0012\\0034"',
        '"\\1x\\3y"',
        '"\\b\\e\\r\\n\\t\\f\\a\\0\\"\\\\"',
        '"\\x{100}"',
        '"\\$foo"',
        '"\\@foo"'
    ) {
          $latin1= eval $want
            or die "$want\n$@";
          $uni= $latin1;
          utf8::upgrade($uni);

          is(DumpLimited($latin1), $want, "$want string (latin)");
          is(DumpLimited($uni), $want, "$want string (uni)");
          is(DumpLimited($latin1), qquote($latin1), "$want qquote (latin)");
          is(DumpLimited($uni), qquote($uni), "$want qquote (uni)");
    }
}

my $x = 2.1;
is(DumpLimited($x), "2.1", "variable");
is(DumpLimited(\2.1), "\\2.1", "scalar reference");
is(DumpLimited(\$x), "\\2.1", "scalar reference (2)");

is(DumpLimited([]), "[]", "empty array");
is(DumpLimited([1, 2.1]), "[1,2.1]", "array with numbers");
is(DumpLimited([[[],[]],[]]), "[[[],[]],[]]", "nested arrays");

is(DumpLimited({"" => [1,2,3]}), q!{'',[1,2,3]}!, "hash with empty string key");
is(DumpLimited({"aaa" => [1,2,3]}), q!{'aaa',[1,2,3]}!, "hash with simple key");

is(DumpLimited([\$x, \$x]), "[\\2.1,\\2.1]", "multiple identical refs");

my $r;
$r = [\$r];
ok(not(eval {DumpLimited($r); 1}) && $@, "cyclic refs barf");
undef $r->[0];

$r = [[[\$r]]];
ok(not(eval {DumpLimited($r); 1}) && $@, "deep cyclic refs barf");
undef $r->[0];

$x = [];
$r = [$x, $x];
is(DumpLimited($r), "[[],[]]", "multiple identical refs (2)");

weaken($r->[1]);
is(DumpLimited($r), "[[],[]]", "multiple identical refs (2)");

$r = [[\$r, \$r]];
weaken($r->[0][0]);
ok(not(eval {DumpLimited($r); 1}) && $@, "deep cyclic refs barf, even with weakrefs: $@");
undef $r->[0];

$r = [$x, $x];
ok(not(eval {DumpLimited($r, {disallow_multi => 1}); 1}) && $@, "non-cyclic, repeated refs barf under disallow_multi");

is(DumpLimited(bless({a => "b"} => "Foo"), {dump_objects => 1}), "bless({'a','b'},'Foo')", "Simple hashref object");


done_testing();
