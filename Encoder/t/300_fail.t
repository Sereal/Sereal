#!perl
use strict;
use warnings;
use Sereal::Encoder;
use Test::More tests => 4;

my $ok; my $err; my $out;

# croak_on_bless test
my $e = Sereal::Encoder->new({
    croak_on_bless => 1,
});
$ok = eval{$out = $e->encode(bless({}, "Foo")); 1};
$err = $@ || 'Zombie error';

ok(!$ok, "Object throws exception");
ok($err =~ /object/i, 'Exception refers to object');

$ok =  eval {$out = $e->encode({}); 1};
ok($ok, "Non-blessed hash does not throw exception");


# test that code refs throw exception
TODO: {
    local $TODO = "Serializing CODE refs should probably fail instead of serializing to \\undef";
    $ok = eval {$out = $e->encode(sub {}); 1};
    ok(!$ok, "Code ref throws exception");
}

