#!perl
use strict;
use warnings;
use Sereal::Encoder;
use Test::More tests => 2;

my $e = Sereal::Encoder->new({
    croak_on_bless => 1,
});
my $ok = eval{$e->encode(bless({}, "Foo")); 1};
my $err = $@ || 'Zombie error';

ok(!$ok);
ok($err =~ /object/);

