#!perl
use strict;
use warnings;

use Sereal::Path;
use Test::More;
use Data::Dumper;

#invalid requests
my $val = Sereal::Path::_tests::is_range("");
ok(@$val == 0, "empty string");

$val = Sereal::Path::_tests::is_range("foo");
ok(@$val == 0, "invalid string");

$val = Sereal::Path::_tests::is_range("123");
ok(@$val == 0, "single int");

$val = Sereal::Path::_tests::is_range("123.0");
ok(@$val == 0, "single float");

$val = Sereal::Path::_tests::is_range(":");
ok(@$val == 0, "only colon");

$val = Sereal::Path::_tests::is_range("::");
ok(@$val == 0, "two colons");

$val = Sereal::Path::_tests::is_range(":::");
ok(@$val == 0, "tree colons");

$val = Sereal::Path::_tests::is_range("1:a");
ok(@$val == 0, "req: '1:a'");

$val = Sereal::Path::_tests::is_range("1:1:a");
ok(@$val == 0, "req: '1:1:a'");

$val = Sereal::Path::_tests::is_range("1:2:3:");
ok(@$val == 0, "req: '1:1:a'");

# valid requests

$val = Sereal::Path::_tests::is_range("1:");
is_deeply($val, [1, 0x7FFFFFFF, 1], "req: '1:'");

$val = Sereal::Path::_tests::is_range(":1");
is_deeply($val, [0, 1, 1], "req: ':1'");

$val = Sereal::Path::_tests::is_range("1:2");
is_deeply($val, [1, 2, 1], "req: '1:2'");

$val = Sereal::Path::_tests::is_range("1:2:");
is_deeply($val, [1, 2, 1], "req: '1:2:'");

$val = Sereal::Path::_tests::is_range("1:2:3");
is_deeply($val, [1, 2, 3], "req: '1:2:3'");

$val = Sereal::Path::_tests::is_range("-1:");
is_deeply($val, [-1, 0x7FFFFFFF, 1], "req: '-1:'");

$val = Sereal::Path::_tests::is_range("-1:-2");
is_deeply($val, [-1, -2, 1], "req: '-1:-2'");

$val = Sereal::Path::_tests::is_range("-1:-1:-3");
is_deeply($val, [-1, -1, -3], "req: '-1:-1:-3'");

done_testing;
