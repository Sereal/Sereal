#!perl
use strict;
use warnings;

use Sereal::Path;
use Sereal::Encoder qw/encode_sereal/;
use Test::More skip_all => 'TODO';

my $a = {};
my $data = [ $a, $a ];
my $encoded = encode_sereal($data);

my $sp = Sereal::Path->new($encoded);
my $val = eval { $sp->value('$[1]') };
is_deeply($val, $a, 'decode REFP');

my $str = 'long_test_string';
$encoded = encode_sereal([$str, $str], { dedupe_strings => 1 });
$val = eval { $sp->value('$[1]') };
is_deeply($val, $str, 'decode COPY');

done_testing();
