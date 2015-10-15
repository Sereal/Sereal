#!perl
use strict;
use warnings;

use Sereal::Path;
use Sereal::Encoder qw/encode_sereal/;
use Test::More;

my $a = {};
my $data = [ $a, $a ];
my $sp = Sereal::Path->new;
my $error = '';

# decode REFP
$sp->set(encode_sereal($data));
my $val = eval { $sp->value('$[1]') } or $error = $@;
is_deeply($val, $a, "decode REFP: $error");

# decode COPY
my $str = 'long_test_string';
$sp->set(encode_sereal([$str, $str], { dedupe_strings => 1 }));
$val = eval { $sp->value('$[1]') } or $error = $@;
is_deeply($val, $str, "decode COPY $error");

# decode ALIAS TODO
# $sp->set(encode_sereal([$str, $str], { dedupe_strings => 1, aliased_dedupe_strings => 1 }));
# $val = eval { $sp->value('$[1]') } or $error = $@;
# is_deeply($val, $str, "decode ALIAS $error");

done_testing();
