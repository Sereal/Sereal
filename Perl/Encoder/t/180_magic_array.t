#!perl

use strict;
use warnings;

use Sereal::Encoder;
use Test::More tests => 1;

my $srl_encoder = Sereal::Encoder->new({no_shared_hashkeys => 1});

my @array;
$array[0] = 1;
$array[3] = 1;
my $last_index = $#array;

my $str = $srl_encoder->encode(\@array);

# if segfaul script is dropped unless pass() test
pass();
