#!perl
use strict;
use warnings;
use Sereal;

use Test::More tests => 6;
ok(defined(\&Sereal::encode_sereal), 'encode_sereal defined in Sereal');
ok(defined(\&Sereal::decode_sereal), 'decode_sereal defined in Sereal');
ok(defined(\&Sereal::looks_like_sereal), 'looks_like_sereal defined in Sereal');

Sereal->import(':all');
ok(defined(\&encode_sereal), 'encode_sereal defined in main');
ok(defined(\&decode_sereal), 'decode_sereal defined in main');
ok(defined(\&looks_like_sereal), 'looks_like_sereal defined in main');

