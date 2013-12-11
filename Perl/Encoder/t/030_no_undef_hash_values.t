#!perl
use strict;
use warnings;
use Sereal::Encoder qw(encode_sereal);

use Test::More tests => 8;

# without the option
my $encoded = encode_sereal({ key_aaa => 0, key_bbb => '', key_ccc => undef }, { no_undef_hash_values => 0 });
like($encoded, qr/key_aaa/, "contains key with 0 value");
like($encoded, qr/key_bbb/, "contains key with empty value");
like($encoded, qr/key_ccc/, "contains keys with undef value");
unlike($encoded, qr/key_ddd/, "does not contain non-existent key");

# with the option
my $encoded2 = encode_sereal({ key_aaa => 0, key_bbb => '', key_ccc => undef }, { no_undef_hash_values => 1 });
like($encoded2, qr/key_aaa/, "contains key with 0 value");
like($encoded2, qr/key_bbb/, "contains key with empty value");
unlike($encoded2, qr/key_ccc/, "does not contain key with undef value");
unlike($encoded2, qr/key_ddd/, "does not contain key with non-existent value");
