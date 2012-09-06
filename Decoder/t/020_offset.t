#!perl
use strict;
use warnings;
use Sereal::Decoder qw(decode_sereal);
use Sereal::Decoder::Constants qw(:all);
use Data::Dumper;
use File::Spec;
use Devel::Peek;

use lib File::Spec->catdir(qw(t lib));
BEGIN {
    lib->import('lib')
        if !-d 't';
}

use Sereal::TestSet qw(:all);

use Test::More tests => 8;

# Simple test to see whether we can get the number of bytes consumed
# and whether offset works
SCOPE: {
    my $d = Sereal::Decoder->new();
    my $data = SRL_MAGIC_STRING . chr(1).chr(0).chr(SRL_HDR_UNDEF);
    ok(!defined($d->decode($data. "GARBAGE")), "can decode with appended garbage");
    is($d->bytes_consumed, length($data), "consumed right number of bytes");

    ok(!defined($d->decode_with_offset($data, 0)), "can decode with zero offset");
    is($d->bytes_consumed, length($data), "consumed right number of bytes");

    ok(!defined($d->decode_with_offset("GARBAGE" . $data, length("GARBAGE"))), "can decode with offset");
    is($d->bytes_consumed, length($data), "consumed right number of bytes");

    ok(!defined($d->decode_with_offset("GARBAGE" . $data . "TRAILING", length("GARBAGE"))), "can decode with offset and trailing garbage");
    is($d->bytes_consumed, length($data), "consumed right number of bytes");
}

