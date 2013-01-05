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

use Test::More tests => 8 + ( 31 * 2 );

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

SKIP: {
    my $have_enc = have_encoder_and_decoder();
    if (not $have_enc) {
        skip "Need encoder for chunk tests", 31;
        die;
    }
    require Sereal::Encoder;
    Sereal::Encoder->import("encode_sereal");

    for my $tuple ( ['raw' => [] ], [ snappy_incr => [ { snappy_incr => 1 } ] ] ) {
        my ($name, $opts)= @$tuple;
        my $data;
        my $n = 30;
        $data .= encode_sereal($_, @$opts) for 1 .. $n;
        my $decoder = Sereal::Decoder->new;
        my @out;
        my $pos = 0;
        my $ok = eval {
            while (1) {
                push @out, $decoder->decode_with_offset($data, $pos);
                $pos += $decoder->bytes_consumed;
                last if $pos >= length($data)
                     or not $decoder->bytes_consumed;
            }
            1
        };
        my $err = $@ || 'Zombie error';
        ok($ok, "incremental decoder ($name) had no hissy fit")
            or note("Error: $err");

        is($out[$_-1], $_, "Decoding multiple packets from single string works ($name: $_)")
            for 1..$n;
    }
}

