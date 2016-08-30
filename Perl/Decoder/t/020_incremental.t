#!perl
use strict;
use warnings;
use Data::Dumper;
use File::Spec;
use Devel::Peek;

use lib File::Spec->catdir(qw(t lib));
BEGIN {
    lib->import('lib')
        if !-d 't';
}

use Sereal::TestSet qw(:all);
use Test::More tests => 8 + (2*6) + ( 31 * 3 );
use Sereal::Decoder qw(decode_sereal);
use Sereal::Decoder::Constants qw(:all);


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

SCOPE: {
    my $d = Sereal::Decoder->new({incremental => 1});
    my $data = '';
    $data .= SRL_MAGIC_STRING . chr(1).chr(0).chr(SRL_HDR_POS | $_) for 1..5;

    for (1..5) {
      my $out = $d->decode($data);
      is("$out", "$_", "Incremental section no. $_ yields right output");
    }

    is($data, '', "Data is gone after incremental parsing");
}

SCOPE: {
    my $d = Sereal::Decoder->new({incremental => 1});
    my $data = '';
    $data .= SRL_MAGIC_STRING . chr(1).chr(0).chr(SRL_HDR_POS | $_) for 1..5;
    utf8::upgrade($data);

    for (1..5) {
      my $out = $d->decode($data);
      is("$out", "$_", "Incremental section no. $_ yields right output utf8 mode");
    }

    is($data, '', "Data is gone after incremental parsing utf8 mode");
}

SKIP: {
    my $have_enc = have_encoder_and_decoder();
    if (not $have_enc) {
        skip "Need encoder for chunk tests", 31 * 3;
    }
    else {
        require Sereal::Encoder;
        Sereal::Encoder->import("encode_sereal", "SRL_ZLIB");

        for my $tuple ( [ raw         => [] ],
                        [ snappy_incr => [ { snappy_incr => 1 } ] ],
                        [ zlib        => [ { compress => SRL_ZLIB() } ] ] )
        {
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
                or note("Error: $err. Data structures decoded up to that point:\n" . Data::Dumper::Dumper(\@out));

            is($out[$_-1], $_, "Decoding multiple packets from single string works ($name: $_)")
                for 1..$n;
        }
    }
}

