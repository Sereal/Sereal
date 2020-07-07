#!perl
use strict;
use warnings;
use File::Spec;
use lib File::Spec->catdir(qw(t lib));

BEGIN {
    lib->import('lib')
        if !-d 't';
}

use Sereal::Decoder;
use Sereal::TestSet qw(:all);
use Test::More;

# plan tests => 56;

sub MONSTER_BLOB () { "\0" x (100 * 1024 * 1024) } # 100MB

my $ok= have_encoder_and_decoder();
if ( not $ok ) {
    plan skip_all => 'Did not find right version of encoder';
}
else {
    my %tests = (
        snappy  => Sereal::Encoder::SRL_SNAPPY(),
        zlib    => Sereal::Encoder::SRL_ZLIB(),
        zstd    => Sereal::Encoder::SRL_ZSTD(),
    );
    for my $compressor (sort keys %tests) {
        my $enc= Sereal::Encoder->new({ compress => $tests{$compressor} });
        my $hugeblob= $enc->encode(MONSTER_BLOB);
        my $dec= Sereal::Decoder->new();
        eval { $dec->decode($hugeblob) };
        like(
            $@,
            qr/^Sereal: Error: The expected uncompressed size is larger than the allowed maximum size/,
            "${compressor}-packed blob is too large to decode",
        );
    }

    # XXX: The values come from inspecting the encoded data with author_tools/hobodecoder.pl
    {
        my $enc= Sereal::Encoder->new({ compress => $tests{snappy} });
        my $hugeblob= $enc->encode(MONSTER_BLOB);

        # Snappy compression
        is(vec($hugeblob, 4, 8), 36);

        # Lets validate we have the expected uncompress size: 100MB
        is(vec($hugeblob, 10, 8), 133);
        is(vec($hugeblob, 11, 8), 128);
        is(vec($hugeblob, 12, 8), 128);
        is(vec($hugeblob, 13, 8), 50);

        # Pretend we only have 10MB
        vec($hugeblob, 13, 8) = 5;

        my $dec= Sereal::Decoder->new();
        is(
            eval { $dec->decode($hugeblob) },
            undef,
            'snappy with forged uncompressed size fails',
        );
        like(
            $@,
            qr/^Sereal: Error: Snappy decompression of Sereal packet payload failed with error -3!/,
            'snappy zipbomb was defused with CSNAPPY_E_OUTPUT_OVERRUN',
        );
    }

    {
        my $enc= Sereal::Encoder->new({ compress => $tests{zlib} });
        my $hugeblob= $enc->encode(MONSTER_BLOB);

        # zlib compression
        is(vec($hugeblob, 4, 8), 52);

        # Lets validate we have the expected uncompress size: 100MB
        is(vec($hugeblob, 6, 8), 133);
        is(vec($hugeblob, 7, 8), 128);
        is(vec($hugeblob, 8, 8), 128);
        is(vec($hugeblob, 9, 8), 50);

        # Pretend we only have 10MB
        vec($hugeblob, 9, 8) = 5;

        my $dec= Sereal::Decoder->new();
        is(
            eval { $dec->decode($hugeblob) },
            undef,
            'zlib with forged uncompressed size fails',
        );
        like(
            $@,
            qr/^Sereal: Error: ZLIB decompression of Sereal packet payload failed with error -5!/,
            'zlib zipbomb was defused with MZ_BUF_ERROR',
        );
    }

    {
        my $enc= Sereal::Encoder->new({ compress => $tests{zstd} });
        my $hugeblob= $enc->encode(MONSTER_BLOB);

        # zstd compression
        is(vec($hugeblob, 4, 8), 68);

        # Lets validate we have the expected uncompress size: 100MB
        is(vec($hugeblob, 25, 8), 128);
        is(vec($hugeblob, 26, 8), 128);
        is(vec($hugeblob, 27, 8), 128);
        is(vec($hugeblob, 28, 8), 50);

        # Pretend we only have 10MB
        vec($hugeblob, 28, 8) = 5;

        my $dec= Sereal::Decoder->new();
        is(
            eval { $dec->decode($hugeblob) },
            undef,
            'zstd with forged uncompressed size fails',
        );
        like(
            $@,
            qr/^Sereal: Error: The expected uncompressed size is larger than the allowed maximum size/,
            'zstd zipbomb was defused',
        );
    }
}

done_testing();
