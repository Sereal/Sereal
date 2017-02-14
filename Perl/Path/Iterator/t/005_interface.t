#!perl
use strict;
use warnings;

use Test::More;
use Data::Dumper;
use Test::Exception;
use Sereal::Path::Iterator;
use Sereal::Encoder qw/encode_sereal/;

subtest "parse document", sub {
    (my $srl_enc_version = $Sereal::Encoder::VERSION) =~ s/_//;

    my %options = (
        v1                    => { protocol_version => 1 },
        v1_snappy             => { protocol_version => 1, snappy => 1 },
        v1_snappy_incremental => { protocol_version => 1, snappy_incr => 1 },
        v2                    => { protocol_version => 2 },
        v2_snappy             => { protocol_version => 2, snappy => 1 },
        v2_snappy_incremental => { protocol_version => 2, snappy_incr => 1 },
        v3                    => { protocol_version => 3 },
        v3_snappy             => { protocol_version => 3, compress => Sereal::Encoder::SRL_SNAPPY },
        v3_zlib               => { protocol_version => 3, compress => Sereal::Encoder::SRL_ZLIB },
    );

    if (int($srl_enc_version) >= 4) {
        $options{v4}        = { protocol_version => 4 };
        $options{v4_snappy} = { protocol_version => 4, compress => Sereal::Encoder::SRL_SNAPPY };
        $options{v4_zlib}   = { protocol_version => 4, compress => Sereal::Encoder::SRL_ZLIB };
        $options{v4_zstd}   = { protocol_version => 4, compress => 3 }; # SRL_ZSTD
    }

    my $body = 'a' x (10 * 1024);
    foreach (sort keys %options) {
        my $enc = encode_sereal($body, $options{$_});
        lives_ok(sub { Sereal::Path::Iterator->new($enc) },
                 "expect constructor from $_ document to live");
    }
};

subtest "set document", sub {
    my $spi = Sereal::Path::Iterator->new();
    lives_ok(sub { $spi->set(encode_sereal(100)) }, 'expect set() to live');
    is($spi->decode(), 100, 'decode 100');
    lives_ok(sub { $spi->set(encode_sereal(200)) }, 'expect set() to live');
    is($spi->decode(), 200, 'decode 200');
};

subtest "reset document", sub {
    my $spi = Sereal::Path::Iterator->new(encode_sereal(100));
    lives_ok(sub { $spi->reset() }, 'expect reset() to live');
    is($spi->decode(), 100, 'decode 100');
};

done_testing();
