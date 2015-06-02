#!perl
use strict;
use warnings;

use Sereal::Path;
use Sereal::Encoder qw/encode_sereal SRL_SNAPPY SRL_ZLIB/;
use Test::More tests => 9;

my $data = [ ('a') x 1024 ];
my %options = (
    v1                    => { protocol_version => 1 },
    v1_snappy             => { protocol_version => 1, snappy => 1 },
    v1_snappy_incremental => { protocol_version => 1, snappy_incr => 1 },
    v2                    => { protocol_version => 2 },
    v2_snappy             => { protocol_version => 2, snappy => 1 },
    v2_snappy_incremental => { protocol_version => 2, snappy_incr => 1 },
    v3                    => { protocol_version => 3 },
    v3_snappy             => { protocol_version => 3, compress => SRL_SNAPPY },
    v3_zlib               => { protocol_version => 3, compress => SRL_ZLIB },
);

foreach my $srl (keys %options) {
    my $encoded = encode_sereal($data, $options{$srl});
    my $ok = eval { Sereal::Path->new($encoded); 1; };
    my $err = $@ || 'Zombie error';
    ok($ok, "don't die on $srl input") or diag $err;
}

done_testing();
