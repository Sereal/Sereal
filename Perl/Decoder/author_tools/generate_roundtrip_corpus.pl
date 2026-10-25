#!/usr/bin/env perl
use strict;
use warnings;
use File::Spec;
use lib File::Spec->catdir(qw(t lib));

use Sereal::Encoder;
use Sereal::TestSet qw(setup_tests roundtrip_corpus_name);
use Sereal::TestSet::Corpus qw(
    write_packet_set prune_unreferenced unpack_corpus pack_corpus
);

my $root = File::Spec->catdir( 't', 'data', 'packet_corpus' );
unpack_corpus($root);
# Each wire protocol gets a compact baseline corpus. Larger stress cases stay
# in the live round-trip suite, which runs when Encoder supports that protocol.
my ( @tests, %v6_names );
for my $version ( 1 .. 6 ) {
    setup_tests($version);
    my @version_tests = Sereal::TestSet::_get_roundtrip_tests();
    my $encoder = Sereal::Encoder->new( { protocol_version => $version } );
    my %packets;
    for my $index ( 0 .. $#version_tests ) {
        my ( $name, $value ) = @{ $version_tests[$index] };
        my $logical_name = roundtrip_corpus_name( $index, $name );
        my $packet = $encoder->encode($value);
        next if length($packet) > 16 * 1024;
        $packets{$logical_name} = $packet;
        $v6_names{$logical_name} = 1 if $version == 6;
    }
    my $source = "roundtrip-v$version-plain";
    my $manifest_hash = write_packet_set(
        $root, $source, Sereal::Encoder->VERSION, \%packets,
    );
    print "Wrote $source manifest $manifest_hash with ",
        scalar( keys %packets ), " packets\n";
    @tests = @version_tests if $version == 6;
}

my %v6_profiles = (
    plain_canonical   => { canonical => 1 },
    'freeze-thaw'     => { freeze_callbacks => 1 },
    dedupe_strings    => { dedupe_strings => 1 },
    readonly          => { set_readonly => 1 },
    sort_keys         => { sort_keys => 1 },
    sort_keys_perl    => { sort_keys => 2 },
    sort_keys_perl_rev => { sort_keys => 3 },
    snappy            => { snappy => 1 },
    snappy_incr       => { snappy_incr => 1 },
    snappy_canon      => { snappy => 1, canonical => 1 },
    snappy_incr_canon => { snappy_incr => 1, canonical => 1 },
    zlib              => { compress => Sereal::Encoder::SRL_ZLIB() },
    zlib_force        => { compress => Sereal::Encoder::SRL_ZLIB(), compress_threshold => 0 },
    zstd              => { compress => Sereal::Encoder::SRL_ZSTD() },
    zstd_force        => { compress => Sereal::Encoder::SRL_ZSTD(), compress_threshold => 0 },
);

for my $profile ( sort keys %v6_profiles ) {
    setup_tests(6);
    my $encoder = Sereal::Encoder->new( {
        protocol_version => 6,
        %{ $v6_profiles{$profile} },
    } );
    my %packets;
    for my $index ( 0 .. $#tests ) {
        my ( $name, $value ) = @{ $tests[$index] };
        my $logical_name = roundtrip_corpus_name( $index, $name );
        next unless $v6_names{$logical_name};
        $packets{$logical_name} = $encoder->encode($value);
    }
    my $source = "roundtrip-v6-$profile";
    my $manifest_hash = write_packet_set(
        $root, $source, Sereal::Encoder->VERSION, \%packets,
    );
    print "Wrote $source manifest $manifest_hash\n";
}

prune_unreferenced($root);
pack_corpus($root);
