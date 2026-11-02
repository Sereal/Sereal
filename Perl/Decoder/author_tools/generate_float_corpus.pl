#!/usr/bin/env perl
use strict;
use warnings;
use lib 't/lib';
use Sereal::Decoder;
use Sereal::TestSet         qw(Header floating_point_wire_cases native_long_double_wire_cases);
use Sereal::TestSet::Corpus qw(write_packet_set unpack_corpus pack_corpus prune_unreferenced);
# The byte patterns are independent wire-format examples, so regeneration
# does not depend on the build host's NV or long-double ABI. Native producer
# captures serve a different purpose: they test what that compiler and Perl
# actually emit. README.corpus.md describes the coverage and contribution path.
my $root = 't/data/packet_corpus';
unpack_corpus($root);

for my $order ( 'little', 'big' ) {
    my %packets;
    for my $version ( 1 .. ( $order eq 'little' ? 7 : 6 ) ) {
        for my $case ( floating_point_wire_cases() ) {
            my ( $name, $tag, $hex, undef, $min_version ) = @$case;
            next if $version < $min_version;
            my $bytes = pack( 'H*', $hex );
            $bytes = reverse($bytes) if $order eq 'big';
            $packets{"v$version/$name"} = Header($version) . chr($tag) . $bytes;
        }
    }
    write_packet_set( $root, "float-wire-$order", '7.000_001', \%packets );
}
my %native;
for my $case ( native_long_double_wire_cases() ) {
    my ( $name, $format, $payload ) = @$case;
    # The literal tag IDs and canonical descriptor length form an independent
    # wire-format oracle. Reusing the Encoder to build these packets would
    # allow the fixture to reproduce the encoding mistake it should detect.
    $native{$name} =
          Header(7)
        . chr(62) # EXTEND
        . chr(1)  # DECLARE
        . chr(3)  # declaration payload length
        . chr(1)  # LONG_DOUBLE declaration kind
        . chr($format)
        . chr( length($payload) )
        . chr(36)
        . $payload;
}
write_packet_set( $root, 'native-long-double-v7', '7.000_001', \%native );
prune_unreferenced($root);
pack_corpus($root);
