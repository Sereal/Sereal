#!perl
use strict;
use warnings;
use lib 't/lib';
use Test::More;
use Config;
use File::Spec;
use File::Temp qw(tempdir);
use Sereal::TestSet qw(Header short_string);
use Sereal::Decoder;
use Sereal::TestSet::Corpus qw(
    write_packet_set read_packet has_source packet_names pack_corpus unpack_corpus
);

# Fixed bytes from the archive format, independent of native integer width.
my @offsets = (
    [ 0,          '0000000000000000' ],
    [ 1,          '0100000000000000' ],
    [ 4294967295, 'ffffffff00000000' ],
);
if ($Config{uvsize} >= 8) {
    push @offsets,
        [ 4294967296,       '0000000001000000' ],
        [ 9007199254740993, '0100000000002000' ],
        [ ~0,               'ffffffffffffffff' ];
}
for my $case (@offsets) {
    my ($offset, $hex) = @$case;
    is(unpack('H*', Sereal::TestSet::Corpus::_pack_offset($offset)), $hex,
        "offset $offset has the specified little-endian bytes");
    is(Sereal::TestSet::Corpus::_unpack_offset(pack('H*', $hex)), $offset,
        "offset $offset can be read without native 64-bit pack support");
}
if ($Config{uvsize} < 8) {
    eval { Sereal::TestSet::Corpus::_unpack_offset(pack('H*', '0000000001000000')) };
    like($@, qr/Corpus offset exceeds the native integer range/,
        'oversized archive offsets are rejected rather than truncated');
    eval { Sereal::TestSet::Corpus::_pack_offset(4294967296) };
    like($@, qr/Corpus offset exceeds the native integer range/,
        'oversized archive offsets cannot be written with truncated high bits');
}

my $root = tempdir(CLEANUP => 1);
my %packets = (
    zero   => Header(6) . "\0",
    one    => Header(6) . "\x07",
    binary => Header(6) . short_string("\0\xff"),
);
write_packet_set($root, 'portable', '6.000_003', \%packets);
my $path = pack_corpus($root);
ok(-f $path, 'packed archive created');
ok(!-e File::Spec->catfile($root, 'sources.tsv'), 'packed reads do not use loose sources');
open my $fh, '<:raw', $path or die "Cannot open test archive: $!";
read($fh, my $header, 16) == 16 or die 'Cannot read archive header';
close $fh or die "Cannot close test archive: $!";
my $index_offset = 16;
$index_offset += length($_) for values %packets;
is($header, "SRLPC02\n" . pack('V2', $index_offset, 0),
    'portable writer preserves the existing eight-byte offset format');
ok(has_source($root, 'portable'), 'packed source index can be loaded');
is_deeply([packet_names($root, 'portable')], [sort keys %packets],
    'packed packet names can be listed');
for my $name (sort keys %packets) {
    is(read_packet($root, 'portable', $name), $packets{$name}, "packed $name bytes preserved");
}
ok(unpack_corpus($root), 'packed archive can be unpacked');
unlink $path or die "Cannot remove temporary archive: $!";
for my $name (sort keys %packets) {
    is(read_packet($root, 'portable', $name), $packets{$name}, "unpacked $name bytes preserved");
}
pack_corpus($root);
is(read_packet($root, 'portable', 'binary'), $packets{binary}, 'unpacked corpus can be repacked');

done_testing;
