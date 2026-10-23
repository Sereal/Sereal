#!perl
use strict;
use warnings;
use File::Spec;
use lib File::Spec->catdir(qw(t lib));

BEGIN { lib->import('lib') if !-d 't' }

use Test::More;
use Sereal::Decoder;

# A compressed document declares its uncompressed size, and the decoder
# allocates the output buffer from that number before inflating anything.
# A tiny document declaring an impossibly large size used to make perl's
# allocator terminate the whole process ("Out of memory in perl:util:...")
# so eval could not catch it. It must die with an ordinary exception.
#
# The pre-fix behaviour is a process exit, so each decode runs in a child.

my $SRL_MAGIC = "=\xF3rl";

# v6, type 3 = zlib, empty header suffix; uncompressed size varint for 2^53,
# then compressed size 0 (the buffer is allocated before inflation).
my $zlib_2_53 = $SRL_MAGIC . chr(0x36) . chr(0)
    . "\x80\x80\x80\x80\x80\x80\x80\x10" . chr(0);

# v6, type 4 = zstd, empty header suffix; compressed size varint, then a
# zstd frame header (single segment, 8-byte content size) declaring 2^53.
my $zstd_frame = "\x28\xB5\x2F\xFD" . "\xE0" . pack("Q<", 2**53);
my $zstd_2_53 = $SRL_MAGIC . chr(0x46) . chr(0)
    . chr(length $zstd_frame) . $zstd_frame;

sub decode_in_child {
    my ($packet, $opts) = @_;
    my $code = sprintf(<<'CODE', unpack("H*", $packet), $opts || '');
use Sereal::Decoder;
my $packet = pack("H*", "%s");
eval { Sereal::Decoder->new({%s})->decode($packet) };
print $@ =~ /Insufficient memory/ ? "caught\n" : "unexpected: $@\n";
CODE
    my $out = '';
    my $pid = open(my $fh, '-|') // die "fork failed: $!";
    if (!$pid) {
        open(STDERR, '>&', \*STDOUT);
        exec($^X, '-Mblib', '-It/lib', '-e', $code) or exit 255;
    }
    $out .= $_ while <$fh>;
    close $fh;
    return ($? >> 8, $out);
}

foreach my $case (
    [ 'zlib declaring 2^53 bytes', $zlib_2_53 ],
    [ 'zstd declaring 2^53 bytes', $zstd_2_53 ],
) {
    my ($name, $packet) = @$case;
    my ($status, $out) = decode_in_child($packet);
    is($status, 0, "$name: decoder process survives");
    is($out, "caught\n", "$name: dies with a catchable exception");
}

foreach my $case (
    [ 'zlib declaring 2^53 bytes', $zlib_2_53 ],
    [ 'zstd declaring 2^53 bytes', $zstd_2_53 ],
) {
    my ($name, $packet) = @$case;
    my $ok = eval {
        Sereal::Decoder->new({ max_uncompressed_size => 1024 })->decode($packet);
        1;
    };
    ok(!$ok, "$name: rejected by max_uncompressed_size");
    like($@, qr/expected uncompressed size is larger than the allowed maximum/,
        "$name: max_uncompressed_size error message");
}

SKIP: {
    skip 'Sereal::Encoder not available', 1
        if !eval { require Sereal::Encoder; Sereal::Encoder->VERSION(3.002); 1 };
    my $data = { a => [ ("x" x 100) x 10 ] };
    my $enc = Sereal::Encoder->new({
        compress           => Sereal::Encoder::SRL_ZLIB(),
        compress_threshold => 0,
    })->encode($data);
    is_deeply(Sereal::Decoder->new->decode($enc), $data,
        'normal zlib round trip still works');
}

done_testing();
