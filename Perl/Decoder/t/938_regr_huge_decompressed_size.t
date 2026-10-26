#!perl
use strict;
use warnings;
use File::Spec;
use lib File::Spec->catdir(qw(t lib));

BEGIN { lib->import('lib') if !-d 't' }

use Test::More;
use Config;
use Sereal::Decoder;
use File::Spec;
use Sereal::TestSet::Corpus qw(read_packet);

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
my $zlib_2_53 = $SRL_MAGIC . chr(0x36) . chr(0) . "\x80\x80\x80\x80\x80\x80\x80\x10" . chr(0);

# v6, type 4 = zstd, empty header suffix; compressed size varint, then a
# zstd frame header (single segment, 8-byte content size) declaring 2^53.
my $zstd_frame = "\x28\xB5\x2F\xFD" . "\xE0" . pack( "V2", 0, 2**21 );
my $zstd_2_53  = $SRL_MAGIC . chr(0x46) . chr(0) . chr( length $zstd_frame ) . $zstd_frame;

sub decode_in_child {
    my ( $packet, $opts ) = @_;
    my $code = sprintf( <<'CODE', unpack( "H*", $packet ), $opts || '' );
use Sereal::Decoder;
use Config;
my $packet = pack("H*", "%s");
my $expected_error = $Config{uvsize} < 8
    ? qr/too large for this platform|varint (?:too big|overflows U32)/
    : qr/Insufficient memory|too large for this platform/;
eval { Sereal::Decoder->new({%s})->decode($packet) };
print $@ =~ $expected_error ? "caught\n" : "unexpected: $@\n";
CODE
    my $out = '';
    my $pid = open( my $fh, '-|' ) // die "fork failed: $!";
    if ( !$pid ) {
        open( STDERR, '>&', \*STDOUT );
        exec( $^X, '-Mblib', '-It/lib', '-e', $code ) or exit 255;
    }
    $out .= $_ while <$fh>;
    close $fh;
    return ( $? >> 8, $out );
}

foreach my $case (
    [ 'zlib declaring 2^53 bytes', $zlib_2_53 ],
    [ 'zstd declaring 2^53 bytes', $zstd_2_53 ],
    )
{
    my ( $name,   $packet ) = @$case;
    my ( $status, $out )    = decode_in_child($packet);
    is( $status, 0, "$name: decoder process survives" );

    # Match a line rather than the whole output: allocator diagnostics such
    # as a sanitizer's "failed to allocate" warning may precede it.
    like( $out, qr/^caught$/m, "$name: dies with a catchable exception" );
    unlike( $out, qr/^unexpected/m, "$name: no unexpected error" );
}

foreach my $case (
    [ 'zlib declaring 2^53 bytes', $zlib_2_53 ],
    [ 'zstd declaring 2^53 bytes', $zstd_2_53 ],
    )
{
    my ( $name, $packet ) = @$case;
    my $ok = eval {
        Sereal::Decoder->new( { max_uncompressed_size => 1024 } )->decode($packet);
        1;
    };
    my $error = $@;
    ok( !$ok, "$name: rejected by max_uncompressed_size" );
    # A 32-bit decoder rejects the size before it can compare it with the
    # configured limit: zlib's varint overflows UV, and zstd's size overflows
    # STRLEN/size_t. Both are controlled rejections, not allocation failures.
    my $expected_error = $Config{uvsize} < 8
        ? ( $name =~ /^zlib/ ? qr/varint (?:too big|overflows U32)/ : qr/too large for this platform/ )
        : qr/expected uncompressed size is larger than the allowed maximum/;
    like( $error, $expected_error, "$name: size rejected before allocation" );
}

my $data = { a => [ ( "x" x 100 ) x 10 ] };
my $normal_zlib = read_packet(
    File::Spec->catdir( 't', 'data', 'packet_corpus' ),
    'decoder-features', 'huge_decompressed_size/normal_zlib'
);
is_deeply(
    Sereal::Decoder->new->decode($normal_zlib), $data,
    'normal zlib corpus packet still decodes'
);

done_testing();
