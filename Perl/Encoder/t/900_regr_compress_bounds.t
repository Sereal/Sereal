#!perl
use strict;
use warnings;
use Test::More;
use Sereal::Encoder qw(
    SRL_SNAPPY
    SRL_ZLIB
    SRL_ZSTD
);

my @modes = (
    [ 'snappy_incr', { snappy_incr => 1, compress_threshold => 0 } ],
    [ 'zlib',        { compress => SRL_ZLIB, compress_threshold => 0 } ],
    [ 'zstd',        { compress => SRL_ZSTD, compress_threshold => 0 } ],
);

# Sizes chosen to hit varint length boundaries (e.g. < 54, 54, 73, 127, 128, 150, 2048)
my @sizes = ( 10, 54, 73, 127, 128, 150, 2048 );

plan tests => scalar(@modes) * scalar(@sizes) * 2;

# Deterministic pseudo-random incompressible byte string
srand(42);
my $incompressible_pool = join '', map { chr(int(rand(256))) } 1 .. 2500;

for my $mode (@modes) {
    my ( $name, $opts ) = @$mode;
    my $enc = Sereal::Encoder->new($opts);
    for my $len (@sizes) {
        my $incompressible = substr( $incompressible_pool, 0, $len );
        my $compressible   = 'A' x $len;

        my $out_inc = eval { $enc->encode($incompressible) };
        ok( !$@ && defined($out_inc) && length($out_inc) > 0,
            "$name incompressible len=$len encodes cleanly" )
            or diag("Error: $@");

        my $out_cmp = eval { $enc->encode($compressible) };
        ok( !$@ && defined($out_cmp) && length($out_cmp) > 0,
            "$name compressible len=$len encodes cleanly" )
            or diag("Error: $@");
    }
}
