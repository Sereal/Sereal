#!perl
use strict;
use warnings;
use Test::More;
use lib 't/lib';
use Sereal::TestSet ();
use Sereal::Encoder qw(SRL_SNAPPY SRL_ZLIB SRL_ZSTD);
use Sereal::Decoder;

# Exercise both Snappy formats and the length prefixes used by Zlib/Zstd.
my @modes = (
    [ 'snappy',      1, 1, { compress => SRL_SNAPPY } ],
    [ 'snappy_incr', 2, 7, { compress => SRL_SNAPPY } ],
    [ 'zlib',        3, 7, { compress => SRL_ZLIB } ],
    [ 'zstd',        4, 7, { compress => SRL_ZSTD } ],
);
my @sizes = ( 0, 10, 54, 73, 127, 128, 150, 2048, 16383, 16384 );

# Repeatable bytes for inputs that are unlikely to shrink on compression.
srand(42);
my $pool    = join '', map { chr( int( rand(256) ) ) } 1 .. $sizes[-1];
my $decoder = Sereal::Decoder->new;

for my $mode (@modes) {
    my ( $name, $first, $last, $options ) = @$mode;
    for my $version ( $first .. $last ) {
        my $encoder = Sereal::Encoder->new( {
            %$options,
            protocol_version   => $version,
            compress_threshold => 0,
        } );
        for my $len (@sizes) {
            for my $case (
                [ incompressible => substr( $pool, 0, $len ) ],
                [ compressible   => 'A' x $len ],
                )
            {
                my ( $kind, $input ) = @$case;
                my $label  = "$name v$version $kind len=$len";
                my $packet = eval { $encoder->encode($input) };
                my $error  = $@;
                ok( !$error && defined($packet) && length($packet), "$label encodes" )
                    or diag($error);
                SKIP: {
                    skip 'encoding failed', 1 unless defined($packet);
                    my $output = eval { $decoder->decode($packet) };
                    is( $output, $input, "$label decodes" ) or diag($@);
                }
            }
        }
    }
}
done_testing();
