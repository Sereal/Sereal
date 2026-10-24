#!perl
use strict;
use warnings;
use File::Spec;
use lib File::Spec->catdir(qw(t lib));

BEGIN { lib->import('lib') if !-d 't' }

use Test::More;
use Sereal::TestSet qw(Header);
use Sereal::Decoder;

# The limit options were read with SvUV() without validation: -1 became
# the largest unsigned value (no limit at all, so a deeply nested document
# could exhaust the C stack despite max_recursion_depth) and fractions
# were truncated (0.5 became 0, which disables a size limit). They must
# now be non-negative integers; 0 keeps its documented meaning.

my @options = qw(
    max_recursion_depth
    max_num_hash_entries
    max_num_array_entries
    max_string_length
    max_uncompressed_size
);

for my $opt (@options) {
    for my $bad ( -1, 0.5, 2.5, -0.5, 'lots', 9**9**9 ) {
        my $ok = eval { Sereal::Decoder->new( { $opt => $bad } ); 1 };
        ok( !$ok, "$opt => $bad is refused" );
        like( $@, qr/'$opt' option must be a non-negative integer/, "$opt => $bad: with a clear error" );
    }
    for my $good ( 0, 1, 10, '100', 1e6, 4294967296 ) {
        my $ok = eval { Sereal::Decoder->new( { $opt => $good } ); 1 };
        ok( $ok, "$opt => $good is accepted" ) or diag $@;
    }
}

# Valid values still take effect.
my $nested = Header(6) . ( "\x41" x 5 ) . "\x01";    # [[[[[1]]]]]
ok( !eval { Sereal::Decoder->new( { max_recursion_depth => 3 } )->decode($nested); 1 },
    'max_recursion_depth => 3 rejects a five level document' );
ok( eval { Sereal::Decoder->new( { max_recursion_depth => '10' } )->decode($nested); 1 },
    'max_recursion_depth => "10" accepts it' );
ok( !eval { Sereal::Decoder->new( { max_string_length => 2 } )->decode( Header(6) . "\x63abc" ); 1 },
    'max_string_length => 2 rejects a three byte string' );
ok( eval { Sereal::Decoder->new( { max_num_array_entries => 0 } )->decode($nested); 1 },
    'max_num_array_entries => 0 means no limit' );

done_testing;
