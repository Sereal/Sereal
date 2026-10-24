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

{
    package LimitOptionTie;
    sub TIESCALAR { bless { value => $_[1], fetches => 0 }, $_[0] }
    sub FETCH { ++$_[0]{fetches}; return $_[0]{value} }

    package ChangingLimitOptionTie;
    sub TIESCALAR { bless { fetches => 0 }, $_[0] }
    sub FETCH { return ++$_[0]{fetches} == 1 ? -1 : 7 }
}

# A tied option can retain a cached integer from before it was tied. Check
# the fetched value, and invoke FETCH only once while validating it.
for my $opt (@options) {
    for my $case ( [ -1, 0 ], [ 0.5, 0 ], [ 0, 1 ], [ undef, 1 ], [ 7, 1 ] ) {
        my ($value, $valid) = @$case;
        my $label = defined($value) ? $value : 'undef';
        my $options = { $opt => 123 };
        my $tie = tie $options->{$opt}, 'LimitOptionTie', $value;
        my $ok = eval { Sereal::Decoder->new($options); 1 };
        is( !!$ok, !!$valid, "$opt tied FETCH $label is validated" );
        like( $@, qr/'$opt' option must be a non-negative integer/,
            "$opt tied FETCH $label has a useful error" ) unless $valid;
        is( $tie->{fetches}, 1, "$opt tied FETCH $label runs once" );
    }
    my $options = { $opt => 123 };
    my $tie = tie $options->{$opt}, 'ChangingLimitOptionTie';
    my $ok = eval { Sereal::Decoder->new($options); 1 };
    ok( !$ok, "$opt rejects the first, negative FETCH result" );
    like( $@, qr/'$opt' option must be a non-negative integer/,
        "$opt rejects a changing tied value with the limit error" );
    is( $tie->{fetches}, 1, "$opt does not read the changed second value" );
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
