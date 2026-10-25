#!perl
use strict;
use warnings;
use File::Spec;
use Test::More;
use lib File::Spec->catdir(qw(t lib));
BEGIN { lib->import('lib') if !-d 't' }
use Sereal::TestSet qw(have_encoder_and_decoder);
use Sereal::Encoder;

if ( !have_encoder_and_decoder() ) {
    plan skip_all => 'Did not find right version of decoder';
    exit 0;
}

# Encoding a hash must not disturb the iterator that a caller's each() loop
# over the same hash relies on. Every loop is bounded so a regression shows
# up as a failure instead of a hang.

my @unexpected_warnings;
local $SIG{__WARN__} = sub { push @unexpected_warnings, @_ };

our ( %target, $current_key, $grown, $in_encode );

{

    package EachIterFreeze;
    sub FREEZE { return $_[0]{v} }
    sub THAW   { my ( $class, $serializer, $v ) = @_; bless { v => $v }, $class }
}
{

    package EachIterDeleteCurrent;
    sub TIESCALAR { bless [ $_[1] ], $_[0] }

    sub FETCH {
        delete $main::target{$main::current_key} if defined $main::current_key;
        return $_[0][0];
    }
    sub STORE { $_[0][0] = $_[1] }
}
{

    package EachIterGrow;
    sub TIESCALAR { bless [ $_[1] ], $_[0] }

    sub FETCH {
        if ( $main::in_encode && !$main::grown++ ) {
            $main::target{"grow_$_"} = $_ for 1 .. 64;
        }
        return $_[0][0];
    }
    sub STORE { $_[0][0] = $_[1] }
}

# Returns how many entries each() visited, stopping at twice the hash size.
sub count_each_while_encoding {
    my ( $enc, $hash ) = @_;
    my $limit = 2 * scalar( keys %$hash );
    my $n     = 0;
    while ( my ( $k, $v ) = each %$hash ) {
        $n++;
        last if $n > $limit;
        $enc->encode($hash);
    }
    keys %$hash;    # reset the iterator for the next case
    return $n;
}

# Enough keys that several share a bucket, so restoring the iterator into
# the wrong bucket loses entries deterministically.
my $n_keys = 200;

my @cases = (
    [ 'reference values, default options',  1, {} ],
    [ 'reference values, sort_keys',        1, { sort_keys => 1 } ],
    [ 'plain values, default options',      0, {} ],
    [ 'plain values, sort_keys',            0, { sort_keys        => 1 } ],
    [ 'reference values, freeze_callbacks', 2, { freeze_callbacks => 1 } ],
);

for my $case (@cases) {
    my ( $name, $kind, $opts ) = @$case;
    my %h;
    for my $i ( 1 .. $n_keys ) {
        $h{"k$i"} =
              $kind == 2 ? bless( { v => $i }, 'EachIterFreeze' )
            : $kind == 1 ? [$i]
            :              $i;
    }
    my $enc = Sereal::Encoder->new($opts);
    is( count_each_while_encoding( $enc, \%h ), $n_keys, "each() sees every key once: $name" );
}

{
    my %h   = map { ( "k$_" => [$_] ) } 1 .. 5;
    my $enc = Sereal::Encoder->new;
    my $n   = 0;
    while ( my ( $k, $v ) = each %h ) {
        $n++;
        last if $n > 20;
        delete $h{$k};
        $enc->encode( \%h );
    }
    is( $n, 5, 'each() sees every key when deleting the current entry before encoding' );
    is( scalar( keys %h ), 0, 'hash is empty after deleting every entry' );
}

{
    %target = ();
    for my $i ( 1 .. 5 ) {
        $target{"k$i"} = undef;
        tie $target{"k$i"}, 'EachIterDeleteCurrent', $i;
    }
    my $enc = Sereal::Encoder->new;
    my $n   = 0;
    while ( my ( $k, $v ) = each %target ) {
        $n++;
        last if $n > 20;
        $current_key = $k;
        eval { $enc->encode( \%target ); 1 };
    }
    $current_key = undef;
    ok(
        $n <= 20,
        "each() loop terminates when a callback deletes the current entry ($n iterations)"
    );
    %target = ();
}

{
    %target = ();
    $grown  = 0;
    for my $i ( 1 .. 5 ) {
        $target{"k$i"} = undef;
        tie $target{"k$i"}, 'EachIterGrow', $i;
    }
    my $enc = Sereal::Encoder->new;
    my $n   = 0;
    my @iterator_warnings;

    # The 64 keys added during the first encode may legitimately be visited by
    # the rest of the loop, so this case needs a larger bound. Perl may warn
    # on the next each() after that deliberate insertion. Capture warnings
    # only while advancing the iterator; other warnings fail the test below.
    while ( my ( $k, $v ) = do {
        local $SIG{__WARN__} = sub { push @iterator_warnings, @_ };
        each %target;
    } ) {
        $n++;
        last if $n > 100;
        local $in_encode = 1;
        eval { $enc->encode( \%target ); 1 };
    }
    ok( $n <= 100, "each() loop terminates when a callback splits the hash ($n iterations)" );
    ok( $grown,    'the growing callback ran' );
    my @unexpected = grep {
        !/^Use of each\(\) on hash after insertion without resetting hash iterator results in undefined behavior(?:, Perl interpreter: 0x[0-9a-f]+)? at /i
    } @iterator_warnings;
    is_deeply( \@unexpected, [], 'iterator advance emitted only the expected insertion warning' );
    %target = ();
}

# An untied hash with magic of its own: the iterator lives in the HV and must
# be kept just like for a plain hash.
SKIP: {
    my $expected = scalar( keys %ENV );
    skip "need at least 2 environment variables, have $expected", 1 if $expected < 2;
    my $enc = Sereal::Encoder->new;
    is( count_each_while_encoding( $enc, \%ENV ), $expected, "each() sees every key once: %ENV" );
}

is_deeply( \@unexpected_warnings, [], 'no unexpected warnings while exercising hash iterators' );
done_testing;
