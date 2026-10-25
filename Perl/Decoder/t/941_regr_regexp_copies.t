#!perl
use strict;
use warnings;
use File::Spec;
use lib File::Spec->catdir(qw(t lib));

BEGIN { lib->import('lib') if !-d 't' }

use Test::More;
use Sereal::TestSet qw(Header);
use Sereal::TestSet::Corpus qw(read_packet);
use Sereal::Decoder;
use re qw(regexp_pattern);

# A decoded REGEXP used to be the compiled regexp itself, swapped into the
# destination scalar. Perl only ever hands out copies of a compiled regexp
# (qr// and ${qr//} both do), and its copy-on-assign and free logic relies
# on that: assigning to a decoded bare regexp, or freeing a copy taken from
# a decoded qr//, overwrote or freed the regexp internals. The decoder now
# builds the same kind of copy perl does. The REGEXP pattern must also be
# a string; a REFP to an earlier REGEXP used to steal that value's body.

# These cases used to crash perl, so they run in a child process whose
# exit status is the assertion.
sub child_survives {
    my ( $code, $name ) = @_;
    my $status = system( $^X, '-Mblib', '-It/lib', '-MSereal::Decoder',
        '-MSereal::TestSet=Header', '-e', 'use strict; use warnings; ' . $code
    );
    is( $status, 0, "$name: child exits cleanly" );
}

my $re_body = "\x31\x63abc\x61i";    # REGEXP, SHORT_BINARY "abc", SHORT_BINARY "i"
my %bare    = (
    'bare REGEXP as the document body' => $re_body,
    'bare REGEXP as an array element'  => "\x41" . $re_body,
    'bare REGEXP as a hash value'      => "\x51\x61k" . $re_body,
);

my $dec = Sereal::Decoder->new;
for my $version ( 1 .. 6 ) {
    my $got = $dec->decode( Header($version) . $bare{'bare REGEXP as the document body'} );
    ok( "xABCx" =~ $got, "v$version bare REGEXP body matches" );
    $got = $dec->decode( Header($version) . $bare{'bare REGEXP as an array element'} );
    ok( "xABCx" =~ $got->[0], "v$version bare REGEXP array element matches" );
    $got = $dec->decode( Header($version) . $bare{'bare REGEXP as a hash value'} );
    ok( "xABCx" =~ $got->{k}, "v$version bare REGEXP hash value matches" );
}

for my $version ( 3, 6 ) {
    my $prelude = qq{ my \$dec = Sereal::Decoder->new; my \$H = Header($version); };
    child_survives(
        $prelude . q{
            my $out; $dec->decode( $H . "\x31\x63abc\x61i", $out );
            die "no match" unless "ABC" =~ $out;
            $out = 1.5; my $c = $out; die "bad value" unless $c == 1.5;
            $dec->decode( $H . "\x31\x63abc\x61i", $out ); $dec->decode( $H . "\x05", $out );
            die "bad reuse" unless $out == 5;
        },
        "v$version assigning to a decoded bare REGEXP output scalar"
    );
    child_survives(
        $prelude . q{
            my $r = $dec->decode( $H . "\x42\x31\x63abc\x61i\x31\x63abc\x61i" );
            $r->[0] = 1; $_ .= "z" for @$r; die "bad values" unless "@$r" eq "1z (?^i:abc)z";
        },
        "v$version assigning to decoded bare REGEXP array elements"
    );
    child_survives(
        $prelude . q{
            my $h = $dec->decode( $H . "\x51\x61k\x31\x63abc\x61i" );
            $h->{k} = 1; my $c = $h->{k}; die "bad value" unless $c == 1;
        },
        "v$version assigning to a decoded bare REGEXP hash value"
    );
    my $blob_code = 'my $blob = pack "H*", "'
        . unpack( 'H*', read_protocol_corpus( $version, 'regexp_object' ) ) . '";';
    child_survives(
        $blob_code . q{
            my $r = Sereal::Decoder->new->decode($blob);
            my $c = ${ $r->{re} };
            ${ $r->{re} } = 1;
            die "no match" unless "xabbbc" =~ $c;
            undef $c; undef $r;
        },
        "v$version copy of a decoded qr// outlives the reference"
    );
    child_survives(
        $prelude . q{
            my ( $out, @copies );
            for ( 1 .. 50 ) { $dec->decode( $H . "\x31\x63abc\x61i", $out ); push @copies, $out; }
            for (@copies) { die "no match" unless "xabcx" =~ $_ }
            my $blob = $H . "\x31\x64ab+c\x61i";
            my @refs;
            for ( 1 .. 50 ) { $dec->decode( $blob, $out ); push @refs, $out; }
            for (@refs) { die "no match" unless "xABBc" =~ $_ }
        },
        "v$version decoding regexps repeatedly into one output scalar"
    );
}

# The pattern must be a string, not a reference to an earlier REGEXP.
for my $hex (
    '3df3726c0300282b03b161616030290431290460',    # [REGEXP|T "a" "", WEAKEN REFP(4), REGEXP REFP(4) ""]
    '3df3726c0600282b02b161616031290460',          # [REGEXP|T "a" "i", REGEXP REFP(4) ""]
    )
{
    my $ok = eval { $dec->decode( pack 'H*', $hex ); 1 };
    ok( !$ok, "$hex: REFP as a REGEXP pattern is rejected" );
    like( $@, qr/REGEXP pattern must be a string/, "$hex: with a clear error" );
}

# Round trips keep the class and the modifiers.
# Protocol 3 and 6 packets are checked in so these Decoder cases do not need
# an installed Encoder to create their inputs.
for my $version ( 3, 6 ) {
    my $r = $dec->decode( read_protocol_corpus( $version, 'regexp_object' ) );
    is( ref( $r->{re} ), 'Regexp', "v$version qr// is a Regexp" );
    is_deeply(
        [ regexp_pattern( $r->{re} ) ], [ 'ab+c', 'i' ],
        "v$version qr// keeps pattern and flags"
    );
    is( ref( $r->{obj} ), 'Foo', "v$version blessed regexp keeps its class" );
    ok( 'x' =~ $r->{obj}, "v$version blessed regexp matches" );
    $r = $dec->decode( read_protocol_corpus( $version, 'regexp_shared' ) );
    is( $r->[0], $r->[1], "v$version a regexp referenced twice is one value" );
    ok( 'q' =~ $r->[1], "v$version and matches through the REFP" );
    $r = $dec->decode( read_protocol_corpus( $version, 'regexp_copy' ) );
    ok( 'ABC' =~ $r->[0], "v$version a copied regexp round-trips" );
}

# A scalar holding a perl regexp copy is a valid output scalar.
{
    my $x = ${qr/abc/i};
    $dec->decode( Header(6) . "\x63abc", $x );
    is( $x, 'abc', 'decode into a scalar holding a regexp copy' );
}

# Output scalars that are not plain scalars are refused rather than written
# through.
{
    no warnings 'once';
    my @warnings;
    local $SIG{__WARN__} = sub { push @warnings, @_ };
    my $ok = eval { $dec->decode( Header(6) . "\x05", *main::some_glob ); 1 };
    ok( !$ok, 'a glob as the output scalar is refused' );
    like( $@, qr/cannot decode into a GLOB/, 'with a clear error' );
    is_deeply( \@warnings, [], 'refusing a glob does not warn' );
}

done_testing;

sub read_protocol_corpus {
    my ( $version, $name ) = @_;
    return read_packet(
        File::Spec->catdir( 't', 'data', 'packet_corpus' ),
        'decoder-regressions', "v$version/$name"
    );
}
