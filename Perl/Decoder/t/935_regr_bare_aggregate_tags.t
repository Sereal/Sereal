#!perl
use strict;
use warnings;
use File::Spec;
use lib File::Spec->catdir(qw(t lib));

BEGIN { lib->import('lib') if !-d 't' }

use Test::More;
use Sereal::TestSet qw(Header varint TRACK_FLAG);
use Sereal::Decoder;

# An ARRAY or HASH tag describes the aggregate itself, not a reference to
# it, so in Perl it is only meaningful as the referent of a REFN. Anywhere
# else the decoder used to upgrade the destination scalar into an AV/HV
# body in place, which is not a valid Perl value: copying it croaks with
# "Bizarre copy" and reusing the scalar for a later decode corrupts memory.

my $dec = Sereal::Decoder->new;

my $ARRAY     = chr(0x2b);
my $HASH      = chr(0x2a);
my $REFN      = chr(0x28);
my $COPY      = chr(0x2f);
my $PAD       = chr(0x3f);
my $ARRAYREF  = chr(0x41);          # ARRAYREF with one element
my $ARRAYREF2 = chr(0x42);          # ARRAYREF with two elements
my $HASHREF   = chr(0x51);          # HASHREF with one pair
my $one       = chr(1);
my $key       = chr(0x61) . 'k';    # SHORT_BINARY "k"

sub rejects {
    my ( $body, $name, %arg ) = @_;
    for my $version ( $arg{header} ? 2 : 1 .. 6 ) {    # user data needs a v2+ header
        my $packet =
            Header( $version, $arg{header} ? $body : undef ) . ( $arg{header} ? $one : $body );
        my $ok =
            eval { $arg{header} ? $dec->decode_with_header($packet) : $dec->decode($packet); 1 };
        ok( !$ok, "v$version $name is rejected" );
        like( $@, qr/ARRAY|HASH/, "v$version $name names the aggregate tag" );
        like( $@, qr/REFN/,       "v$version $name explains that REFN is required" );
    }
}

rejects( $ARRAY . varint(1) . $one,                      'bare ARRAY as the document body' );
rejects( $HASH . varint(0),                              'bare HASH as the document body' );
rejects( $ARRAYREF . $ARRAY . varint(0),                 'bare ARRAY as an array element' );
rejects( $HASHREF . $key . $HASH . varint(0),            'bare HASH as a hash value' );
rejects( $REFN . $ARRAY . varint(1) . $HASH . varint(0), 'bare HASH inside a referenced array' );
rejects( $ARRAY . varint(0), 'bare ARRAY as header user data', header => 1 );

# COPY re-parses the bytes at its target. A target that is an ARRAY tag
# (rather than the REFN preceding it) must be rejected the same way.
for my $version ( 2 .. 6 ) {
    my $body   = $REFN . chr( 0x2b | TRACK_FLAG ) . varint(1) . $one;         # body offset 3 is the ARRAY tag
    my $packet = Header($version) . $ARRAYREF2 . $body . $COPY . varint(3);
    my $ok     = eval { $dec->decode($packet); 1 };
    ok( !$ok, "v$version COPY of an ARRAY tag is rejected" );
    like(
        $@, qr/ARRAY.*REFN|REFN.*ARRAY/,
        "v$version COPY of an ARRAY tag explains that REFN is required"
    );
}

# The ordinary encodings keep working, including PAD bytes between the
# REFN and its aggregate.
for my $version ( 1 .. 6 ) {
    is_deeply(
        $dec->decode( Header($version) . $REFN . $ARRAY . varint(1) . $one ), [1],
        "v$version REFN ARRAY decodes"
    );
    is_deeply(
        $dec->decode( Header($version) . $REFN . $HASH . varint(1) . $key . $one ),
        { k => 1 }, "v$version REFN HASH decodes"
    );
    is_deeply(
        $dec->decode( Header($version) . $REFN . $PAD . $PAD . $ARRAY . varint(0) ), [],
        "v$version REFN PAD PAD ARRAY decodes"
    );
    is_deeply(
        $dec->decode( Header($version) . $REFN . chr( 0x2b | TRACK_FLAG ) . varint(0) ), [],
        "v$version REFN tracked ARRAY decodes"
    );
}

# A rejected packet must leave a caller-supplied output scalar usable.
{
    my $out;
    eval { $dec->decode( Header(6) . $ARRAY . varint(0), $out ) };
    $dec->decode( Header(6) . chr(0x63) . 'abc', $out );
    is( $out, 'abc', 'output scalar is a plain string after a rejected bare ARRAY' );
    my @copy = ($out);
    is( $copy[0], 'abc', 'the output scalar can be copied' );
}

done_testing;
