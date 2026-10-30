#!perl
use strict;
use warnings;
use File::Spec;
use Scalar::Util qw(refaddr);
use Test::More;

use lib File::Spec->catdir(qw(t lib));
BEGIN { lib->import('lib') if !-d 't' }
use Sereal::TestSet qw(have_encoder_and_decoder Header short_string varint);
use Sereal::Encoder;
use Sereal::Encoder::Constants qw(:all);

if ( !have_encoder_and_decoder() ) {
    plan skip_all => 'Did not find right version of decoder';
    exit 0;
}

{

    package SortedKeyHash;
    require Tie::Hash;
    our @ISA = qw(Tie::StdHash);
}

{

    package NumericKeyHash;
    sub TIEHASH  { bless {}, shift }
    sub FIRSTKEY { 123 }
    sub NEXTKEY  { undef }
    sub FETCH    { 456 }
}

for my $version ( 1 .. 6 ) {
    # Anonymous hash refcounts vary between Perls. Use the canonical
    # reference form so this checks COPY/ALIAS offsets independently.
    my $hash_header = chr(SRL_HDR_REFN) . chr(SRL_HDR_HASH) . varint(2);
    my $base_offset  = $version == 1 ? length( Header($version) ) : 1;
    my $key_offset   = $base_offset + length($hash_header);
    my $value_offset = $key_offset + length( short_string('bbbb') );
    for my $alias ( 0, 1 ) {
        my $option = $alias ? 'aliased_dedupe_strings' : 'dedupe_strings';
        my $packet = Sereal::Encoder->new( {
                protocol_version => $version,
                canonical_refs   => 1,
                sort_keys        => 2,
                $option          => 1,
            } )->encode( { bbbb => 'bbbb', cccc => 'bbbb' } );
        my $expected =
              Header($version)
            . $hash_header
            . short_string('bbbb')
            . chr( SRL_HDR_COPY | ( $alias ? SRL_HDR_TRACK_FLAG : 0 ) )
            . varint($key_offset)
            . short_string('cccc')
            . chr( $alias    ? SRL_HDR_ALIAS : SRL_HDR_COPY )
            . varint( $alias ? $value_offset : $key_offset );
        is(
            $packet, $expected,
            "v$version $option: COPY the key, then reuse the value without a COPY chain"
        );
        my $decoded = Sereal::Decoder->new->decode($packet);
        is_deeply(
            $decoded, { bbbb => 'bbbb', cccc => 'bbbb' },
            "v$version $option: value copied from key decodes"
        );
        if ($alias) {
            is(
                refaddr( \$decoded->{bbbb} ), refaddr( \$decoded->{cccc} ),
                "v$version: subsequent value aliases the SV created by COPY"
            );
        }
    }

    for my $sort ( 1 .. 3 ) {
        for my $tied ( 0, 1 ) {
            my $label = "v$version sort_keys=$sort tied=$tied";
            my %data;
            tie %data, 'SortedKeyHash' if $tied;

            # All values equal a key. Ascending and descending sorts exercise
            # both key-to-value and value-to-key deduplication attempts.
            %data = ( aaaa => 'bbbb', bbbb => 'bbbb', cccc => 'bbbb' );
            my $packet = Sereal::Encoder->new( {
                    protocol_version       => $version,
                    sort_keys              => $sort,
                    aliased_dedupe_strings => 1,
                } )->encode( \%data );
            my $decoded = eval { Sereal::Decoder->new->decode($packet) };
            my $error   = $@;
            ok( !$error, "$label: keys and aliased values decode" )
                or diag($error);
            SKIP: {
                skip 'decode failed', 3 if $error;
                is_deeply(
                    $decoded, { aaaa => 'bbbb', bbbb => 'bbbb', cccc => 'bbbb' },
                    "$label: key and value strings survive"
                );
                is(
                    refaddr( \$decoded->{aaaa} ), refaddr( \$decoded->{bbbb} ),
                    "$label: first two values still alias"
                );
                is(
                    refaddr( \$decoded->{bbbb} ), refaddr( \$decoded->{cccc} ),
                    "$label: last two values still alias"
                );
            }
        }

        tie my %numeric, 'NumericKeyHash';
        my $packet = Sereal::Encoder->new( {
                protocol_version => $version,
                sort_keys        => $sort,
            } )->encode( \%numeric );
        my $decoded = eval { Sereal::Decoder->new->decode($packet) };
        my $error   = $@;
        ok( !$error, "v$version sort_keys=$sort: numeric tied key decodes" )
            or diag($error);
        SKIP: {
            skip 'decode failed', 1 if $error;
            is_deeply(
                $decoded, { 123 => 456 },
                "v$version sort_keys=$sort: numeric key is a string"
            );
        }
    }
}

# Preserve the string's UTF-8 flag while bypassing scalar encoding for keys.
for my $sort ( 1 .. 3 ) {
    my %data   = ( '' => 1, "\x{100}wide" => 2, '123' => 3 );
    my $packet = Sereal::Encoder->new( { sort_keys => $sort } )->encode( \%data );
    is_deeply(
        Sereal::Decoder->new->decode($packet), \%data,
        "sort_keys=$sort: empty, Unicode, and numeric-looking keys survive"
    );
}

for my $tied ( 0, 1 ) {
    for my $sort ( 0 .. 3 ) {
        for my $no_shared ( 0, 1 ) {
            my %data;
            tie %data, 'SortedKeyHash' if $tied;
            %data = ( "\x{100}wide" => "\x{100}wide", zzzz => "\x{100}wide" );
            my $encoder = Sereal::Encoder->new( {
                sort_keys              => $sort,
                no_shared_hashkeys     => $no_shared,
                aliased_dedupe_strings => 1,
            } );
            for ( 1 .. 2 ) {
                my $decoded = Sereal::Decoder->new->decode( $encoder->encode( \%data ) );
                is_deeply(
                    $decoded, \%data,
                    "Unicode keys/values: tied=$tied sort=$sort no_shared=$no_shared reuse=$_"
                );
                is(
                    refaddr( \$decoded->{"\x{100}wide"} ), refaddr( \$decoded->{zzzz} ),
                    'Unicode values alias each other'
                );
            }
        }
    }
}

for my $version ( 2 .. 6 ) {
    my $encoder = Sereal::Encoder->new( {
        protocol_version       => $version,
        sort_keys              => 2,
        aliased_dedupe_strings => 1,
    } );
    my $packet =
        $encoder->encode( { bbbb => 'bbbb', cccc => 'bbbb' }, { bbbb => 'bbbb', cccc => 'bbbb' } );
    my $decoder = Sereal::Decoder->new;
    is_deeply(
        $decoder->decode_only_header($packet), { bbbb => 'bbbb', cccc => 'bbbb' },
        "v$version: key/value string sharing works in user header"
    );
    is_deeply(
        $decoder->decode($packet), { bbbb => 'bbbb', cccc => 'bbbb' },
        "v$version: body alias targets are independent of header targets"
    );
}

done_testing();
