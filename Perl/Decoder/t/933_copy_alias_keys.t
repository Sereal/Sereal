#!perl
use strict;
use warnings;
use File::Spec;
use Scalar::Util qw(refaddr);
use Test::More;

use lib File::Spec->catdir(qw(t lib));
BEGIN { lib->import('lib') if !-d 't' }
use Sereal::TestSet qw(Header short_string varint);
use Sereal::Decoder;
use Sereal::Decoder::Constants qw(:all);

for my $version ( 1 .. 6 ) {
    my $header  = Header($version);
    my $base    = $version == 1 ? length($header) : 1;
    my $decoder = Sereal::Decoder->new;

    # Re-reading a tracked source through COPY must leave aliases pointing
    # at the original value, not at the newly created copy.
    my $source_offset = $base + 1;
    my $values =
        $decoder->decode( $header
            . chr( SRL_HDR_ARRAYREF + 3 )
            . short_string( 'bbbb', 1 )
            . chr(SRL_HDR_COPY)
            . varint($source_offset)
            . chr(SRL_HDR_ALIAS)
            . varint($source_offset) );
    is(
        refaddr( \$values->[0] ), refaddr( \$values->[2] ),
        "v$version: ALIAS still targets the original value after COPY"
    );
    isnt(
        refaddr( \$values->[0] ), refaddr( \$values->[1] ),
        "v$version: COPY creates an independent scalar"
    );
    $values->[1] = 'changed';
    is( $values->[0], 'bbbb', "v$version: changing COPY leaves the original value alone" );

    my $key_offset  = $base + 1;
    my $copy_offset = $key_offset + length( short_string('bbbb') );
    my $hash_prefix = $header . chr( SRL_HDR_HASHREF + 2 ) . short_string( 'bbbb', 1 );

    # The tracking bit on a key is ignored. Only the copied value's offset
    # becomes an alias target when COPY itself carries the tracking bit.
    my $hash =
        $decoder->decode( $hash_prefix
            . chr( SRL_HDR_COPY | SRL_HDR_TRACK_FLAG )
            . varint($key_offset)
            . short_string('cccc')
            . chr(SRL_HDR_ALIAS)
            . varint($copy_offset) );
    is_deeply(
        $hash, { bbbb => 'bbbb', cccc => 'bbbb' },
        "v$version: key COPY followed by value ALIAS decodes"
    );
    is(
        refaddr( \$hash->{bbbb} ), refaddr( \$hash->{cccc} ),
        "v$version: tracked COPY is an aliasable value"
    );

    for my $case ( [
            'ALIAS as key',
            $header
                . chr( SRL_HDR_ARRAYREF + 2 )
                . short_string( 'bbbb', 1 )
                . chr( SRL_HDR_HASHREF + 1 )
                . chr(SRL_HDR_ALIAS)
                . varint($source_offset)
                . chr(1),
            qr/Unexpected tag SRL_HDR_ALIAS/
        ],
        [
            'ALIAS to key',
            $header
                . chr( SRL_HDR_ARRAYREF + 2 )
                . chr( SRL_HDR_HASHREF + 1 )
                . short_string( 'bbbb', 1 )
                . chr(1)
                . chr(SRL_HDR_ALIAS)
                . varint( $base + 2 ),
            qr/ALIAS\(\d+\) references an unknown item/
        ],
        [
            'ALIAS to key after COPY',
            $hash_prefix
                . chr(SRL_HDR_COPY)
                . varint($key_offset)
                . short_string('cccc')
                . chr(SRL_HDR_ALIAS)
                . varint($key_offset),
            qr/ALIAS\(\d+\) references an unknown item/
        ],
        )
    {
        my ( $name, $packet, $error ) = @$case;
        eval { $decoder->decode($packet); 1 };
        like( $@, $error, "v$version: reject $name" );
        is(
            $decoder->decode( $header . chr(7) ), 7,
            "v$version: decoder remains usable after $name"
        );
    }
}

done_testing();
