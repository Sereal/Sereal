#!perl
use strict;
use warnings;
use Test::More;
use Sereal::Decoder;

for my $version ( 1 .. 6 ) {
    my $header = ( $version < 3 ? '=srl' : "=\xf3rl" ) . chr($version) . "\0";
    for my $options (
        {}, { use_undef => 1 }, { set_readonly => 1 },
        { set_readonly_scalars => 1 } )
    {
        my $dec    = Sereal::Decoder->new($options);
        my $label  = "v$version " . ( join( ',', sort keys %$options ) || 'default' );
        my @values = ( "\x25", "\xa5" );
        push @values, "\x39", "\xb9" if $version >= 3;
        for my $value (@values) {
            for my $key ( "\x61x", "\x26\x01x", "\x27\x01x" ) {
                eval { $dec->decode( $header . "\x52" . $key . $value . $key . "\x01" ) };
                like(
                    $@, qr/duplicate key 'x' in hash/,
                    "$label rejects repeated key after " . unpack( 'H*', $value . $key ) );
            }
        }
        is_deeply(
            $dec->decode( $header . "\x52\x61x\x25\x61y\x01" ),
            { x => undef, y => 1 }, "$label distinct keys still work after rejection"
        );
    }
}
done_testing;
