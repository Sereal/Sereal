#!perl
use strict;
use warnings;

use File::Spec;
use Scalar::Util qw( blessed );
use Test::More;

use lib File::Spec->catdir(qw(t lib));
BEGIN {
    lib->import('lib')
        if !-d 't';
}

my $ok = eval "
    use Sereal::Encoder;
    use Sereal::Decoder 0.34;
    1;
";
if (not $ok) {
    plan skip_all => 'Did not find right version of encoder: '.$@;
}
else {
    my $class = 'MyFoo';
    my %hash = ( x => 1 );
    my $object = bless( \%hash, $class );
    my $enc = Sereal::Encoder->new();
    my $blob = $enc->encode( $object );

    # do not bless anything
    {
        my $dec = Sereal::Decoder->new({ nobless_objects => 1 });
        my $data = $dec->decode( $blob );

        ok( ref( $data ) && !blessed( $data ), 'reference without class' );
        is_deeply( $data, \%hash, 'same structure' );
    }

    # normally do the blessing
    {
        my $dec = Sereal::Decoder->new();
        my $data = $dec->decode( $blob );

        is_deeply( $data, $object, 'same structure' );
        isa_ok( $data, $class, 'same class' );
    }
}

done_testing();

