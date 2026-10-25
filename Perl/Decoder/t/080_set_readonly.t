use strict;
use warnings;

use Test::More;
use File::Spec;
use lib File::Spec->catdir(qw(t lib));

BEGIN {
    lib->import('lib')
        if !-d 't';
}
use Sereal::Decoder;
use Scalar::Util qw(reftype);
use Sereal::TestSet::Corpus qw(read_packet);

my @tests = (
    [ set_readonly => 1 ],
);

my $packet = read_packet(
    File::Spec->catdir( 't', 'data', 'packet_corpus' ),
    'decoder-features', 'set_readonly'
);
my $dec  = Sereal::Decoder->new( { set_readonly         => 1 } );
my $dec2 = Sereal::Decoder->new( { set_readonly_scalars => 1 } );
my $got  = $dec->decode($packet);
my $got2 = $dec2->decode($packet);
undef $dec;
undef $dec2;

for my $name ( sort keys %$got ) {
    _recurse( $got->{$name},  '', $name, 0 );
    _recurse( $got2->{$name}, '', $name, 1 );
}
done_testing();

sub _recurse {
    my ( $s, $path, $name, $scalars_only ) = @_;

    $scalars_only ||= 0;
    my $should_be_readonly = $scalars_only ? !ref($s) : 1;
    is(
        Internals::SvREADONLY( $_[0] ), $should_be_readonly,
        "scalar_only: '$scalars_only'. We want ro: '$should_be_readonly'. struct: $name, path: $path"
    );

    my $reftype = reftype( $_[0] )
        or return;

    if ( length($path) ) {
        is(
            &Internals::SvREADONLY( $_[0] ), $should_be_readonly,
            "scalar_only: '$scalars_only'. We want ro: '$should_be_readonly'. struct: $name, path: $path"
        );
    }
    if ( $reftype eq 'ARRAY' ) {
        my $i = 0;
        foreach (@$s) {
            _recurse( $_, $path . '->[' . $i . ']', $name, $scalars_only );
        }
    }
    elsif ( $reftype eq 'HASH' ) {
        foreach ( keys %$s ) {
            next if reftype( $s->{$_} ) && $s->{$_} == $s;
            _recurse( $s->{$_}, $path . '->{' . $_ . '}', $name, $scalars_only );
        }
    }
    elsif ( $reftype eq 'SCALAR' ) {
        _recurse( $$s, '${' . $path . '}', $name, $scalars_only );
    }
    else {
        die "unknown ref type '$reftype'";
    }
}

