use strict;
use warnings;

use Sereal::Decoder;
use Test::More;
use File::Spec;
use lib File::Spec->catdir(qw(t lib));
BEGIN {
    lib->import('lib')
        if !-d 't';
}
use Sereal::TestSet qw(:all);

my @tests= (
    [ set_readonly => 1  ],
);

if (have_encoder_and_decoder()) {
    my $num_tests= 26;
    plan tests => $num_tests;
} else {
    plan skip_all => 'Did not find right version of encoder';
}

my $foo = bless([ 1, 2, 3 ],"foo");

my $struct= {
    hashref => { a => [ "b", 5, bless({ foo => "bar"}, "SomeClass")] },
    string => "foobar",
    arrayref => [ "foobar" ],
    blessed_arrayref => $foo,
};

foreach my $name ( keys %$struct ) {

    local $_ = $struct->{$name};
    my $enc = Sereal::Encoder->new;
    my $dec = Sereal::Decoder->new( { set_readonly => 1 } );
    my $dec2 = Sereal::Decoder->new( { set_readonly_scalars => 1 } );

    my $got;
    $dec->decode($enc->encode($_), $got);
    my $got2;
    $dec2->decode($enc->encode($_), $got2);

    # undef the decoder to make sure if it blows up on DESTROY it does it before we test.
    undef $dec;
    undef $dec2;
    undef $enc;

    _recurse($got, '', $name, 0);
    _recurse($got2, '', $name, 1);

}

sub _recurse {
    my ($s, $path, $name, $scalars_only) = @_;

    $scalars_only ||= 0;
    my $should_be_readonly = $scalars_only ? !ref($s) : 1;
    is(Internals::SvREADONLY($_[0]), $should_be_readonly,
       "scalar_only: '$scalars_only'. We want ro: '$should_be_readonly'. struct: $name, path: $path"
      );

    my $ref = ref $s
      or return;

    if ($ref eq 'ARRAY' || $ref eq 'foo') { 
        my $i = 0;
        foreach (@$s) {
            _recurse($_, $path . '->[' . $i . ']', $name, $scalars_only);
        }
    }
    elsif ($ref eq 'HASH' || $ref eq 'SomeClass') {
        foreach (keys %$s) {
            _recurse($s->{$_}, $path . '->{' . $_ . '}', $name, $scalars_only);
        }
    } elsif ($ref eq 'SCALAR') {
        _recurse($$s, '${' . $path . '}', $name, $scalars_only);
    } else {
        die "unknown ref value '$ref'";
    }
}


