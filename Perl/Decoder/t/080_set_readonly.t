use strict;
use warnings;

use Test::More;
use File::Spec;
use lib File::Spec->catdir(qw(t lib));
BEGIN {
    lib->import('lib')
        if !-d 't';
}
use Sereal::TestSet qw(:all);
use Sereal::Decoder;
use Scalar::Util qw(reftype weaken);

my @tests= (
    [ set_readonly => 1  ],
);

if (have_encoder_and_decoder()) {
    my $num_tests= 62;
    plan tests => $num_tests;
} else {
    plan skip_all => 'Did not find right version of encoder';
}

my $foo = bless([ 1, 2, 3 ],"foo");

my $weak_blessed_href = bless({ blah => 'bat', hash => { t => 1 } }, 'SomeClass');
weaken($weak_blessed_href->{foo} = $weak_blessed_href);
my $struct= {
    hashref => { a => [ "b", 5, bless({ foo => "bar"}, "SomeClass")] },
    blessed_ref_with_refs => bless({ foo => { bar => 'test' }, bar => ['baz'], empty_href => {}, empty_aref => [] }, 'Blah'),
    string => "foobar",
    arrayref => [ "foobar" ],
    blessed_arrayref => $foo,
    weak_blessed_href => $weak_blessed_href,
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
    is(Internals::SvREADONLY( $_[0] ), $should_be_readonly,
       "scalar_only: '$scalars_only'. We want ro: '$should_be_readonly'. struct: $name, path: $path"
      );

    my $reftype = reftype($_[0])
        or return;

    if ( length($path) ) {
        is(&Internals::SvREADONLY( $_[0] ), $should_be_readonly,
            "scalar_only: '$scalars_only'. We want ro: '$should_be_readonly'. struct: $name, path: $path"
        );
    }
    if ($reftype eq 'ARRAY') {
        my $i = 0;
        foreach (@$s) {
            _recurse($_, $path . '->[' . $i . ']', $name, $scalars_only);
        }
    }
    elsif ($reftype eq 'HASH') {
        foreach (keys %$s) {
            next if reftype($s->{$_}) && $s->{$_} == $s;
            _recurse($s->{$_}, $path . '->{' . $_ . '}', $name, $scalars_only);
        }
    } elsif ($reftype eq 'SCALAR') {
        _recurse($$s, '${' . $path . '}', $name, $scalars_only);
    } else {
        die "unknown ref type '$reftype'";
    }
}


