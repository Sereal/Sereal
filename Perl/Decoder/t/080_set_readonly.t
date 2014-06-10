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
    my $num_tests= 11;
    plan tests => $num_tests;
} else {
    plan skip_all => 'Did not find right version of encoder';
}

my $foo = bless([ 1, 2, 3 ],"foo");

my $struct= {
    hash1 => { a => [ "b", 5, bless({ foo => "bar"}, "SomeClass")] },
    string => "foobar",
    blessed_array => $foo,
};

foreach my $name ( keys %$struct ) {

    local $_ = $struct->{$name};
    my $enc = Sereal::Encoder->new;
    my $dec = Sereal::Decoder->new( { set_readonly => 1 } );

    my $got;
    $dec->decode($enc->encode($_), $got);

    # undef the decoder to make sure if it blows up on DESTROY it does it before we test.
    undef $dec;
    undef $enc;

    _recurse($got, '', $name);

}

sub _recurse {
    my ($s, $path, $name) = @_;

    ok(Internals::SvREADONLY($_[0]), "scalar is readonly. struct: $name, path: $path");

    my $ref = ref $s
      or return;

    if ($ref eq 'ARRAY' || $ref eq 'foo') { 
        my $i = 0;
        foreach (@$s) {
            _recurse($_, $path . '->[' . $i . ']', $name);
        }
    }
    elsif ($ref eq 'HASH' || $ref eq 'SomeClass') {
        foreach (keys %$s) {
            _recurse($s->{$_}, $path . '->{' . $_ . '}', $name);
        }
    } elsif ($ref eq 'SCALAR') {
        _recurse($$s, '${' . $path . '}', $name);
    } else {
        die "unknown ref value '$ref'";
    }
}


