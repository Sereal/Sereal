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

if (have_encoder_and_decoder(3.005003)) {
    plan tests => 6;
} else {
    plan skip_all => 'Did not find right version of encoder';
}
my $enc = Sereal::Encoder->new;
my $dec = Sereal::Decoder->new;

sub desc_special($) {
        return $_[0] == \undef() ? "undef" :
                $_[0] == \!1 ? "false" :
                $_[0] == \!0 ? "true" :
                !defined($_[0]) ? "undef" :
                length($_[0]) ? "not-special" :
                do {
                    my @warn;
                    local $SIG{__WARN__}= sub { push @warn,$_[0] };
                    my $i= int($_[0]);
                    @warn ? "not-special" : "false";
                };
}

foreach(
        [ "ref undef",     \undef(),                                            ],
        [ "ref undef var", \do { my $z = undef },                               ],
        [ "ref false",     \!1,                                                 ],
        [ "ref false var", \do { my $z = !1 },                                  ],
        [ "ref true",      \!0,                                                 ],
        [ "ref true var ", \do { my $z = !0 },                                  ],
) {
    my ($name, $var, $todo)= @$_;
    TODO: {
        todo_skip $todo, 1 if $todo;
        is( desc_special($dec->decode($enc->encode($var))), desc_special($var), $name );
    }
}
