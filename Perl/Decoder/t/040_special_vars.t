use strict;
use warnings;

use Sereal::Decoder;
use Test::More;

if (eval "use Sereal::Encoder; 1") {
    plan tests => 6;
} else {
    plan skip_all => "Requires Sereal::Encoder to be installed";
}
my $enc = Sereal::Encoder->new;
my $dec = Sereal::Decoder->new;

sub desc_special($) {
        return $_[0] == \undef() ? "undef" :
                $_[0] == \!1 ? "false" :
                $_[0] == \!0 ? "true" :
                "not-special";
}

foreach(
        [ "ref undef",     \undef(),              "needs new tag in protocol"   ],
        [ "ref undef var", \do { my $z = undef }, "needs new tag in protocol"   ],
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
