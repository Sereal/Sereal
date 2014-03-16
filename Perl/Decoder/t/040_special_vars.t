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
        \undef(),
        \do { my $z = undef },
        \!1,
        \do { my $z = !1 },
        \!0,
        \do { my $z = !0 },
) {
    is( desc_special($dec->decode($enc->encode($_))), desc_special($_) );
}
