use strict;
use warnings;
use Test::More;
use File::Spec;
use Sereal::Decoder;
use lib File::Spec->catdir(qw(t lib));

BEGIN {
    lib->import('lib')
        if !-d 't';
}
use Sereal::TestSet::Corpus qw(read_packet);
plan tests => 6;
my $dec = Sereal::Decoder->new;

sub desc_special($) {
    return
          $_[0] == \undef() ? "undef"
        : $_[0] == \!1      ? "false"
        : $_[0] == \!0      ? "true"
        : !defined( $_[0] ) ? "undef"
        : length( $_[0] )   ? "not-special"
        : do {
        my @warn;
        local $SIG{__WARN__} = sub { push @warn, $_[0] };
        my $i = int( $_[0] );
        @warn ? "not-special" : "false";
        };
}

foreach (
    [ "ref undef",     \undef(),          'special/ref_undef' ],
    [ "ref undef var", \do { my $z = undef }, 'special/ref_undef_var' ],
    [ "ref false",     \!1,                'special/ref_false' ],
    [ "ref false var", \do { my $z = !1 },  'special/ref_false_var' ],
    [ "ref true",      \!0,                'special/ref_true' ],
    [ "ref true var ", \do { my $z = !0 },  'special/ref_true_var' ],
    )
{
    my ( $name, $var, $corpus_name ) = @$_;
    my $want = desc_special($var);
    my $packet = read_packet(
        File::Spec->catdir( 't', 'data', 'packet_corpus' ),
        'decoder-features', $corpus_name
    );
    is( desc_special( $dec->decode($packet) ), $want, $name . " ($want)" );
}
