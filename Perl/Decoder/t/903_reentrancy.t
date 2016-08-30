use strict;
use warnings;
use File::Spec;
use lib File::Spec->catdir(qw(t lib));
BEGIN {
    lib->import('lib')
        if !-d 't';
}

use Sereal::TestSet qw(:all);
use Test::More tests => 2;
use Sereal::Decoder;

# Regression test for RT #93563

# Decoder is (was) not re-entrant.

my $dec;
package Foo;
sub FREEZE { my $x = Sereal::Encoder->new->encode($_[0]->{a}); return $x; }
sub THAW { bless({a => $dec->decode($_[2])}, $_[0]) }

package main;

SKIP: {
    my $have_enc = have_encoder_and_decoder();
    if (not $have_enc) {
        skip "Need encoder for Snappy regression tests", 2;
    }
    else {
        $dec = Sereal::Decoder->new;
        my $z = [ bless({a=>42},"Foo") ];
        push @$z, $z;
        my $a = Sereal::Encoder->new({freeze_callbacks=>1})->encode($z);
        my $b;
        my $err;
        eval {
            $b = $dec->decode($a);
            1
        } or do {
            $err = $@ || 'Zombie error';
        };
        ok(!$err, "Decoding did not barf")
            or diag("Decoding barfed with '$err'");
        is_deeply($b, $z, "Output from decoding is correct");
    }
}
