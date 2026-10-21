#!perl
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
use Test::Warn;

# Regression test: a decoder reused after a failed decode of a document
# containing a FREEZE'd object.
#
# srl_read_frozen_object() pushes the item onto dec->thaw_av and records the
# address of its argument AV in dec->ref_thawhash. Both are consumed later, by
# srl_finalize_structure(). If the decode dies in between -- e.g. on a truncated
# document -- finalize never runs, so those entries survive.
#
# srl_clear_decoder_body_state() used to reset ref_seenhash, ref_stashes and
# ref_bless_av but not thaw_av or ref_thawhash, so the leftovers were still there
# for the next document. The next decode that needed finalizing would then walk
# the previous document's thaw_av entries and look up ref_thawhash keys that point
# at long-freed AVs, which segfaults or silently returns the wrong class.
#
# Before the fix this file dies with SIGSEGV rather than failing an assertion,
# because perl cannot trap that.

# Documents are hardcoded, as in t/901_regr_segv.t, so this test does not need
# Sereal::Encoder. Both were produced with freeze_callbacks enabled:
#   frozen: { a => bless(\"secret", "Frz") }
#   good:   { b => bless(\"other",  "Frz"), c => "plain" }
my $frozen_doc = "=\xf3rl\x05\x00Qaa2cFrz\x28\x2b\x01fsecret";
my $good_doc   = "=\xf3rl\x05\x00Raceplainab2cFrz\x28\x2b\x01eother";

{

    package Frz;

    sub THAW {
        my ( $class, $serializer, $data ) = @_;
        return bless \$data, 'Thawed';
    }
}

sub check_good {
    my ( $out, $label ) = @_;
    return 0 unless ok( ref($out) eq 'HASH', "$label: decoded a hash" );
    my $ok = 1;
    $ok &&= is( ref( $out->{b} ),      'Thawed', "$label: frozen value was thawed" );
    $ok &&= is( ${ $out->{b} // \'' }, 'other',  "$label: thawed value is intact" );
    $ok &&= is( $out->{c},             'plain',  "$label: sibling value is intact" );
    return $ok;
}

# Control: the good document on its own decodes correctly.
check_good( Sereal::Decoder->new->decode($good_doc), "virgin decoder" );

# Every truncation that makes the decode fail must leave the decoder clean enough
# to handle the next document. Sweeping rather than picking one offset keeps this
# robust if the document layout ever changes.
my $swept = 0;
for my $cut ( 6 .. length($frozen_doc) - 1 ) {
    my $decoder = Sereal::Decoder->new;

    my $died = !eval { $decoder->decode( substr( $frozen_doc, 0, $cut ) ); 1 };
    next unless $died;    # only interested in cuts that actually fail
    $swept++;

    my $out;
    warnings_are {
        eval { $out = $decoder->decode($good_doc); 1 }
            or diag("reuse after a failed decode threw: $@");
    }
    [], "cut=$cut: reusing the decoder produces no warnings";

    check_good( $out, "cut=$cut" );
}

ok( $swept > 0, "at least one truncation failed to decode (swept $swept)" );

pass("Alive");

done_testing();
