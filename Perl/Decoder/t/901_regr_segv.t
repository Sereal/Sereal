#!perl
use strict;
use warnings;
use Test::More;
use Sereal::Decoder;
use Test::Warn;

SCOPE: {
    my $d = Sereal::Decoder->new;
    warnings_are { eval {$d->decode("=srl\1\0\321j\3\3\3\3\3\3\3\3\3\3.\1")} } [], "no warnings";
    warnings_are { eval {$d->decode("=srl\1\0\254/\6")} } [], "no warnings";
}

pass("Alive");

done_testing();
