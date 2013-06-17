#!perl
use strict;
use warnings;
use Test::More;
use Sereal::Decoder;
use File::Spec;

my $file = File::Spec->catfile(qw(data for_901_regr_freeing_t.srl));
$file = File::Spec->catdir('t', $file) if -d 't';

open my $fh, "<", $file or die $!;
binmode $fh;
my $data = do { local $/; <$fh> };
close $fh;

SCOPE: {
    my $d = Sereal::Decoder->new;

    eval { my $out = $d->decode($data); }
}

pass("Alive");

done_testing();

