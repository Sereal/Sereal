#!perl
use strict;
use warnings;
use Data::Dumper;
use File::Spec;

use lib File::Spec->catdir(qw(t lib));
BEGIN {
    lib->import('lib')
        if !-d 't';
}

use Sereal::TestSet qw(:all);
use Test::More;

my $ok = have_encoder_and_decoder();
if (not $ok) {
    plan skip_all => 'Did not find right version of encoder';
}
else {
    my $e = Sereal::Encoder->new();
    my $d = Sereal::Decoder->new();

    my $out;
    my $payload = [ 'abcd' x 1024 ];
    my $ok = eval {$out = $e->encode($payload); 1};
    my $err = $@ || 'Zombie error';
    ok($ok, "snappy_incr and warn_unknown makes CODE encoding not fail");

    my $decoded;
    $ok = eval {$decoded = $d->decode($out); 1};
    $err = $@ || 'Zombie error';
    ok($ok, "snappy_incr and warn_unknown produced decodable output")
    or do {
        diag($err);
        hobodecode($out) if $ENV{DEBUG_SEREAL};
    };

    is_deeply($decoded, $payload, 'results matches');
}


pass();
done_testing();

