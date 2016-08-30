#!perl
use strict;
use warnings;
use File::Spec;
use lib File::Spec->catdir(qw(t lib));
BEGIN {
    lib->import('lib')
        if !-d 't';
}
use Sereal::TestSet;
use Sereal::Encoder qw(encode_sereal);
use Test::More tests => 2;

{
    my $v = [{}];
    my $v_sereal = encode_sereal($v);
    my $v2 = [@$v];
    my $v_new_sereal = encode_sereal($v);
    cmp_ok($v_sereal, 'ne', $v_new_sereal, "Without canonical_refs we're sensitive to refcount changes");
}

{
    my $v = [{}];
    my $v_sereal = encode_sereal($v, {canonical_refs => 1});
    my $v2 = [@$v];
    my $v_new_sereal = encode_sereal($v, {canonical_refs => 1});
    cmp_ok($v_sereal, 'eq', $v_new_sereal, "With canonical_refs we're not sensitive to refcount changes");
}
