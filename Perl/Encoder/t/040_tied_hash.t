use warnings;
use strict;

package NewStdHash;
require Tie::Hash;
our @ISA = qw(Tie::StdHash);

package main;
use lib 't/lib';
use Sereal::TestSet qw(hobodecode);
use Test::More;
use Sereal::Encoder 3.005 qw(
    sereal_encode_with_object
);
use Sereal::Decoder 3.005 qw(
    decode_sereal
);
use Devel::Peek;

my $enc = Sereal::Encoder->new({
    sort_keys => 1,
});

tie my %new_std_hash, 'NewStdHash';
my %normal_hash;
my @keys= ('foo', 'bar', 'mip', 'xap');
foreach my $i (0..$#keys) {
    $new_std_hash{$keys[$i]} = $i;
    $normal_hash{$keys[$i]}= $i;
}

my $enc_tied  = sereal_encode_with_object($enc, \%new_std_hash);
my $enc_normal= sereal_encode_with_object($enc, \%normal_hash);
diag hobodecode $enc_tied;

TODO:{
    local $::TODO= "Not reliable right now.";
    is($enc_tied, $enc_normal, "Tied and untied are the same");
};

my $dec_tied= decode_sereal($enc_tied);
my $dec_normal= decode_sereal($enc_normal);

foreach my $i (0..$#keys) {
    is($dec_tied->{$keys[$i]},$i, "decoded tied");
    is($dec_normal->{$keys[$i]},$i, "decoded normal");
    is($new_std_hash{$keys[$i]},$i, "original tied");
    is($normal_hash{$keys[$i]},$i, "original normal");
}
done_testing;

