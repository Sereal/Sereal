use warnings;
use strict;

package NewStdHash;
require Tie::Hash;
our @ISA = qw(Tie::StdHash);

package main;
use Test::More;
use File::Spec;
use lib File::Spec->catdir(qw(t lib));
BEGIN {
    lib->import('lib')
        if !-d 't';
}
use Sereal::TestSet qw(:all);
my $u_df= "\xDFu";
utf8::upgrade($u_df);
my @keys= (
    'foo', 'bar', 'mip', 'xap', 'food', 'fool', 'fools', 'barking', 'bark',
    $u_df,
    "\x{df}a",
    "\x{c3}",
    "\x{de}",
    "\x{e0}",
    "\x{100}",
    "\x{123}",
    "\x{c4}\x{80}",
);
my $have_decoder= have_encoder_and_decoder();
if ($have_decoder) {
    plan tests => 1 + (4 * @keys);
} else {
    plan tests => 1;
}

my $enc = Sereal::Encoder->new({
    sort_keys => 1,
});

tie my %new_std_hash, 'NewStdHash';
my %normal_hash;
foreach my $i (0..$#keys) {
    $new_std_hash{$keys[$i]} = $i;
    $normal_hash{$keys[$i]}= $i;
}

my $enc_tied  = $enc->encode(\%new_std_hash);
my $enc_normal= $enc->encode(\%normal_hash);


is($enc_tied, $enc_normal, "Tied and untied are the same")
or do {
    diag "Normal:\n";
    hobodecode $enc_normal;
    diag "Tied: \n";
    hobodecode $enc_tied;
};

if ($have_decoder) {
    my $dec= Sereal::Decoder->new();
    my $dec_tied= $dec->decode($enc_tied);
    my $dec_normal= $dec->decode($enc_normal);
    foreach my $i (0..$#keys) {
        is($dec_tied->{$keys[$i]},$i, "decoded tied");
        is($dec_normal->{$keys[$i]},$i, "decoded normal");
        is($new_std_hash{$keys[$i]},$i, "original tied");
        is($normal_hash{$keys[$i]},$i, "original normal");
    }
}

