#!perl
use strict;
use warnings;
use File::Spec;
use Scalar::Util qw(weaken);
use Test::More;
use lib File::Spec->catdir(qw(t lib));
BEGIN { lib->import('lib') if !-d 't' }
use Sereal::TestSet qw(have_encoder_and_decoder);
use Sereal::Encoder;
plan skip_all => 'decoder unavailable' unless have_encoder_and_decoder();

{
    package SerealCallbackReuseObject;
    sub FREEZE { return $_[0]{value} }
    sub THAW { return bless { value => $_[2] }, $_[0] }
}

for my $version (2 .. 6) {
    for my $sort (0, 1) {
        my $encoder = Sereal::Encoder->new({
            protocol_version => $version,
            freeze_callbacks => 1,
            sort_keys => $sort,
        });
        for my $round (1 .. 12) {
            my $weak;
            {
                my %input;
                for my $key ('plain', "caf\x{e9}", "long-\x{100}-key") {
                    my $object = bless { value => "$key-$round" }, 'SerealCallbackReuseObject';
                    $input{$key} = $object;
                    if ($key eq 'plain') {
                        $weak = $object;
                        weaken($weak);
                    }
                }
                my $blob = $encoder->encode(\%input);
                my $got = Sereal::Decoder->new->decode($blob);
                is_deeply($got, \%input, "v$version sort=$sort round=$round round-trips all keys");
            }
            ok(!defined $weak, "v$version sort=$sort round=$round releases the old objects");
        }
    }
}

{
    my $encoder = Sereal::Encoder->new({
        protocol_version => 6,
        freeze_callbacks => 1,
        sort_keys => 1,
    });
    my @input = map { bless { value => "item-$_" }, 'SerealCallbackReuseObject' } 1 .. 2300;
    my $blob = $encoder->encode(\@input);
    my $got = Sereal::Decoder->new->decode($blob);
    is(scalar @$got, scalar @input, 'large callback table round-trips beyond spare limit');
    is($got->[2299]{value}, 'item-2300', 'last object survives large callback table');
    $blob = $encoder->encode({ again => bless { value => 'small' }, 'SerealCallbackReuseObject' });
    $got = Sereal::Decoder->new->decode($blob);
    is($got->{again}{value}, 'small', 'encoder works after recycling large callback table');
}

done_testing();
