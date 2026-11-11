use strict;
use warnings;
use Test::More;
use Sereal::Decoder;

my $prefix = "=\xF3rl\x03\x00";
my $good = $prefix . "\x2c\x64Good\x28\x01";
my $bad  = $prefix . "\x2c\x63Bad\x28\x01";

for my $entry (
    [ array => ['Good'] ],
    [ hash  => { Good => 1, Bad => 0 } ],
    [ code  => sub { $_[0] eq 'Good' } ],
) {
    my ($kind, $list) = @$entry;
    my $decoder = Sereal::Decoder->new({ allowed_classes => $list });
    isa_ok($decoder->decode($good), 'Good', "$kind permits ordinary object");
    eval { $decoder->decode($bad) };
    like($@, qr/Bad.*allowed_classes/, "$kind rejects ordinary object");
}

my $frozen_good = $prefix . "\x32\x64Good\x28\x2b\x00";
{
    package Good;
    sub THAW { bless {}, $_[0] }
}

my $decoder = Sereal::Decoder->new({
    allowed_classes => ['Good'],
    thaw_allow_classes => [],
});
eval { $decoder->decode($frozen_good) };
like($@, qr/THAW not allowed.*thaw_allow_classes/,
    'THAW filter applies after class allowlist');

$decoder = Sereal::Decoder->new({ allowed_classes => [] });
eval { $decoder->decode($frozen_good) };
like($@, qr/Good.*allowed_classes/, 'allowlist rejects frozen class');

eval { Sereal::Decoder->new({ allowed_classes => 'Good' }) };
like($@, qr/allowed_classes.*array ref, hash ref or code ref/,
    'invalid allowlist is rejected');

$decoder = Sereal::Decoder->new({ refuse_objects => 1, allowed_classes => ['Good'] });
eval { $decoder->decode($good) };
like($@, qr/objects.*refused|refuse_objects/i,
    'refuse_objects rejects even an allowed class');

done_testing;
