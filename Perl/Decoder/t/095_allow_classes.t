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
    my $decoder = Sereal::Decoder->new({ allow_classes => $list });
    isa_ok($decoder->decode($good), 'Good', "$kind permits ordinary object");
    eval { $decoder->decode($bad) };
    like($@, qr/Bad.*allow_classes/, "$kind rejects ordinary object");
}

my $frozen_good = $prefix . "\x32\x64Good\x28\x2b\x00";
{
    package Good;
    sub THAW { bless {}, $_[0] }
}

my $decoder = Sereal::Decoder->new({
    allow_classes => ['Good'],
    thaw_allow_classes => [],
});
eval { $decoder->decode($frozen_good) };
like($@, qr/THAW not allowed.*thaw_allow_classes/,
    'THAW filter applies after class allowlist');

$decoder = Sereal::Decoder->new({ allow_classes => [] });
eval { $decoder->decode($frozen_good) };
like($@, qr/Good.*allow_classes/, 'allowlist rejects frozen class');

eval { Sereal::Decoder->new({ allow_classes => 'Good' }) };
like($@, qr/allow_classes.*array ref, hash ref or code ref/,
    'invalid allowlist is rejected');

$decoder = Sereal::Decoder->new({ refuse_objects => 1, allow_classes => ['Good'] });
eval { $decoder->decode($good) };
like($@, qr/objects.*refused|refuse_objects/i,
    'refuse_objects rejects even an allowed class');

$decoder = Sereal::Decoder->new({ allowed_classes => ['Good'] });
isa_ok($decoder->decode($good), 'Good', 'allowed_classes alias remains accepted');

eval { Sereal::Decoder->new({ allow_classes => [], allowed_classes => [] }) };
like($@, qr/Specify only one/, 'both names together are rejected');

my %classes = (Good => 2);
$decoder = Sereal::Decoder->new({ track_classes => \%classes });
$decoder->decode($good);
$decoder->decode($frozen_good);
is_deeply(\%classes, { Good => 4 }, 'tracks ordinary and frozen objects cumulatively');

$decoder = Sereal::Decoder->new({ track_classes => \%classes, no_bless_objects => 1 });
$decoder->decode($good);
is($classes{Good}, 5, 'tracks an object decoded without blessing');

my $repeated = "=\xF3rl\x07\x00\x42\x2c\x64Good\x28\xaa\x00\x29\x09";
%classes = ();
$decoder = Sereal::Decoder->new({ track_classes => \%classes });
$decoder->decode($repeated);
is($classes{Good}, 1, 'a repeated reference is not another object');

eval { Sereal::Decoder->new({ track_classes => [] }) };
like($@, qr/track_classes.*hash ref/, 'track_classes requires a hash');

done_testing;
