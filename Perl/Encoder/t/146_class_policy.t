use strict;
use warnings;
use Test::More;
use Sereal::Encoder;

{
    package Good;
    our $freeze_calls = 0;
    sub FREEZE { ++$freeze_calls; return 'frozen' }
}
{
    package Bad;
    our $freeze_calls = 0;
    sub FREEZE { ++$freeze_calls; return 'frozen' }
}

my $good = bless {}, 'Good';
my $bad = bless {}, 'Bad';

for my $list (
    ['Good'],
    { Good => 1, Bad => 0 },
    sub { $_[0] eq 'Good' },
) {
    my $encoder = Sereal::Encoder->new({ allow_classes => $list });
    eval { $encoder->encode($good) };
    is($@, '', 'allow_classes permits the listed class');
    eval { $encoder->encode($bad) };
    like($@, qr/Bad.*allow_classes/, 'allow_classes rejects another class');
}

eval { Sereal::Encoder->new({ paranoid => 1 }) };
like($@, qr/require_key.*checksum key/,
    'paranoid requires a checksum key by default');
{
    local $Sereal::Encoder::ChecksumPolicy = 'ignore';
    eval { Sereal::Encoder->new({ paranoid => 1 }) };
    like($@, qr/require_key.*checksum key/,
        'package checksum policy cannot weaken paranoid');
}
my $authenticated_encoder = Sereal::Encoder->new({
    paranoid => 1, checksum_key => 'k' x 16,
});
my $signed = $authenticated_encoder->encode(7);
ok(length($signed) > length(Sereal::Encoder->new->encode(7)),
    'paranoid with a key emits a checksum');

my $encoder = Sereal::Encoder->new({
    allow_classes => ['Good'], freeze_callbacks => 1,
});
eval { $encoder->encode($bad) };
like($@, qr/Bad.*allow_classes/, 'class filter runs before FREEZE');
is($Bad::freeze_calls, 0, 'denied object did not run FREEZE');

my %classes = (Good => 2);
$encoder = Sereal::Encoder->new({ track_classes => \%classes });
$encoder->encode([$good, $good, bless {}, 'Good']);
is($classes{Good}, 4, 'track_classes counts object tags and accumulates');

$encoder = Sereal::Encoder->new({
    paranoid => 1, checksum_policy => 'allow_unsigned',
});
eval { $encoder->encode($good) };
like($@, qr/refuse_objects/, 'paranoid refuses objects without allow_classes');

$encoder = Sereal::Encoder->new({
    paranoid => 1, checksum_policy => 'allow_unsigned',
    allow_classes => ['Good'],
});
eval { $encoder->encode($good) };
is($@, '', 'paranoid permits an allowlisted object');

eval { Sereal::Encoder->new({
    paranoid => 1, checksum_policy => 'allow_unsigned',
    misspelled_limit => 1,
}) };
like($@, qr/misspelled_limit/, 'paranoid rejects unknown options');

my $deep = [];
$deep = [$deep] for 1 .. 2050;
eval { $encoder->encode($deep) };
like($@, qr/maximum recursion depth \(2048\)/i,
    'paranoid uses the 2048 recursion limit');

done_testing;
