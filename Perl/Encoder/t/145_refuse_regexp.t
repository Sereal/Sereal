use strict;
use warnings;
use Test::More;
use Sereal::Encoder;

my $encoder = Sereal::Encoder->new({ refuse_objects => 1 });
my $regexp = qr/x/;
my $packet = eval { $encoder->encode($regexp) };
is($@, '', 'refuse_objects permits a real regexp');
like(unpack('H*', $packet), qr/2c665265676578702831/,
    'regexp retains its canonical object and REGEXP tags');

eval { $encoder->encode(bless {}, 'Regexp') };
like($@, qr/refuse_objects/, 'a hash blessed as Regexp is refused');

my $other_regexp = bless qr/y/, 'Other';
eval { $encoder->encode($other_regexp) };
like($@, qr/refuse_objects/, 'a regexp with another class is refused');

$encoder = Sereal::Encoder->new({
    paranoid => 1, checksum_policy => 'allow_unsigned',
});
is(ref($regexp), 'Regexp', 'canonical regexp class is intact');
eval { $encoder->encode($regexp) };
is($@, '', 'paranoid permits a real regexp');

$encoder = Sereal::Encoder->new({ croak_on_bless => 1 });
eval { $encoder->encode($regexp) };
like($@, qr/croak_on_bless/, 'croak_on_bless still rejects regexps');

done_testing;
