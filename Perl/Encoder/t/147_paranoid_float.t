use strict;
use warnings;
use Config;
use Test::More;
use Sereal::Encoder;

my $value = 1 + 2**-60;
my $unsigned = { checksum_policy => 'allow_unsigned' };
my $paranoid = Sereal::Encoder->new({ paranoid => 1, %$unsigned });
my $double = Sereal::Encoder->new({ use_standard_double => 1 });
is($paranoid->encode($value), $double->encode($value),
    'paranoid uses IEEE double output by default');

SKIP: {
    skip 'this Perl has no precision beyond double', 1
        unless $Config{nvsize} > 8 && $value != 1;
    my $wide = Sereal::Encoder->new({
        paranoid => 1, %$unsigned, use_standard_double => 0,
    });
    isnt($wide->encode($value), $double->encode($value),
        'explicit false restores extended precision');
}

done_testing;
