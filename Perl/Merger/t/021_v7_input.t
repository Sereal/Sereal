#!perl
use strict;
use warnings;
use Test::More;
use Config;
use Sereal::Merger;
use Sereal::Encoder;
use Sereal::Decoder qw(decode_sereal);

my $doc = Sereal::Encoder->new({ protocol_version => 7 })->encode([ 0.5, 0.1, 'after' ]);
my $merger = Sereal::Merger->new({ top_level_element => 1 });
$merger->append($doc);
my $merged = $merger->finish();
is(ord(substr($merged, 4, 1)) & 15, 6, 'Merger emits its supported protocol version');
is_deeply(decode_sereal($merged), [ [ 0.5, 0.1, 'after' ] ],
          'v7 portable FLOAT and DOUBLE values survive v6 output');

my $bad_declaration = "=\xF3rl\x07\x00\x3e\x01\x01\x7f\x01";
$merger = Sereal::Merger->new({ top_level_element => 1 });
ok(!eval { $merger->append($bad_declaration); 1 }, 'unknown v7 declaration is rejected');
like($@, qr/Unknown declaration kind/, 'declaration error identifies the problem');

SKIP: {
    skip 'requires a little-endian x87 long double ABI', 2
        unless $Config{archname} =~ /(?:x86_64|i[3-6]86)/
            && $Config{byteorder} =~ /^1234/
            && ($Config{longdblsize} == 12 || $Config{longdblsize} == 16);
    my $native = ("\0" x 7) . "\x80\xfe\x3f" . ("\0" x ($Config{longdblsize} - 10));
    my $declared = "=\xF3rl\x07\x00\x3e\x01\x03\x01\x02"
                 . chr($Config{longdblsize}) . "\x24" . $native;
    is(decode_sereal($declared), 0.5, 'v7 LONG_DOUBLE declaration describes native bytes');
    my $native_merger = Sereal::Merger->new({ top_level_element => 0 });
    $native_merger->append($declared);
    is(decode_sereal($native_merger->finish), 0.5, 'native LONG_DOUBLE survives v6 merge');
}

done_testing();
