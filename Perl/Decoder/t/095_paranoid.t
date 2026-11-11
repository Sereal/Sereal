use strict;
use warnings;
use Test::More;
use Sereal::Decoder;

# These fixtures are unsigned; opt out while testing other paranoid controls.
sub paranoid_decoder {
    my ($options) = @_;
    return Sereal::Decoder->new({
        paranoid => 1, checksum_policy => 'allow_unsigned', %{$options || {}},
    });
}

eval { Sereal::Decoder->new({ paranoid => 1 }) };
like($@, qr/require_key.*checksum key/,
    'paranoid requires a checksum key by default');
{
    local $Sereal::Decoder::ChecksumPolicy = 'ignore';
    eval { Sereal::Decoder->new({ paranoid => 1 }) };
    like($@, qr/require_key.*checksum key/,
        'package checksum policy does not weaken paranoid');
}
my $authenticated_decoder = Sereal::Decoder->new({
    paranoid => 1, checksum_key => 'k' x 16,
});
eval { $authenticated_decoder->decode("=\xF3rl\x03\x00\x01") };
like($@, qr/require_key.*checksummed packet/,
    'paranoid with a key rejects unsigned input');

my $prefix = "=\xF3rl\x03\x00";
my $good = $prefix . "\x2c\x64Good\x28\x01";
my $bad  = $prefix . "\x2c\x63Bad\x28\x01";
my $regexp = $prefix . "\x2c\x66Regexp\x28\x31\x61x\x60";
my $pretend_regexp = $prefix . "\x2c\x66Regexp\x28\x2a\x00";
my $reused_regexp_class = "=\xF3rl\x07\x00\x42\x2c\x66Regexp\x28\x31\x61a\x60\x2d\x03\x28\x31\x61b\x60";
my $reused_fake_regexp = "=\xF3rl\x07\x00\x42\x2c\x66Regexp\x28\x31\x61a\x60\x2d\x03\x28\x2a\x00";

my $decoder = paranoid_decoder({ });
eval { $decoder->decode($good) };
like($@, qr/objects.*refused|refuse_objects/i,
    'paranoid refuses objects without allow_classes');
is(ref($decoder->decode($regexp)), 'Regexp',
    'paranoid permits a real regexp object');
eval { $decoder->decode($pretend_regexp) };
like($@, qr/objects.*refused|refuse_objects/i,
    'paranoid rejects a non-regexp claiming the Regexp class');
is_deeply([map { ref($_) } @{$decoder->decode($reused_regexp_class)}],
    ['Regexp', 'Regexp'], 'paranoid permits OBJECTV with a real regexp');
eval { $decoder->decode($reused_fake_regexp) };
like($@, qr/objects.*refused|refuse_objects/i,
    'paranoid refuses OBJECTV with a forged regexp');

$decoder = paranoid_decoder({ allow_classes => ['Good'] });
isa_ok($decoder->decode($good), 'Good', 'allowlist permits a class');
eval { $decoder->decode($bad) };
like($@, qr/Bad.*allow_classes/, 'allowlist rejects other classes');

{
    package Good;
    our $thaw_calls = 0;
    sub THAW { ++$thaw_calls; bless {}, $_[0] }
}
my $frozen_good = $prefix . "\x32\x64Good\x28\x2b\x00";
$decoder = paranoid_decoder({ allow_classes => ['Good'] });
isa_ok($decoder->decode($frozen_good), 'Sereal::Decoder::THAW_args',
    'paranoid suppresses THAW without a thaw allowlist');
is($Good::thaw_calls, 0, 'suppressed THAW was not called');

$decoder = paranoid_decoder({ allow_classes => ['Good'], thaw_allow_classes => ['Good'],
});
isa_ok($decoder->decode($frozen_good), 'Good',
    'thaw allowlist permits THAW in paranoid mode');
is($Good::thaw_calls, 1, 'allowlisted THAW was called');

$decoder = paranoid_decoder({ allow_classes => ['Good'], no_thaw_objects => 0,
});
isa_ok($decoder->decode($frozen_good), 'Good',
    'explicit no_thaw_objects override permits THAW');
is($Good::thaw_calls, 2, 'explicitly enabled THAW was called');

$decoder = paranoid_decoder({ allow_classes => undef });
eval { $decoder->decode($good) };
like($@, qr/objects.*refused|refuse_objects/i,
    'undefined allowlist does not permit objects');

$decoder = paranoid_decoder({ refuse_objects => 0 });
isa_ok($decoder->decode($good), 'Good',
    'explicit refuse_objects setting takes precedence');

my $deep = $prefix . ("\x28\x2b\x01" x 1025) . "\x01";
$decoder = paranoid_decoder({ });
eval { $decoder->decode($deep) };
like($@, qr/Reached recursion limit \(2048\)/,
    'paranoid uses the tighter recursion limit');

$decoder = paranoid_decoder({ max_recursion_depth => undef });
eval { $decoder->decode($deep) };
like($@, qr/Reached recursion limit \(2048\)/,
    'undefined recursion limit retains paranoid default');

$decoder = paranoid_decoder({ max_recursion_depth => 10000 });
eval { $decoder->decode($deep) };
is($@, '', 'explicit recursion limit takes precedence');

my $large = "=\xF3rl\x36\x00" . "\x81\x80\x80\x80\x01" . "\x00";
$decoder = paranoid_decoder({ });
eval { $decoder->decode($large) };
like($@, qr/larger than the allowed maximum/,
    'paranoid rejects a declared decompressed size above 256 MiB');

my $small = "=\xF3rl\x36\x00" . "\x80\x08" . "\x00";
$decoder = paranoid_decoder({ max_uncompressed_size => 512,
});
eval { $decoder->decode($small) };
like($@, qr/larger than the allowed maximum/,
    'explicit decompression limit takes precedence');

sub varint {
    my ($n) = @_;
    my $out = '';
    while ($n >= 128) {
        $out .= chr(($n & 127) | 128);
        $n >>= 7;
    }
    return $out . chr($n);
}

for my $case (
    [ max_string_length => "\x26" . varint(64 * 1024 * 1024 + 1),
      qr/configured maximum/i ],
    [ max_num_array_entries => "\x28\x2b" . varint(1_000_001),
      qr/configured maximum|maximum.*array/i ],
    [ max_num_hash_entries => "\x28\x2a" . varint(250_001),
      qr/configured maximum|maximum.*hash/i ],
) {
    my ($option, $body, $error) = @$case;
    my $packet = $prefix . $body;
    eval { paranoid_decoder({})->decode($packet) };
    like($@, $error, "paranoid caps $option");
    eval { paranoid_decoder({ $option => 0 })->decode($packet) };
    unlike($@, $error, "explicit zero overrides $option cap");
    eval { paranoid_decoder({ $option => undef })->decode($packet) };
    like($@, $error, "undef preserves paranoid $option cap");
}

my $invalid_utf8 = "=srl\x01\x00'\x01\xC0";
$decoder = paranoid_decoder({ });
eval { $decoder->decode($invalid_utf8) };
like($@, qr/Invalid UTF8 byte sequence/,
    'paranoid validates UTF-8 text');

$decoder = paranoid_decoder({ validate_utf8 => 0 });
eval { $decoder->decode($invalid_utf8) };
is($@, '', 'explicit validate_utf8 setting takes precedence');

eval { paranoid_decoder({ misspelled_limit => 1 }) };
like($@, qr/misspelled_limit/,
    'paranoid rejects unknown constructor options');

{
    local $SIG{__WARN__} = sub { };
    eval { paranoid_decoder({ unknown_option_policy => 'warn',
        misspelled_limit => 1,
    }) };
    is($@, '', 'explicit unknown option policy takes precedence');
}

done_testing;
