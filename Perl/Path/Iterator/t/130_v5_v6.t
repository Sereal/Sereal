#!perl
use strict;
use warnings;

use Config;
use Test::More;
use Sereal::Path::Iterator;
use Sereal::Encoder;

my $key = "0123456789abcdef";

# hand-built documents: protocol v6 header without a suffix, then the body
sub v6_doc { return "=\xF3rl\x06\x00" . join('', @_) }
sub arrayref { my $n = shift; return chr(0x40 + $n) }
sub short_binary { my $s = shift; return chr(0x60 + length $s) . $s }

sub warnings_of {
    my ($code) = @_;
    my @warnings;
    local $SIG{__WARN__} = sub { push @warnings, $_[0] };
    $code->();
    return @warnings;
}

subtest "NO and YES tags (protocol v5)" => sub {
    plan skip_all => "perl $] has no distinct booleans" if $] < 5.036;
    my $spi = Sereal::Path::Iterator->new(Sereal::Encoder->new->encode([ !!1, !!0, "after" ]));
    $spi->step_in();
    is($spi->decode(), !!1, 'decode YES');
    is_deeply([ $spi->info() ], [ SRL_INFO_SCALAR, 1 ], 'info of YES');
    $spi->next(2);
    is($spi->decode(), 'after', 'step over YES and NO');
};

subtest "decode() knows the document's protocol version" => sub {
    my $spi = Sereal::Path::Iterator->new(Sereal::Encoder->new->encode([ -100, v1.2.3 ]));
    $spi->step_in();
    is($spi->decode(), -100, 'decode NEG_VARINT (protocol v6)');
    $spi->next();
    is(sprintf('%vd', $spi->decode()), '1.2.3', 'decode a v-string (protocol v6 EXTEND)');
};

subtest "FLOAT_128 (protocol v5)" => sub {
    my $spi = Sereal::Path::Iterator->new(v6_doc(arrayref(2), chr(56), "\x11" x 16, short_binary("z")));
    $spi->step_in();
    is_deeply([ $spi->info() ], [ SRL_INFO_SCALAR, 1 ], 'info of FLOAT_128');
    $spi->next();
    is($spi->decode(), 'z', 'step over FLOAT_128');
};

subtest "LONG_DOUBLE is sizeof(long double) like the decoder" => sub {
    my $spi = Sereal::Path::Iterator->new(
        v6_doc(arrayref(2), chr(36), "\x00" x $Config{longdblsize}, short_binary("z")));
    $spi->step_in();
    $spi->next();
    is($spi->decode(), 'z', "step over a $Config{longdblsize} byte LONG_DOUBLE");
};

subtest "PAD before a value" => sub {
    my $spi = Sereal::Path::Iterator->new(v6_doc(arrayref(2), chr(63) x 2, short_binary("a"), short_binary("b")));
    $spi->step_in();
    $spi->next();
    is($spi->decode(), 'b', 'next() steps over PAD and the padded value');

    $spi = Sereal::Path::Iterator->new(v6_doc(arrayref(2), short_binary("a"), chr(63), short_binary("b")));
    $spi->step_in(2);
    is($spi->decode(), 'b', 'step_in() steps over PAD');
};

subtest "truncated payloads" => sub {
    for my $tag (34, 35, 36, 56) {
        my $spi = Sereal::Path::Iterator->new(v6_doc(arrayref(2), chr($tag), "\x00\x00"));
        $spi->step_in();
        ok(!eval { $spi->next(); 1 }, "truncated payload of tag $tag is rejected");
        like($@, qr/Unexpected termination of packet/, 'with a bounds error');
    }
};

subtest "checksummed documents (protocol v6)" => sub {
    my $doc = Sereal::Encoder->new({ checksum_v2_key => $key })->encode([ 'a', { b => 2 } ]);

    my $spi = Sereal::Path::Iterator->new($doc, { checksum_v2_key => $key, checksum_policy => 'die' });
    $spi->step_in();
    $spi->next();
    is_deeply($spi->decode(), { b => 2 }, 'verified and iterated');

    my $tampered = $doc;
    substr($tampered, -1, 1) ^= "\x01";
    ok(!eval { Sereal::Path::Iterator->new($tampered, { checksum_v2_key => $key, checksum_policy => 'die' }); 1 },
       'tampered document is rejected');
    like($@, qr/checksum verification failed/, 'with the checksum error');

    my @warnings = warnings_of(sub {
        local $Sereal::DefaultKey;
        $spi = Sereal::Path::Iterator->new($doc);
    });
    $spi->step_in();
    is($spi->decode(), 'a', 'iterated without a key under the default policy');
    like("@warnings", qr/without authentication/, 'and warned about it');

    my $sp = Sereal::Path->new($doc, { checksum_v2_key => $key, checksum_policy => 'die' });
    is($sp->value('$[1].b'), 2, 'Sereal::Path passes the checksum options on');
} if eval { require Sereal::Path; 1 };

done_testing();
