#!perl
use strict;
use warnings;
# must be loaded before Sereal::TestSet
use Sereal::Merger qw(:all);
use Sereal::Merger::Constants qw(:all);
use Sereal::Encoder;
use Sereal::Decoder qw(decode_sereal);
use File::Spec;

use lib File::Spec->catdir(qw(t lib));
BEGIN {
  lib->import('lib')
    if !-d 't';
}

use Sereal::TestSet qw(Header);
use Test::More;

my $key = "0123456789abcdef";

sub merge_docs {
    my ($opt, @docs) = @_;
    my $mrg = Sereal::Merger->new({ top_level_element => SRL_TOP_LEVEL_ARRAY, %$opt });
    $mrg->append($_) for @docs;
    return $mrg->finish();
}

sub warnings_of {
    my ($code) = @_;
    my @warnings;
    local $SIG{__WARN__} = sub { push @warnings, $_[0] };
    $code->();
    return @warnings;
}

subtest "NO and YES tags (protocol v5)" => sub {
    plan skip_all => "perl $] has no distinct booleans" if $] < 5.036;
    my $doc = Sereal::Encoder->new->encode([ !!1, !!0 ]);
    my $out = merge_docs({}, $doc, $doc);
    is_deeply(decode_sereal($out), [ [ !!1, !!0 ], [ !!1, !!0 ] ], "booleans survive a merge");

    my $v4 = merge_docs({ protocol_version => 4 }, $doc);
    is_deeply(decode_sereal($v4), [ [ 1, '' ] ], "protocol v4 output falls back to TRUE/FALSE");
};

subtest "compressed input" => sub {
    for my $compress (Sereal::Encoder::SRL_SNAPPY(), Sereal::Encoder::SRL_ZLIB(), Sereal::Encoder::SRL_ZSTD()) {
        my $doc = Sereal::Encoder->new({ compress => $compress, compress_threshold => 0 })
            ->encode([ ("abc") x 50 ]);
        my $out = merge_docs({}, $doc);
        is_deeply(decode_sereal($out), [ [ ("abc") x 50 ] ], "merged input compressed with $compress");
    }
};

subtest "checksummed input (protocol v6)" => sub {
    my $doc = Sereal::Encoder->new({ checksum_v2_key => $key })->encode({ a => [ 1, 2 ] });

    my $out = merge_docs({ checksum_v2_key => $key, checksum_policy => 'die' }, $doc);
    is_deeply(decode_sereal($out), [ { a => [ 1, 2 ] } ], "verified and merged");

    my $tampered = $doc;
    substr($tampered, -1, 1) ^= "\x01";
    ok(!eval { merge_docs({ checksum_v2_key => $key, checksum_policy => 'die' }, $tampered); 1 },
       "tampered input is rejected");
    like($@, qr/checksum verification failed/, "with the checksum error");

    my @warnings = warnings_of(sub {
        local $Sereal::DefaultKey;
        $out = merge_docs({}, $doc);
    });
    is_deeply(decode_sereal($out), [ { a => [ 1, 2 ] } ], "merged without a key under the default policy");
    like("@warnings", qr/without authentication/, "and warned about it");

    my $plain = Sereal::Encoder->new->encode(1);
    ok(!eval { merge_docs({ checksum_v2_key => $key, checksum_policy => 'require_key' }, $plain); 1 },
       "require_key rejects unprotected input");
    like($@, qr/requires a checksummed packet/, "with the policy error");
};

subtest "FLOAT_128 (protocol v5)" => sub {
    my $doc = Header(6) . chr(SRL_HDR_FLOAT_128) . ("\x11" x 16);
    my $mrg = Sereal::Merger->new({ top_level_element => SRL_TOP_LEVEL_SCALAR });
    $mrg->append($doc);
    is($mrg->finish(), $doc, "FLOAT_128 is copied verbatim");

    $mrg = Sereal::Merger->new({ top_level_element => SRL_TOP_LEVEL_SCALAR, protocol_version => 4 });
    ok(!eval { $mrg->append($doc); 1 }, "FLOAT_128 cannot be merged into protocol v4 output");
    like($@, qr/FLOAT_128/, "with an error naming the tag");
};

subtest "UNDEF_DEADLY must not be emitted" => sub {
    for my $version (4, 6) {
        for my $body ("\x3e\xff", "\x41\xbe\xff") {
            ok(!eval { merge_docs({ protocol_version => $version }, Header(6) . $body); 1 },
                "cannot copy a sentinel into protocol $version output");
            like($@, qr/Cannot emit UNDEF_DEADLY/, "with an explicit diagnostic");
        }
    }
    my $doc = Header(6) . "\x62\x3e\xff";
    is_deeply(decode_sereal(merge_docs({}, $doc)), ["\x3e\xff"], "the same bytes inside a string are data");
};

subtest "truncated input" => sub {
    for my $tag (SRL_HDR_FLOAT, SRL_HDR_DOUBLE, SRL_HDR_LONG_DOUBLE, SRL_HDR_FLOAT_128) {
        my $mrg = Sereal::Merger->new();
        ok(!eval { $mrg->append(Header(6) . chr($tag) . "\x00\x00"); 1 },
           "truncated payload of tag $tag is rejected");
        like($@, qr/Unexpected termination of packet/, "with a bounds error");
    }
};

done_testing();
