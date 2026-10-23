#!perl
use strict;
use warnings;
use Test::More;

use Sereal::Splitter qw(:all);
use Sereal::Encoder;
use Sereal::Decoder qw(decode_sereal);

my $key = "0123456789abcdef";

# split $doc and return the decoded chunks joined back into one array
sub resplit {
    my ($doc, %opt) = @_;
    my $splitter = Sereal::Splitter->new({ input => $doc, chunk_size => 1, %opt });
    my (@elements, @chunks);
    while (defined(my $chunk = $splitter->next_chunk())) {
        push @chunks, $chunk;
        push @elements, @{ decode_sereal($chunk) };
    }
    return wantarray ? (\@elements, \@chunks) : \@elements;
}

sub version_of { my ($doc) = @_; return ord(substr($doc, 4, 1)) & 0x0f }
sub encoding_of { my ($doc) = @_; return ord(substr($doc, 4, 1)) >> 4 }

sub warnings_of {
    my ($code) = @_;
    my @warnings;
    local $SIG{__WARN__} = sub { push @warnings, $_[0] };
    $code->();
    return @warnings;
}

subtest "chunks keep the input's protocol version" => sub {
    my $data = [ -100, -200, v1.2.3, "x" ];
    my ($elements, $chunks) = resplit(Sereal::Encoder->new->encode($data));
    is_deeply($elements, $data, "NEG_VARINT and v-strings (protocol v6) survive a split");
    is(version_of($chunks->[0]), 6, "chunk of a v6 document is v6");

    my $v4 = Sereal::Encoder->new({ protocol_version => 4 })->encode([ 1, 2 ]);
    (undef, $chunks) = resplit($v4);
    is(version_of($chunks->[0]), 4, "chunk of a v4 document is v4");

    (undef, $chunks) = resplit($v4, header_data_template => create_header_data_template({ min => -100 }, 4));
    is_deeply(Sereal::Decoder::decode_sereal_with_header_data($chunks->[0]), [ { min => -100 }, [ 1 ] ],
              "a header template for an older protocol version");
};

subtest "NO and YES tags (protocol v5)" => sub {
    plan skip_all => "perl $] has no distinct booleans" if $] < 5.036;
    my $data = [ !!1, !!0, "x" ];
    is_deeply(scalar resplit(Sereal::Encoder->new->encode($data)), $data, "booleans survive a split");
};

subtest "FLOAT_128 (protocol v5)" => sub {
    my $doc = "=\xF3rl\x06\x00" . chr(0x42) . chr(56) . ("\x11" x 16) . chr(0x61) . "z";
    my $splitter = Sereal::Splitter->new({ input => $doc, chunk_size => 1 });
    my $chunks = '';
    while (defined(my $chunk = $splitter->next_chunk())) { $chunks .= $chunk }
    like($chunks, qr/\x38\x11{16}/, "FLOAT_128 is copied verbatim");
};

subtest "compressed input" => sub {
    my $data = [ ("abc") x 50 ];
    for my $compress (Sereal::Encoder::SRL_SNAPPY(), Sereal::Encoder::SRL_ZLIB(), Sereal::Encoder::SRL_ZSTD()) {
        my $doc = Sereal::Encoder->new({ compress => $compress, compress_threshold => 0 })->encode($data);
        is_deeply(scalar resplit($doc), $data, "split input compressed with $compress");
    }
};

subtest "compressed output" => sub {
    my $data = [ map { { id => $_, name => "name" x 20 } } 1 .. 20 ];
    my $doc = Sereal::Encoder->new->encode($data);
    for my $compress ([ SRL_SNAPPY, 2 ], [ SRL_ZLIB, 3 ], [ SRL_ZSTD, 4 ]) {
        my ($elements, $chunks) = resplit($doc, compress => $compress->[0], chunk_size => 1000);
        is_deeply($elements, $data, "chunks compressed with $compress->[0] decode");
        is(encoding_of($chunks->[0]), $compress->[1], "and are marked as compressed");
    }
};

subtest "checksummed input (protocol v6)" => sub {
    my $data = [ { a => 1 }, { b => 2 } ];
    my $doc = Sereal::Encoder->new({ checksum_v2_key => $key })->encode($data);

    is_deeply(scalar resplit($doc, checksum_v2_key => $key, checksum_policy => 'die'), $data, "verified and split");

    my $tampered = $doc;
    substr($tampered, -1, 1) ^= "\x01";
    ok(!eval { resplit($tampered, checksum_v2_key => $key, checksum_policy => 'die'); 1 },
       "tampered input is rejected");
    like($@, qr/checksum verification failed/, "with the checksum error");

    my $elements;
    my @warnings = warnings_of(sub { local $Sereal::DefaultKey; $elements = resplit($doc) });
    is_deeply($elements, $data, "split without a key under the default policy");
    like("@warnings", qr/without authentication/, "and warned about it");
};

subtest "malformed input" => sub {
    my $doc = Sereal::Encoder->new->encode([ "abcdef" x 10, "ghijkl" x 10 ]);
    ok(!eval { resplit(substr($doc, 0, -5)); 1 }, "truncated input is rejected");
    like($@, qr/Unexpected termination of packet|end of packet/, "with a bounds error");

    # [ REFP to the enclosing ARRAY ]: following it never terminates
    my $loop = "=\xF3rl\x03\x00" . "\x28\x2b\x01" . "\x29\x02";
    local $SIG{ALRM} = sub { die "timed out\n" };
    alarm 10;
    ok(!eval { resplit($loop, max_recursion_depth => 100); 1 }, "self-referencing REFP is rejected");
    alarm 0;
    like($@, qr/recursion limit/, "by the recursion limit");
};

subtest "independent splitters" => sub {
    my $doc1 = Sereal::Encoder->new({ dedupe_strings => 1 })->encode([ map { "same string $_" } (1, 1, 2, 2) ]);
    my $doc2 = Sereal::Encoder->new({ dedupe_strings => 1 })->encode([ map { "other string $_" } (1, 1, 2, 2) ]);
    my $s1 = Sereal::Splitter->new({ input => $doc1, chunk_size => 1 });
    my $s2 = Sereal::Splitter->new({ input => $doc2, chunk_size => 1 });
    my (@e1, @e2);
    while (1) {
        my $c1 = $s1->next_chunk();
        my $c2 = $s2->next_chunk();
        last if !defined $c1 && !defined $c2;
        push @e1, @{ decode_sereal($c1) } if defined $c1;
        push @e2, @{ decode_sereal($c2) } if defined $c2;
    }
    is_deeply(\@e1, [ map { "same string $_" } (1, 1, 2, 2) ], "first splitter");
    is_deeply(\@e2, [ map { "other string $_" } (1, 1, 2, 2) ], "second splitter");
};

done_testing;
