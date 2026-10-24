#!perl
use strict;
use warnings;
use Test::More;

use Sereal::Merger qw(:all);
use Sereal::Encoder qw(encode_sereal);
use Sereal::Decoder qw(decode_sereal);

my @docs = map { encode_sereal({ id => $_, text => "compressible " x 20 }) } 1 .. 5;
my $expected = [ map { decode_sereal($_) } @docs ];

sub encoding_of { my ($doc) = @_; return ord(substr($doc, 4, 1)) >> 4 }

for my $case ([ SRL_SNAPPY, 'Snappy', 2 ], [ SRL_ZLIB, 'Zlib', 3 ], [ SRL_ZSTD, 'Zstd', 4 ]) {
    my ($compress, $name, $encoding) = @$case;
    my $mrg = Sereal::Merger->new({ compress => $compress });
    $mrg->append_all(\@docs);
    my $out = $mrg->finish();
    is(encoding_of($out), $encoding, "$name: output is marked as compressed");
    is_deeply(decode_sereal($out), $expected, "$name: output decodes");

    $mrg = Sereal::Merger->new({ compress => $compress, compress_level => 1 });
    $mrg->append_all(\@docs);
    is_deeply(decode_sereal($mrg->finish()), $expected, "$name: output with compress_level decodes");
}

{
    my $mrg = Sereal::Merger->new({ compress => SRL_ZSTD });
    $mrg->append_all(\@docs);
    my $out = $mrg->finish(encode_sereal({ header => 1 }));
    is_deeply(Sereal::Decoder::decode_sereal_with_header_data($out), [ { header => 1 }, $expected ],
              "compressed output with a user header decodes");
}

is(SRL_UNCOMPRESSED, 0, "SRL_UNCOMPRESSED is exported");

ok(!eval { Sereal::Merger->new({ compress => SRL_ZSTD, protocol_version => 3 }); 1 },
   "zstd needs protocol version 4");
like($@, qr/requires protocol version 4/, "with an error saying so");

ok(!eval { Sereal::Merger->new({ compress => 42 }); 1 }, "unknown compression is rejected");

done_testing;
