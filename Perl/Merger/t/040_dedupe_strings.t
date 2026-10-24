#!perl
use strict;
use warnings;
use Test::More;

use Sereal::Merger qw(:all);
use Sereal::Encoder qw(encode_sereal);
use Sereal::Decoder qw(decode_sereal);

# strings of every length up to 40 at varying offsets, so that the string
# table hashes unaligned data of every size
my @docs = map { encode_sereal([ "x" x ($_ % 7), "a fairly long string " . ("y" x $_), bless({ k => 1 }, "Some::Class") ]) } 0 .. 40;
my $expected = [ map { decode_sereal($_) } @docs, @docs ];

my $plain = Sereal::Merger->new();
$plain->append_all([ @docs, @docs ]);
my $plain_out = $plain->finish();

my $deduped = Sereal::Merger->new({ dedupe_strings => 1 });
$deduped->append_all([ @docs, @docs ]);
my $deduped_out = $deduped->finish();

is_deeply(decode_sereal($deduped_out), $expected, "deduplicated output decodes");
cmp_ok(length $deduped_out, '<', length $plain_out, "and is smaller");

done_testing;
