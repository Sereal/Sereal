#!perl
use strict;
use warnings;
use Test::More;
use IPC::Open3;
use Symbol qw(gensym);
use Sereal::Decoder;
use Sereal::Decoder::Constants qw(SRL_EXT_DEADLY);

is( SRL_EXT_DEADLY, 0, 'DEADLY extension ID is zero' );
my $header = "=\xf3rl\x06\0";
my $deadly = "\x3e\0";
my $dec    = Sereal::Decoder->new;
for my $body ( $deadly, "\x41" . $deadly ) {
    eval { $dec->decode( $header . $body ) };
    like( $@, qr/Encountered DEADLY extended tag/, 'DEADLY is fatal when parsed' );
}
is(
    $dec->decode( $header . "\x62" . $deadly ), $deadly,
    'DEADLY bytes in a string are ordinary data'
);
my $packet = $header . "\x07";
is( $dec->decode( $packet . $deadly ), 7,               'trailing DEADLY is not parsed' );
is( $dec->bytes_consumed,              length($packet), 'sentinel does not change packet framing' );
is(
    $dec->decode_with_offset( $packet . $header . "\x08", length($packet) ), 8,
    'concatenated documents retain their framing'
);

my ( $in, $out );
my $err = gensym;
my $pid = open3( $in, $out, $err, $^X, '-Mblib', 'author_tools/hobodecoder.pl' );
binmode $in;
print {$in} $header . $deadly;
close $in;
my $stdout = do { local $/; <$out> };
my $stderr = do { local $/; <$err> };
waitpid( $pid, 0 );
ok( $? != 0, 'hobodecoder fails on DEADLY' );
like( $stderr, qr/Encountered DEADLY extended tag/, 'hobodecoder identifies DEADLY' );
done_testing;
