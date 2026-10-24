#!perl
use strict;
use warnings;
use Config;
use POSIX ();
use Test::More;

use Sereal::Splitter qw(:all);
use Sereal::Decoder qw(decode_sereal);

my $header = "=\xF3rl\x06\x00";

sub varint {
    my ($n) = @_;
    my $s = '';
    while ($n >= 0x80) { $s .= chr(($n & 0x7f) | 0x80); $n >>= 7 }
    return $s . chr($n);
}

# split the packet into chunks; returns the chunks, or nothing with the
# error in $@
sub split_packet {
    my ($packet) = @_;
    my @chunks;
    eval {
        my $splitter = Sereal::Splitter->new({ input => $packet, chunk_size => 1 });
        while (defined(my $chunk = $splitter->next_chunk())) { push @chunks, $chunk }
        1;
    } or return;
    return @chunks;
}

sub split_fails {
    my ($body, $error, $name) = @_;
    ok(!split_packet($header . $body), "$name is rejected");
    like($@, $error, "$name: error message");
}

# run $code in a child process and return "ok" or "error: <message>", or
# "timeout" if it has not finished after $secs seconds
sub in_child {
    my ($secs, $code) = @_;
    pipe(my $r, my $w) or die "pipe: $!";
    my $pid = fork();
    die "fork: $!" unless defined $pid;
    if (!$pid) {
        close $r;
        my $ok = eval { $code->(); 1 };
        print $w ($ok ? "ok" : "error: $@");
        close $w;
        POSIX::_exit(0);
    }
    close $w;
    my $timed_out = 0;
    local $SIG{ALRM} = sub { $timed_out = 1; kill 'KILL', $pid };
    alarm $secs;
    my $out = do { local $/; <$r> };
    alarm 0;
    waitpid($pid, 0);
    return $timed_out ? "timeout" : $out;
}

subtest "COPY" => sub {
    plan skip_all => "needs fork" unless $Config{d_fork};
    # REFN ARRAY [ "xxxxxxxx", ( ARRAYREF_2 [ COPY of the previous level x 2 ] ) x 30 ]:
    # every COPY was expanded in full, 2**30 copies of the string in all
    my $levels = 30;
    my $body = "\x28\x2b" . varint($levels + 1);
    my $prev = length($body) + 1;
    $body .= "\x68" . ("x" x 8);
    for (1 .. $levels) {
        my $ofs = length($body) + 1;
        $body .= "\x42" . ("\x2f" . varint($prev)) x 2;
        $prev = $ofs;
    }
    like(in_child(10, sub { split_packet($header . $body) or die $@ }),
         qr/^error: .*COPY target/, "COPY of a non-string is rejected");

    # REFN ARRAY [ "abcd", COPY of "abcd" ]
    my @chunks = split_packet($header . "\x28\x2b\x02\x64abcd\x2f\x04");
    is_deeply([ map { @{ decode_sereal($_) } } @chunks ], [ "abcd", "abcd" ], "COPY of a string is split");
};

subtest "references to untracked items" => sub {
    # REFN ARRAY [ [] (untracked), REFP / ALIAS to it ]
    split_fails("\x28\x2b\x02\x40\x29\x04", qr/unknown item/, "REFP to an untracked item");
    split_fails("\x28\x2b\x02\x40\x2e\x04", qr/unknown item/, "ALIAS to an untracked item");
    # REFN ARRAY [ [], OBJECTV with the [] as its class name, [] ]
    split_fails("\x28\x2b\x02\x40\x2d\x04\x40", qr/class name/, "OBJECTV of a non-string");

    # REFN ARRAY [ [] (tracked), REFP to it ], split into one element per chunk
    my @chunks = split_packet($header . "\x28\x2b\x02\xc0\x29\x04");
    is(scalar @chunks, 2, "REFP to a tracked item in an earlier chunk is split");
    is_deeply([ map { @{ decode_sereal($_) } } @chunks ], [ [], \[] ], "and copies the item");
};

subtest "protocol v1 offsets into the header" => sub {
    # v1 offsets count from the start of the packet, so offsets 1 to 5 point
    # into the header; after decompression that is memory that was never
    # written. REFN ARRAY [ "q", COPY of offset 1 ]
    my $body = "\x28\x2b\x02\x61q\x2f\x01";
    my $snappy = "=srl\x11\x00" . chr(length $body) . chr((length($body) - 1) << 2) . $body;
    for ([ "raw", "=srl\x01\x00" . $body ], [ "snappy", $snappy ]) {
        my ($name, $packet) = @$_;
        ok(!split_packet($packet), "$name: COPY of a header byte is rejected");
        like($@, qr/points before the document body/, "$name: error message");
    }
};

done_testing;
