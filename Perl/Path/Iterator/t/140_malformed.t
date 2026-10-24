#!perl
use strict;
use warnings;
use Config;
use POSIX ();
use Test::More;
use Sereal::Path::Iterator;
use Sereal::Path;

my $header = "=\xF3rl" . chr(6) . chr(0);

sub varint {
    my ($n) = @_;
    my $s = '';
    while ($n >= 0x80) { $s .= chr(($n & 0x7f) | 0x80); $n >>= 7 }
    return $s . chr($n);
}

# run $code on an iterator over the packet; returns false with the error
# in $@ if it croaks
sub iter_ok {
    my ($body, $code) = @_;
    my $iter = Sereal::Path::Iterator->new($header . $body);
    return eval { $code->($iter); 1 };
}

# run $code in a child process and return "ok" or "error: <message>", or
# "timeout" if it has not finished after $secs seconds: some of the bugs
# tested here were endless loops in XS code, which no signal interrupts
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

subtest "back-references to their own tag" => sub {
    plan skip_all => "needs fork" unless $Config{d_fork};
    # COPY whose offset is the offset of the COPY tag itself
    my $packet = $header . "\x2f\x01";
    like(in_child(10, sub { Sereal::Path::Iterator->new($packet)->info() }),
         qr/^error: .*Offset 1 points past/, "info() rejects it");
    like(in_child(10, sub { Sereal::Path->new($packet)->values('$[*]') }),
         qr/^error: .*Offset 1 points past/, "Sereal::Path->values rejects it");
    # ARRAYREF_1 [ ALIAS to itself ]
    like(in_child(10, sub { my $it = Sereal::Path::Iterator->new($header . "\x41\x2e\x02"); $it->step_in(); $it->info() }),
         qr/^error: .*Offset 2 points past/, "info() rejects an ALIAS to itself");
};

subtest "decode() of references to untracked items" => sub {
    plan skip_all => "needs fork" unless $Config{d_fork};
    # HASHREF_1 { k => ALIAS/REFP to the enclosing, untracked hash }: this
    # was read again and again until the recursion limit, and overflowed
    # the C stack on the way there
    for my $tag ("\x2e", "\x29") {
        my $packet = $header . "\x51\x61k" . $tag . "\x01";
        is(in_child(30, sub { Sereal::Path::Iterator->new($packet)->decode() }), "ok",
           sprintf("reference (tag %02x) to the enclosing item is decoded", ord $tag));
    }

    # REFN ARRAY [ [], ( ARRAYREF_2 [ REFP to the previous level x 2 ] ) x 40 ]:
    # every level was read twice per reference to it, 2**40 times in all
    my $levels = 40;
    my $body = "\x28\x2b" . chr($levels + 1);
    my $prev = length($body) + 1;
    $body .= "\x40";
    for (1 .. $levels) {
        my $ofs = length($body) + 1;
        $body .= "\x42" . ("\x29" . varint($prev)) x 2;
        $prev = $ofs;
    }
    my $got;
    is(in_child(30, sub { $got = Sereal::Path::Iterator->new($header . $body)->decode();
                          die "wrong result\n" unless ref $got eq 'ARRAY' && @$got == $levels + 1
                                                  && ref ${ $got->[-1][0] } eq 'ARRAY' }),
       "ok", "a chain of references to untracked items is decoded once per item");
};

subtest "protocol v1 offsets into the header" => sub {
    # v1 offsets count from the start of the packet, so offsets 1 to 5 point
    # into the header; after decompression that is memory that was never
    # written. HASHREF_1 { COPY of offset 1 => 1 }
    my $body = "\x51\x2f\x01\x01";
    my $snappy = "=srl\x11\x00" . chr(length $body) . chr((length($body) - 1) << 2) . $body;
    for ([ "raw", "=srl\x01\x00" . $body ], [ "snappy", $snappy ]) {
        my ($name, $packet) = @$_;
        my $iter = Sereal::Path::Iterator->new($packet);
        $iter->step_in();
        ok(!eval { $iter->hash_key(); 1 }, "$name: COPY of a header byte is rejected");
        like($@, qr/points before the document body/, "$name: error message");
    }
};

subtest "end of input after a reference tag" => sub {
    # REFN as the last byte
    ok(!iter_ok("\x28", sub { $_[0]->step_in() }), "REFN at the end of input is rejected");
    like($@, qr/Premature end of document/, "with an end of document error");

    # OBJECT "a" and nothing after the class name
    ok(!iter_ok("\x2c\x61a", sub { $_[0]->step_in() }), "OBJECT without a value is rejected");
    like($@, qr/Premature end of document/, "with an end of document error");
};

done_testing;
