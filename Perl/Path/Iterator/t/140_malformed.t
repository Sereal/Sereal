#!perl
use strict;
use warnings;
use Config;
use POSIX ();
use Test::More;
use Sereal::Path::Iterator;
use Sereal::Path;

my $header = "=\xF3rl" . chr(6) . chr(0);

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

subtest "end of input after a reference tag" => sub {
    # REFN as the last byte
    ok(!iter_ok("\x28", sub { $_[0]->step_in() }), "REFN at the end of input is rejected");
    like($@, qr/Premature end of document/, "with an end of document error");

    # OBJECT "a" and nothing after the class name
    ok(!iter_ok("\x2c\x61a", sub { $_[0]->step_in() }), "OBJECT without a value is rejected");
    like($@, qr/Premature end of document/, "with an end of document error");
};

done_testing;
