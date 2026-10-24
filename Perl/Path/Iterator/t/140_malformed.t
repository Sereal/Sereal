#!perl
use strict;
use warnings;
use Test::More;
use Sereal::Path::Iterator;

my $header = "=\xF3rl" . chr(6) . chr(0);

# run $code on an iterator over the packet; returns false with the error
# in $@ if it croaks
sub iter_ok {
    my ($body, $code) = @_;
    my $iter = Sereal::Path::Iterator->new($header . $body);
    return eval { $code->($iter); 1 };
}

subtest "end of input after a reference tag" => sub {
    # REFN as the last byte
    ok(!iter_ok("\x28", sub { $_[0]->step_in() }), "REFN at the end of input is rejected");
    like($@, qr/Premature end of document/, "with an end of document error");

    # OBJECT "a" and nothing after the class name
    ok(!iter_ok("\x2c\x61a", sub { $_[0]->step_in() }), "OBJECT without a value is rejected");
    like($@, qr/Premature end of document/, "with an end of document error");
};

done_testing;
