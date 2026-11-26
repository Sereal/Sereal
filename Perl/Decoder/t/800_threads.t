#!perl
use strict;
use warnings;
use Test::More;

BEGIN {
    use Config;
    if ( !$Config{'useithreads'} ) {
        print("1..0 # SKIP Perl not compiled with 'useithreads'\n");
        exit(0);
    }
    elsif ( $] < 5.008007 ) {
        print("1..0 # SKIP Sereal not thread safe on Perls before 5.8.7\n");
        exit(0);
    }
}

plan tests => 5;
use threads;
use Thread::Semaphore;

# Test 1-2: Parent thread loads Sereal::Decoder, spawns child t2, and exits
# before t2 runs option parsing (verifies MY_CXT_CLONE isolates child MY_CXT).
my $sample_packet = "=\xF3rl\x04\x00\x51\x61a\x01"; # { a => 1 }
SCOPE: {
    my $parent_exited = Thread::Semaphore->new(0);
    my $t1 = threads->create(sub {
        require Sereal::Decoder;
        my $t2 = threads->create(sub {
            $parent_exited->down();
            my $dec = Sereal::Decoder->new({ refuse_objects => 1, validate_utf8 => 1 });
            my $d1  = $dec->decode($sample_packet);
            my $d2  = Sereal::Decoder::decode_sereal(
                $sample_packet,
                { refuse_objects => 1, validate_utf8 => 1 }
            );
            return ( ref($d1) eq 'HASH' && $d1->{a} == 1
                  && ref($d2) eq 'HASH' && $d2->{a} == 1 ) ? 1 : 0;
        });
        return $t2;
    });
    my $t2 = $t1->join();
    ok( defined($t2), "Parent thread t1 exited cleanly" );
    $parent_exited->up();
    is( $t2->join(), 1, "Child thread t2 parsed options safely after parent thread t1 exited" );
}

require Sereal::Decoder;

sub foo { }
SCOPE: {
    my $dec = Sereal::Decoder->new;

    my $thr = threads->new( \&foo );
    $thr->join;
    ok( defined($dec), "Original thread decoder survives child join" );
}

# Test 4-5: Concurrent child threads creating OO and functional decoders with options
SCOPE: {
    my @threads = map {
        threads->create(sub {
            my $dec = Sereal::Decoder->new({ refuse_objects => 1, set_readonly => 1 });
            my $d1  = $dec->decode($sample_packet);
            my $d2  = Sereal::Decoder::decode_sereal(
                $sample_packet,
                { refuse_objects => 1 }
            );
            return ( ref($d1) eq 'HASH' && $d1->{a} == 1
                  && ref($d2) eq 'HASH' && $d2->{a} == 1 ) ? 1 : 0;
        });
    } 1 .. 2;
    for my $i ( 0 .. $#threads ) {
        is( $threads[$i]->join(), 1, "Concurrent child thread " . ($i + 1) . " decoded with options" );
    }
}
