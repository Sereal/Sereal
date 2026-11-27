#!perl
use strict;
use warnings;
use Test::More;

BEGIN {
    use Config;
    if ( !$Config{'useithreads'} and !$Config{'usethreads'} ) {
        print("1..0 # SKIP Perl not compiled with 'useithreads'\n");
        exit(0);
    }
    elsif ( $] < 5.008007 ) {
        print("1..0 # SKIP Sereal not thread safe on Perls before 5.8.7\n");
        exit(0);
    }
}
use File::Spec;
use lib File::Spec->catdir(qw(t lib));

BEGIN {
    lib->import('lib')
        if !-d 't';
}

plan tests => 5;
use threads;
use Thread::Semaphore;
use threads::shared;

# Test 1-2: Parent thread loads Sereal::Encoder, spawns child t2, and exits
# before t2 runs option parsing (verifies MY_CXT_CLONE isolates child MY_CXT).
SCOPE: {
    my $parent_exited = Thread::Semaphore->new(0);
    my $t1            = threads->create(
        sub {
            require Sereal::Encoder;
            my $t2 = threads->create(
                sub {
                    $parent_exited->down();
                    my $enc = Sereal::Encoder->new( { canonical => 1, sort_keys => 1 } );
                    my $b1  = $enc->encode( { b => 2, a => 1 } );
                    my $b2  = Sereal::Encoder::encode_sereal(
                        { b         => 2, a         => 1 },
                        { canonical => 1, sort_keys => 1 } );
                    return ( defined($b1) && defined($b2) && $b1 eq $b2 ) ? 1 : 0;
                } );
            return $t2;
        } );
    my $t2 = $t1->join();
    ok( defined($t2), "Parent thread t1 exited cleanly" );
    $parent_exited->up();
    is( $t2->join(), 1, "Child thread t2 parsed options safely after parent thread t1 exited" );
}

require Sereal::Encoder;

sub foo { }
SCOPE: {
    my $dat = shared_clone( [undef] );
    my $enc = Sereal::Encoder->new;

    my $thr = threads->new( \&foo );
    $thr->join;
    my $encoded = $enc->encode($dat);
    ok( defined($encoded), "Original thread encoding succeeds after child join" );
}

# Test 4-5: Concurrent child threads creating OO and functional encoders with options
SCOPE: {
    my @threads = map {
        my $idx = $_;
        threads->create(
            sub {
                my $enc = Sereal::Encoder->new( { canonical => 1, sort_keys => 1 } );
                my $b1  = $enc->encode( { k => $idx, z => 1, a => 2 } );
                my $b2  = Sereal::Encoder::encode_sereal(
                    { k => $idx, z => 1, a => 2 },
                    { canonical => 1, sort_keys => 1 } );
                return ( defined($b1) && $b1 eq $b2 ) ? 1 : 0;
            } );
    } 1 .. 2;
    for my $i ( 0 .. $#threads ) {
        is(
            $threads[$i]->join(), 1,
            "Concurrent child thread " . ( $i + 1 ) . " encoded with options"
        );
    }
}
