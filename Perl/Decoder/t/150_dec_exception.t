#!perl
use strict;
use warnings;
use Sereal::Decoder qw(decode_sereal);
use Sereal::Decoder::Constants qw(:all);
use Data::Dumper;
use Test::More;
use File::Spec;

use lib File::Spec->catdir(qw(t lib));
BEGIN {
    lib->import('lib')
        if !-d 't';
}

use Sereal::TestSet qw(:all);

# These tests are a manual attempt at seeing the decoder blow up on
# bad input. This obviously shouldn't segfault and neither leak
# memory.

plan tests => 20;
my ($ok, $out, $err);

SCOPE: {
    check_fail($Header, qr/unexpected end of input/i, "Cannot decode just header");

    my $badheaderpacket = "srX".chr(SRL_PROTOCOL_VERSION) . chr(0) . integer(1);
    check_fail($badheaderpacket, qr/bad header/i, "Packet with invalid header blows up");

    my $bad_nested_packet = $Header . array(integer(1), 7777);
    check_fail($bad_nested_packet, qr/Sereal: Error/, "Random crap in packet");

    my $d = Sereal::Decoder->new({refuse_snappy => 1});
    # strictly speaking not entirely correct; also: +16 for the snappy flag isn't exactly API
    my $h = SRL_MAGIC_STRING . chr(1+16) . chr(0) . chr(SRL_HDR_UNDEF);
    ok(!eval {$d->decode($h)} && $@ =~ /Snappy/i);
}

pass("Alive"); # done


sub check_fail {
    my ($data, $err_like, $name) = @_;

    my ($ok, $out, $err);
    ($ok, $out, $err) = dec_func($data);
    expect_fail($ok, $out, $err, $err_like, $name . "(func)");
    ($ok, $out, $err) = dec_obj($data);
    expect_fail($ok, $out, $err, $err_like, $name . "(OO)");
}

sub expect_fail {
    my ($ok, $out, $err, $err_like, $name) = @_;
    ok(!$ok, "$name, got exception");
    ok(!defined($out), "$name, got no output");
    if (defined $err_like) {
        like($err, $err_like, "$name, matched exception");
    }
    else {
        diag($err);
    }
}

sub dec_func {
    my ($ok, $out);
    $ok = eval {
        $out = decode_sereal($_[0]);
        1
    };
    my $err = $@ || 'Zombie error';
    return($ok, $out, $err);
}


sub dec_obj {
    my ($ok, $out);
    my $obj = @_ > 1 ? shift : Sereal::Decoder->new;
    $ok = eval {
        $out = $obj->decode($_[0]);
        1
    };
    my $err = $@ || 'Zombie error';
    return($ok, $out, $err);
}

