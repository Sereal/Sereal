#!/usr/bin/env perl
use strict;
use warnings;

use File::Spec;
use lib File::Spec->catdir(qw(t lib));

BEGIN {
    lib->import('lib')
      if !-d 't';
}
use Test::More;
use Sereal::TestSet qw(:all);
use Sereal::Decoder qw(decode_sereal);
use Data::Dumper;

if ( !have_encoder_and_decoder() ) {
    plan skip_all => 'Did not find right version of encoder';
}

{

    package Rot13;

    sub rot13 {
        my ($d) = @_;
        $d =~ tr/a-zA-Z/nopqrstuvwxyzabcdefghijklmNOPQRSTUVWXYZABCDEFGHIJKLM/;
        return $d;
    }

    sub new {
        my ( $class, $secret ) = @_;
        bless \$secret, $class;
    }

    sub FREEZE {
        my ( $self, $serializer ) = @_;
        return rot13($$self);
    }

    sub THAW {
        my ( $class, $serializer, $data ) = @_;
        return rot13($data);
    }
}

my $encoder_freeze = Sereal::Encoder->new( { freeze_callbacks => 1 } );

my $secret = "SECRET";

ok(
    $secret,
    decode_sereal($encoder_freeze->encode(Rot13->new($secret))),
);

ok(
    { secret => $secret },
    decode_sereal($encoder_freeze->encode({ secret => Rot13->new($secret) })),
);

done_testing;
