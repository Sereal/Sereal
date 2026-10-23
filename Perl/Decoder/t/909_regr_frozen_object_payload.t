#!perl
use strict;
use warnings;
use File::Spec;
use lib File::Spec->catdir(qw(t lib));

BEGIN { lib->import('lib') if !-d 't' }

use Test::More tests => 2;
use Sereal::TestSet            qw(Header short_string);
use Sereal::Decoder::Constants qw(SRL_HDR_OBJECT_FREEZE);
use Sereal::Decoder            qw(decode_sereal);

my $packet =
    Header(6) . chr(SRL_HDR_OBJECT_FREEZE) . short_string('Sereal::Test::MalformedFreeze') . chr(1);

my $ok = eval { decode_sereal($packet); 1 };
ok( !$ok, 'frozen object with a scalar payload is rejected' );
like(
    $@,
    qr/OBJECT\(V\)_FREEZE must be followed by an array reference/,
    'malformed frozen object is rejected by a decoder exception',
);
