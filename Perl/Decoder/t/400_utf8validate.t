#!perl

use strict;
use warnings;
use Data::Dumper;
use Test::More;
use File::Spec;

use lib File::Spec->catdir(qw(t lib));
BEGIN {
  lib->import('lib')
    if !-d 't';
}
use Sereal::TestSet;

use Sereal::Decoder qw(decode_sereal);
no warnings 'utf8';
my @valid_utf8 = (
    [ latin1 => "=srl\x01\x00'\x06Au feu" => 'Au feu' ],
    [ utf8   => "=srl\x01\x00'\x08\xc3\x80 l'eau" => "\xC0 l'eau" ],
    [ bom    => "=srl\x01\x00'\x06\xEF\xBB\xBFfoo" => "\x{FEFF}foo" ],
    # Invalid code points that are nonetheless valid UTF8 :
    # FFFE is a non-character
    [ fffe   => "=srl\x01\x00'\x03\xEF\xBF\xBE" => "\x{FFFE}" ],
    # This is binary, not utf8, so must not throw an error
    [ ffpadded => "=srl\x01\x00&\x04\xFF\xFF\xFF\xFF" => "\xFF\xFF\xFF\xFF" ],
);

my @invalid_utf8 = (
    # Only FF bytes
    [ ffpadded => "=srl\x01\x00'\x04\xFF\xFF\xFF\xFF" ],
    # Overlong encoding F0 82 82 AC for U+20AC
    [ overlong => "=srl\x01\x00'\x04\xF0\x82\x82\xAC" ],
    # Not enough contination bytes
    [ continuation => "=srl\x01\x00'\x01\xC0" ],
);

plan tests => 2 * @valid_utf8 + 2 * @invalid_utf8;

for my $test (@valid_utf8) {
    my ($name, $exp, $expected) = @$test;
    my $out;
    my $ok = eval { decode_sereal($exp, { validate_utf8 => 1 }, $out); 1 };
    my $err = $@ || 'Zombie error';
    ok($ok,"$name: did not die")
        or do {
            diag $err;
            diag "input=", Data::Dumper::qquote($exp);
            next;
        };
    is($out, $expected, "$name: correctly decoded");
}

for my $test (@invalid_utf8) {
    my ($name, $exp) = @$test;
    my $out;
    my $ok = eval { decode_sereal($exp, undef, $out); 1 };
    my $err = $@ || 'Zombie error';
    ok($ok,"$name: did not die")
        or do {
            diag $err;
            diag "input=", Data::Dumper::qquote($exp);
            next;
        };
    $ok = eval { decode_sereal($exp, { validate_utf8 => 1 }, $out); 1 };
    $err = $@ || 'Zombie error';
    like($err, qr/Invalid UTF8 byte sequence/, "$name: die with a UTF8 error");
}
