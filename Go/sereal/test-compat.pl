#!/usr/bin/env perl

use strict;
use warnings;

use blib "../../Perl/Decoder/blib/";
use blib "../../Perl/Encoder/blib/";
use lib "../../Perl/shared/t/lib/";

use Cwd;
use Sereal::Decoder qw(decode_sereal);
use Sereal::Encoder qw(encode_sereal);
use Test::More;
use Data::Dumper;

$Data::Dumper::Indent = 0;
$Data::Dumper::Sortkeys = 1;

sub slurp {
    my $n = shift;
    open (my $fh, "<", $n) or die "can't open $n: $!\n";
    local $/ = undef;
    my $d = <$fh>;
    return $d;
}

# Some parts of the Sereal specification (like aliases) are deliberately not
# implemented in Go. As a result a set of tests checking omitted functionality
# will fail. To reduce a level of false negatives here we list names of all
# tests that are supposed to fail and skip them later.
#
# Multiple original tests share the same name making the following list not
# 100% reliable and accurate. To mitigate it we also maintain a counter holding
# a total number of tests to be skipped.
#
my $skip_total = 216;
my %skip = map { $_ => 1 } (
    'array ref to aliases blessed array',
    'array ref to aliases complex hash',
    'array ref to aliases deep nest',
    'array ref to aliases double ref to foo',
    'array ref to aliases empty hash',
    'array ref to aliases empty string',
    'array ref to aliases eng-ref',
    'array ref to aliases escaped string',
    'array ref to aliases float',
    'array ref to aliases integer: -1',
    'array ref to aliases integer: -127',
    'array ref to aliases integer: -128',
    'array ref to aliases integer: -129',
    'array ref to aliases integer: -2147483647',
    'array ref to aliases integer: -2147483648',
    'array ref to aliases integer: -2147483649',
    'array ref to aliases integer: -9223372036854775807',
    'array ref to aliases integer: -9223372036854775808',
    'array ref to aliases integer: 0',
    'array ref to aliases integer: 0e0',
    'array ref to aliases integer: 0e1',
    'array ref to aliases integer: 1',
    'array ref to aliases integer: 11285066962739960988',
    'array ref to aliases integer: 126',
    'array ref to aliases integer: 127',
    'array ref to aliases integer: 128',
    'array ref to aliases integer: 129',
    'array ref to aliases integer: 18446744073709551614',
    'array ref to aliases integer: 18446744073709551615',
    'array ref to aliases integer: 1e3',
    'array ref to aliases integer: 2147483646',
    'array ref to aliases integer: 2147483647',
    'array ref to aliases integer: 2147483648',
    'array ref to aliases integer: 2147483649',
    'array ref to aliases integer: 254',
    'array ref to aliases integer: 255',
    'array ref to aliases integer: 256',
    'array ref to aliases integer: 257',
    'array ref to aliases integer: 3735928559',
    'array ref to aliases integer: 42',
    'array ref to aliases integer: 4294967295',
    'array ref to aliases integer: 9223372036854775806',
    'array ref to aliases integer: 9223372036854775807',
    'array ref to aliases integer: 9223372036854775808',
    'array ref to aliases largeish int',
    'array ref to aliases largeish negative int',
    'array ref to aliases long ascii string',
    'array ref to aliases long latin1 string',
    'array ref to aliases long utf8 string with only ascii',
    'array ref to aliases long utf8 string with only latin1 subset',
    'array ref to aliases long utf8 string',
    'array ref to aliases more complex',
    'array ref to aliases more escapes',
    'array ref to aliases negative big num',
    'array ref to aliases negative float',
    'array ref to aliases negative small float',
    'array ref to aliases float 0.1',
    'array ref to aliases float 0.2',
    'array ref to aliases float 0.3',
    'array ref to aliases float 0.4',
    'array ref to aliases float 0.5',
    'array ref to aliases float 0.6',
    'array ref to aliases float 0.7',
    'array ref to aliases float 0.8',
    'array ref to aliases float 0.9',
    'array ref to aliases small float 0.41',
    'array ref to aliases negative small float -0.13',
    'array ref to aliases complex hash with float',
    'array ref to aliases more complex with float',
    'array ref to aliases nested simple',
    'array ref to aliases positive big num',
    'array ref to aliases quote keys',
    'array ref to aliases ref to foo',
    'array ref to aliases ref to undef',
    'array ref to aliases reffy hash',
    'array ref to aliases refy array',
    'array ref to aliases regexp with inline modifiers',
    'array ref to aliases regexp with modifiers',
    'array ref to aliases short ascii string',
    'array ref to aliases short latin1 string',
    'array ref to aliases short utf8 string',
    'array ref to aliases simple array',
    'array ref to aliases simple hash',
    'array ref to aliases simple regexp',
    'array ref to aliases small float',
    'array ref to aliases small int',
    'array ref to aliases small negative int',
    'array ref to aliases undef value',
    'array ref to aliases undef',
    'array ref to aliases utf8 string',
    'array ref to aliases var strings',
    'array ref to aliases troublesome num/strs',
    "array ref to aliases  troublesome num/strs '    1    '",
    "array ref to aliases  troublesome num/strs '0.0'",
    "array ref to aliases  troublesome num/strs '00000.0000'",
    "array ref to aliases  troublesome num/strs '0.0.0.0'",
    "array ref to aliases  troublesome num/strs '.0'",
    "array ref to aliases  troublesome num/strs '    .0'",
    "array ref to aliases  troublesome num/strs ' 22'",
    "array ref to aliases  troublesome num/strs '01'",
    "array ref to aliases  troublesome num/strs '01.1'",
    "array ref to aliases  troublesome num/strs '   0   '",
    "array ref to aliases  troublesome num/strs '.0'",
    "array ref to aliases  troublesome num/strs '0.001'",
    "array ref to aliases  troublesome num/strs '.1'",
    "array ref to aliases  troublesome num/strs '  .1'",
    "array ref to aliases  troublesome num/strs '.2'",
    "array ref to aliases  troublesome num/strs '00'",
    "array ref to aliases  troublesome num/strs '.00'",
    "array ref to aliases  troublesome num/strs '0 but true'",
    "array ref to aliases  troublesome num/strs '0E0'",
    "array ref to aliases largeish negative int -302001",
    "array ref to aliases largeish negative int -1234567",
    "array ref to aliases largeish negative int -12345678",
    "array ref to aliases long ascii string 'a' x 9999",
    "array ref to aliases long ascii string 'a' x 10000",
    "array ref to aliases long ascii string 'a' x 10001",
    "array ref to aliases long ascii string 'a' x 1023",
    "array ref to aliases long ascii string 'a' x 1024",
    "array ref to aliases long ascii string 'a' x 1025",
    "array ref to aliases long ascii string 'a' x 8191",
    "array ref to aliases long ascii string 'a' x 8192",
    "array ref to aliases long ascii string 'a' x 8193",
    "array ref to aliases long ascii string 'ab' x 9999",
    "array ref to aliases long ascii string 'ab' x 10000",
    "array ref to aliases long ascii string 'ab' x 10001",
    "array ref to aliases long ascii string 'ab' x 1023",
    "array ref to aliases long ascii string 'ab' x 1024",
    "array ref to aliases long ascii string 'ab' x 1025",
    "array ref to aliases long ascii string 'ab' x 8191",
    "array ref to aliases long ascii string 'ab' x 8192",
    "array ref to aliases long ascii string 'ab' x 8193",
    "array ref to aliases long ascii string 'abc' x 9999",
    "array ref to aliases long ascii string 'abc' x 10000",
    "array ref to aliases long ascii string 'abc' x 10001",
    "array ref to aliases long ascii string 'abc' x 1023",
    "array ref to aliases long ascii string 'abc' x 1024",
    "array ref to aliases long ascii string 'abc' x 1025",
    "array ref to aliases long ascii string 'abc' x 8191",
    "array ref to aliases long ascii string 'abc' x 8192",
    "array ref to aliases long ascii string 'abc' x 8193",
    "array ref to aliases long ascii string 'abcd' x 9999",
    "array ref to aliases long ascii string 'abcd' x 10000",
    "array ref to aliases long ascii string 'abcd' x 10001",
    "array ref to aliases long ascii string 'abcd' x 1023",
    "array ref to aliases long ascii string 'abcd' x 1024",
    "array ref to aliases long ascii string 'abcd' x 1025",
    "array ref to aliases long ascii string 'abcd' x 8191",
    "array ref to aliases long ascii string 'abcd' x 8192",
    "array ref to aliases long ascii string 'abcd' x 8193",
    'array ref to scalar refs to same largeish negative int -302001',
    'array ref to scalar refs to same largeish negative int -1234567',
    'array ref to scalar refs to same largeish negative int -12345678',
    'array ref to scalar refs to same float',
    'array ref to scalar refs to same integer: -1',
    'array ref to scalar refs to same integer: -127',
    'array ref to scalar refs to same integer: -128',
    'array ref to scalar refs to same integer: -129',
    'array ref to scalar refs to same integer: -2147483647',
    'array ref to scalar refs to same integer: -2147483648',
    'array ref to scalar refs to same integer: -2147483649',
    'array ref to scalar refs to same integer: -9223372036854775807',
    'array ref to scalar refs to same integer: -9223372036854775808',
    'array ref to scalar refs to same integer: 0',
    'array ref to scalar refs to same integer: 1',
    'array ref to scalar refs to same integer: 11285066962739960988',
    'array ref to scalar refs to same integer: 126',
    'array ref to scalar refs to same integer: 127',
    'array ref to scalar refs to same integer: 128',
    'array ref to scalar refs to same integer: 129',
    'array ref to scalar refs to same integer: 18446744073709551614',
    'array ref to scalar refs to same integer: 18446744073709551615',
    'array ref to scalar refs to same integer: 2147483646',
    'array ref to scalar refs to same integer: 2147483647',
    'array ref to scalar refs to same integer: 2147483648',
    'array ref to scalar refs to same integer: 2147483649',
    'array ref to scalar refs to same integer: 254',
    'array ref to scalar refs to same integer: 255',
    'array ref to scalar refs to same integer: 256',
    'array ref to scalar refs to same integer: 257',
    'array ref to scalar refs to same integer: 3735928559',
    'array ref to scalar refs to same integer: 42',
    'array ref to scalar refs to same integer: 4294967295',
    'array ref to scalar refs to same integer: 9223372036854775806',
    'array ref to scalar refs to same integer: 9223372036854775807',
    'array ref to scalar refs to same integer: 9223372036854775808',
    'array ref to scalar refs to same integer: 0e0',
    'array ref to scalar refs to same integer: 0e1',
    'array ref to scalar refs to same integer: 1e3',
    'array ref to scalar refs to same largeish int',
    'array ref to scalar refs to same largeish negative int',
    'array ref to scalar refs to same negative big num',
    'array ref to scalar refs to same float 0.1',
    'array ref to scalar refs to same float 0.2',
    'array ref to scalar refs to same float 0.3',
    'array ref to scalar refs to same float 0.4',
    'array ref to scalar refs to same float 0.5',
    'array ref to scalar refs to same float 0.6',
    'array ref to scalar refs to same float 0.7',
    'array ref to scalar refs to same float 0.8',
    'array ref to scalar refs to same float 0.9',
    'array ref to scalar refs to same small float 0.41',
    'array ref to scalar refs to same negative small float -0.13',
    'array ref to scalar refs to same negative float',
    'array ref to scalar refs to same negative small float',
    'array ref to scalar refs to same positive big num',
    'array ref to scalar refs to same small float',
    'array ref to scalar refs to same small int',
    'array ref to scalar refs to same small negative int',
    'repeated substructure (REFP): scalar ref',
    'scalar cross',
    'weak scalar cross',
    'weak thing copy (requires PAD)',
    'BlessedArrayCheck 1',
    'BlessedArrayCheck 2',
    'Scalar Cross Blessed Array',
);

my $skipped = 0;

for my $n (glob("test_dir/test_data_?????")) {

    (my $test_number = $n) =~ s/.*test_data_0*//;

    chomp(my $name = slurp(sprintf("test_dir/test_name_%05d", $test_number)));

    if ($skip{$name}) {
        SKIP: { skip "$name ($n) not implemented", 1; };
        $skipped++;
        next;
    }

    if (not -f "$n-go.out") {
        fail($name);
        diag("No Go test output for $n");
#        die;
        next;
    }

    my $testdata = slurp($n);
    my $p;
    eval {
        $p = decode_sereal($testdata);
        1;
    } or do {
        my $err = $@;
        fail($name);
        diag("Failed unpacking perl $n: $err");
        next;
    };
    
    $testdata = slurp("$n-go.out");
    my $g;

    eval {
        $g = decode_sereal($testdata);
        1;
    } or do {
        my $err = $@;
        fail($name);
        diag("Failed unpacking go $n: $err");
        next;
    };

    my $dg = Dumper($g);
    my $dp = Dumper($p);

    if (!ok($dg eq $dp, $name)) {
        diag("$n\nGot: $dg\nExp: $dp");
#        die;
        next;
    }
}

is($skipped, $skip_total, "skipped expected number of tests");

{
    my $cwd = getcwd();
    $cwd =~ s/sereal$//;
    chop $cwd if $cwd =~ m/\/$/;
    foreach my $class ("time.Time", "github.com/Sereal/Sereal/Go/sereal.StructWithTime", "_${cwd}/sereal.StructWithTime") {
        no strict 'refs';
        *{"${class}::THAW"} = sub { my ( $pkg, $srl, $val ) = @_; bless \$val, $pkg };
        *{"${class}::FREEZE"} = sub { ${$_[0]} };
    }

    my ($version) = map { m/VERSION_([0-9]+)$/; $1 } glob("test_freeze/VERSION_*");
    for my $n (glob("test_freeze/*-go.out")) {
        my $testdata = slurp($n);
        my ( $name ) = ( $n =~ m{([^/]+)-go\.out$} );
        my $g;

        eval {
            $g = decode_sereal($testdata);
            1;
        } or do {
            my $err = $@;
            fail($name);
            diag("Failed unpacking go $n: $err");
            next;
        };

        ( my $perl = $n ) =~ s{-go\.out$}{-perl.out};

        open my $fh, ">", $perl or die "Can't open $perl for writing: $!";
        print $fh encode_sereal($g, { freeze_callbacks => 1, protocol_version => $version }) or die "print($perl): $!";
        close $fh or die "close($perl): $!";

        pass($name);
    }
}

done_testing();

