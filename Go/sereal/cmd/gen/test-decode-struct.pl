#!/usr/bin/perl

#use blib "../../Perl/Decoder/blib/";
#use blib "../../Perl/Encoder/blib/";
#use lib "../../Perl/shared/t/lib/";

use Sereal::Encoder qw(encode_sereal);

my $obj1 = {
    ValueStr   => "string as string value which actually should be 32+ characters",
    ValueByte  => "string as binary value",
    ValueInt   => 10,
    ValueSlice => [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0 ],
    ValueHash  => {
        key1 => "unique value",
        key2 => "duplicate value",
        key3 => "deplicate value",
    }
};

my $obj2 = {
    ValueStr   => "another string as string value which actually should be 32+ characters",
    ValueByte  => "another string as binary value",
    ValueInt   => -10,
    ValueSlice => [ 18.0, 19.0, 20.0 ],
    ValueHash  => {
        key1 => "unique value",
        key2 => "duplicate value",
        key3 => "deplicate value",
    }
};

my $dir = $ARGV[0];
open(my $fh, '>', "$dir/test-decode-struct.srl") or die $!;
my ($version) = map { m/VERSION_([0-9]+)$/; $1 } glob("$dir/VERSION_*");
printf "$0 uses protocol_version %d\n", $version || 'undef';
print $fh encode_sereal([ $obj1, $obj2, $obj1 ], { dedupe_strings => 1, protocol_version => $version });
exit;
