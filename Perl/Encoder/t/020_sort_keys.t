#!perl
use strict;
use warnings;
use File::Spec;
use lib File::Spec->catdir(qw(t lib));

BEGIN {
    lib->import('lib')
        if !-d 't';
}
use Sereal::TestSet;
use Sereal::Encoder qw(encode_sereal);
use List::Util qw(shuffle);
use Test::More;

BEGIN {
    if (!eval "use Hash::Util 'num_buckets'; 1") {
        my %hash= ( test => 1);
        my $scalar= scalar(%hash);
        if ($scalar=~m!/!) {
            eval "sub num_buckets(\\%) { (split( m!/!, scalar %{\$_[0]}))[-1] } 1"
        } else {
            plan skip_all => "Hash::Util not installed, this is perl $], scalar(%h)='$scalar'";
        }
    }
}

my %test= ( foo => 1, bar => 2);
my $num_buckets= num_buckets(%test);
if ($num_buckets < 8) {
    plan skip_all => "num_buckets() not working as expected.";
}

# This logic needs to be revisted...
#
# Try and find 15 hash collisions in "A".."Z"
# we will use the colliding keys to produce hashes
# with the same contents, but with different key orders,
# which we will use to test the "sort_keys" logic.

my $max= 15;
my %hash;
my ( %i, %j );
keys %i= $max;
keys %j= $max;

LOOP:
for my $x ( "A" .. "Z" ) {
    for my $y ( chr( ord($x) + 1 ) .. "Z" ) {
        %i= ();
        %j= ();
        $i{$x}= 1; $i{$y}= 1;
        $j{$y}= 1; $j{$x}= 1;
        if ( "@{[keys %i]}" ne "@{[keys %j]}" ) {    # collission?
            $hash{$x}= 1;
            last LOOP if keys %hash == $max;
            $hash{$y}= 1;
            last LOOP if keys %hash == $max;
        }
    }
}

my %copy= %hash;
my $copy_keys= join "", keys %copy;

my %bigger= %hash;
keys(%bigger)= 1024;

my %shuffled;
$shuffled{$_}= $hash{$_} for shuffle keys %hash;

my %encoded;
my %encoded_unsorted;
for ( \%hash, \%copy, \%bigger, \%shuffled ) {
    my $keys= join "", keys %$_;
    $encoded{$keys}          ||= encode_sereal( $_, { sort_keys => 1 } );
    $encoded_unsorted{$keys} ||= encode_sereal($_);
}
my @keys= keys %encoded;

if ( @keys > 1 ) {
    plan tests => 2 * ( ( @keys * ( @keys - 1 ) ) / 2 );
}
else {
    plan skip_all => "Could not generate test hashes";
}

foreach my $x ( 0 .. $#keys ) {
    foreach my $y ( $x + 1 .. $#keys ) {
        is(
            $encoded{ $keys[$x] }, $encoded{ $keys[$y] },
            "$keys[$x] vs $keys[$y] (same: sort_keys)"
        );
        SKIP: {
            skip "test causes random false failures", 1;
            isnt(
                $encoded_unsorted{ $keys[$x] }, $encoded_unsorted{ $keys[$y] },
                "$keys[$x] vs $keys[$y] (different: no sort_keys)"
            );
        }
    }
}
