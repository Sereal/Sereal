#!perl

use strict;
use warnings;

use Data::Dumper;
use Sereal::Path;

my $file = $ARGV[0] // '../../sample_data/output3';
my $encoded = do {
    $/ = undef;
    open(my $fh, '<', $file) or die $!;
    my $data = <$fh>;
    $data;
};

my $p = Sereal::Path->new($encoded);
$p->traverse('$[*].__uuid__');
exit 1;
