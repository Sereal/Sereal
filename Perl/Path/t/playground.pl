#!perl

use strict;
use warnings;

use Data::Dumper;
use Sereal::Path;

my $file = $ARGV[0] // '../../sample_data/output1';
my $encoded = do {
    $/ = undef;
    open(my $fh, '<', $file) or die $!;
    my $data = <$fh>;
    $data;
};

my $p = Sereal::Path->new($encoded);
#$p->traverse('$[*].__uuid__,__handler_epoch__,__dc_name__');
$p->traverse('$.*.\'__uuid__\'');
print Dumper $p->results;
$p->traverse('$.*.\'__dc_name__\'');
my $result = $p->results;
print Dumper $result;
exit 1;
