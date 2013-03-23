use strict;
use warnings;
use Getopt::Long;

my @files= qw(
    Decoder/lib/Sereal/Decoder.pm
    Encoder/lib/Sereal/Encoder.pm
    Sereal/lib/Sereal.pm
    Sereal/Makefile.PL
);
my %special= (
    'Sereal/lib/Sereal.pm' => '%.3f'
);

my $to= shift @ARGV;

die "usage: $0 VERSION" if !$to;

foreach my $file (@files) {
    my $to_str= sprintf $special{$file}||"%.2f", $to;
    open my $in, "<", $file
        or die "Failed to open for read '$file': $!";
    unlink $file;
    open my $out, ">", $file
        or die "Failed to open for write '$file': $!";
    while (<$in>) {
        s/\$VERSION = '\d.\d+'/\$VERSION = '$to_str'/g;
        if ($special{$file}) {
            s/(Sereal::(En|De)coder) (\d+.\d+)/$1 $to/g;
        }
        print $out $_;
    }
}
