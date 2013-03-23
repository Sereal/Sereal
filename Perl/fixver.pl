use strict;
use warnings;
use Getopt::Long;

my @files= qw(
    Decoder/lib/Sereal/Decoder.pm
    Encoder/lib/Sereal/Encoder.pm
    Sereal/lib/Sereal.pm
    Sereal/Makefile.PL
);

my $to= shift @ARGV;

die "usage: $0 VERSION REASON" if !$to;
my $reason= join " ", @ARGV;
die "usage: $0 VERSION REASON" if !$reason;

$to= sprintf "%.2f", $to;
my $to_long= sprintf("%.3f", $to),
my %special= (
    'Sereal/lib/Sereal.pm' => $to_long
);

foreach my $file (@files) {
    my $to_str= $special{$file}||$to;
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

print <<"EOF_TEXT";

for d in Encoder/ Decoder/; do pushd \$d; perl Makefile.PL; make test; make manifest; make disttest; make dist; popd; done;

export PERL5OPT="-Mblib=/home/yorton/git_tree/Sereal/Perl/Encoder/ -Mblib=/home/yorton/git_tree/Sereal/Perl/Decoder/"; pushd Sereal; perl Makefile.PL; make test; make disttest; make dist; popd; unset PERL5OPT;

git commit -a -m'Release v$to - $reason'
git tag Sereal-Decoder-$to -m'Release Sereal::Decoder version $to ($reason)'
git tag Sereal-Encoder-$to -m'Release Sereal::Encoder version $to ($reason)'
git tag Sereal-$to_long -m'Sereal v$to_long - Update encoder ($reason)'
git push
git push --tags

cpan-upload-http -verbose Encoder/Sereal-Encoder-$to.tar.gz Decoder/Sereal-Decoder-$to.tar.gz Sereal/Sereal-$to_long.tar.gz
EOF_TEXT

