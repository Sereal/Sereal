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

__END__

for d in Encoder/ Decoder/; do pushd $d; perl Makefile.PL; make test; make distcheck; make dist; popd; done; pushd Sereal/; PERL5OPT="-Mblib=../Encoder/ -Mblib=../Decoder/" perl Makefile.PL; PERL5OPT="-Mblib=../Encoder/ -Mblib=../Decoder/" make test; make distcheck; make dist; popd;
cpan-upload-http -verbose Encoder/Sereal-Encoder-0.33.tar.gz Decoder/Sereal-Decoder-0.33.tar.gz Sereal/Sereal-0.330.tar.gz
git commit -a -m'Release v0.33 to fix issue #27 - Issue with weakref'
git tag Sereal-Decoder-0.33 -m'Release Sereal::Decoder version 0.33 (encoder release sync)'
git tag Sereal-Encoder-0.33 -m'Release Sereal::Encoder version 0.33 (fix weakref problem)'
git tag Sereal-0.330 -m'Sereal v0.330 - Update encoder (fix weakref problem)'
git push
git push --tags
