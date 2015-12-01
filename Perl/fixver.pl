use strict;
use warnings;
use Getopt::Long;

my @files= qw(
    Decoder/lib/Sereal/Decoder.pm
    Encoder/lib/Sereal/Encoder.pm
    Decoder/lib/Sereal/Decoder/Constants.pm
    Encoder/lib/Sereal/Encoder/Constants.pm
    Sereal/lib/Sereal.pm
    Sereal/Makefile.PL
);

my $to= shift @ARGV;

die "usage: MAJOR.MINOR_(DEV) REASON" if !$to;
my $reason= join " ", @ARGV;
die "usage: $0 VERSION REASON" if !$reason;

my ($major,$minor,$dev)= split/[_.]/, $to;

$to= $dev ? sprintf "%d.%03d_%03d", $major, $minor, $dev
          : sprintf "%d.%03d",      $major, $minor
;

my %special= (
    'Sereal/lib/Sereal.pm' => $to
);

foreach my $file (@files) {
    my $to_str= $special{$file}||$to;
    open my $in, "<", $file
        or die "Failed to open for read '$file': $!";
    unlink $file;
    open my $out, ">", $file
        or die "Failed to open for write '$file': $!";
    while (<$in>) {
        s/\$VERSION = '[^']+'/\$VERSION = '$to_str'/g;
        if ($special{$file}) {
            s/(Sereal::(En|De)coder) (\d+\.\d+(?:_\d+)?)/$1 $to/g;
        }
        print $out $_;
    }
}

print <<"EOF_TEXT";

./make_all &&
git commit -a -m'Release v$to - $reason' &&
git tag Sereal-Decoder-$to -m'Release Sereal::Decoder version $to ($reason)' &&
git tag Sereal-Encoder-$to -m'Release Sereal::Encoder version $to ($reason)' &&
git tag Sereal-$to -m'Sereal v$to - Update encoder ($reason)' &&
git push && git push --tags

cpan-upload-http -verbose Encoder/Sereal-Encoder-$to.tar.gz Decoder/Sereal-Decoder-$to.tar.gz Sereal/Sereal-$to.tar.gz

EOF_TEXT

