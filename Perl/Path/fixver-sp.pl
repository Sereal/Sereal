use strict;
use warnings;
use Getopt::Long;

my @files= qw(
    Iterator/lib/Sereal/Path/Iterator.pm
    Tie/lib/Sereal/Path/Tie.pm
    lib/Sereal/Path.pm
    lib/Sereal/Path/Constants.pm
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
    open my $in, "<", $file
        or die "Failed to open for read '$file': $!";
    unlink $file;
    open my $out, ">", $file
        or die "Failed to open for write '$file': $!";
    while (<$in>) {
        s/\$VERSION\s*=\s*'[^']+'/\$VERSION = '$to'/g;
        print $out $_;
    }
}

print <<"EOF_TEXT";

git clean -fdx && perl Makefile.PL && make && make manifest && make disttest && make dist &&
git commit -a -m'Release Sereal::Path v$to - $reason' &&
git tag Sereal-Path-$to -m'Release Sereal::Path version $to ($reason)' &&
git push && git push --tags

cpan-upload -verbose Sereal-Path-$to.tar.gz

EOF_TEXT

