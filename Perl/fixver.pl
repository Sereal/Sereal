use strict;
use warnings;
use Getopt::Long;

my @files= qw(
    Decoder/lib/Sereal/Decoder.pm
    Encoder/lib/Sereal/Encoder.pm
    Decoder/lib/Sereal/Decoder/Constants.pm
    Encoder/lib/Sereal/Encoder/Constants.pm
    Encoder/Makefile.PL
    Sereal/lib/Sereal.pm
    Sereal/Makefile.PL
);

my @changes_files= qw(
    Decoder/Changes
    Encoder/Changes
    Sereal/Changes
);

my $to= shift @ARGV;

die "usage: MAJOR.MINOR_(DEV) REASON" if !$to;
my $reason= join " ", @ARGV;
die "usage: $0 VERSION REASON" if !$reason;

sub shell_quote {
    my ($value) = @_;
    $value =~ s/'/'\\''/g;
    return "'$value'";
}

my ($major,$minor,$dev)= split/[_.]/, $to;

$to= $dev ? sprintf "%d.%03d_%03d", $major, $minor, $dev
          : sprintf "%d.%03d",      $major, $minor
;

foreach my $file (@changes_files) {
    open my $in, "<", $file
        or die "Failed to open for read '$file': $!";
    local $/;
    my $changes= <$in>;
    close $in;

    next if $changes =~ /^\Q$to\E\s*$/m;

    my $entry= "$to\n    * $reason\n\n";
    if ($changes =~ /^\d+\.\d{3}(?:_\d{3})?\s*$/m) {
        substr $changes, $-[0], 0, $entry;
    } else {
        $changes .= "\n" unless $changes =~ /\n\z/;
        $changes .= $entry;
    }

    open my $out, ">", $file
        or die "Failed to open for write '$file': $!";
    print $out $changes;
    close $out;
}

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
        s/(\$VERSION\s*=\s*)'[^']+'/$1'$to_str'/g;
        if ($special{$file}) {
            s/(Sereal::(En|De)coder) (\d+\.\d+(?:_\d+)?)/$1 $to/g;
        }
        print $out $_;
    }
}

my $subject = shell_quote("Release v$to - $reason");
my $decoder_tag_message = shell_quote("Release Sereal::Decoder version $to ($reason)");
my $encoder_tag_message = shell_quote("Release Sereal::Encoder version $to ($reason)");
my $wrapper_tag_message = shell_quote("Release Sereal version $to ($reason)");

print <<"EOF_TEXT";

./clean_make_all &&
perl author_tools/encoder_compat_matrix.pl &&
git-compose-commit --subject $subject -a &&
git tag Sereal-Decoder-$to -m$decoder_tag_message &&
git tag Sereal-Encoder-$to -m$encoder_tag_message &&
git tag Sereal-$to -m$wrapper_tag_message &&
git push && git push --tags &&
cpan-upload-http -verbose Encoder/Sereal-Encoder-$to.tar.gz Decoder/Sereal-Decoder-$to.tar.gz Sereal/Sereal-$to.tar.gz

EOF_TEXT
