use 5.014;
use strict;
use warnings;
use Test::More;
use Cwd            qw(getcwd abs_path);
use File::Path     qw(make_path);
use File::Temp     qw(tempdir);
use File::Basename qw(dirname);
use FindBin;

my $script = abs_path("$FindBin::Bin/../../fixver.pl");
my $dir    = tempdir( CLEANUP => 1 );
my $cwd    = getcwd();
my @files  = qw(
    Decoder/lib/Sereal/Decoder.pm Encoder/lib/Sereal/Encoder.pm
    Decoder/lib/Sereal/Decoder/Constants.pm Encoder/lib/Sereal/Encoder/Constants.pm
    Encoder/Makefile.PL Sereal/lib/Sereal.pm Sereal/Makefile.PL
    Decoder/Changes Encoder/Changes Sereal/Changes
);

for my $file (@files) {
    make_path( dirname("$dir/$file") );
    open my $fh, '>', "$dir/$file" or die $!;
    print {$fh} $file =~ /Changes$/ ? "Unreleased\n    * Portable test fixes\n\n7.008\n    * Prior release\n" : "our \$VERSION = '7.008';\n";
    close $fh or die $!;
}
chdir $dir or die $!;
open my $fh, '-|', $^X, $script, '7.009', 'test release checks' or die $!;
my $commands = do { local $/; <$fh> };
ok( close($fh), 'generate release commands in an isolated fixture' );
chdir $cwd or die $!;
like(
    $commands,
    qr{perl cpantesters/fetch-reports\.pl --version '7\.008' --check &&\s*\./clean_make_all &&\s*perl author_tools/encoder_compat_matrix\.pl &&\s*git-compose-commit}s,
    'fresh report review gates the build, compatibility checks, and release commit'
);
like( $commands, qr/Release v7\.009/, 'release subject remains intact' );
like(
    $commands,
    qr{git-compose-commit[^\n]+&&\s*git push &&\s*perl author_tools/check_release_ci\.pl &&\s*git tag}s,
    'push release commit and require exact-commit CI before creating release tags'
);
open my $changed, '<', "$dir/Decoder/Changes" or die $!;
my $notes = do { local $/; <$changed> };
like(
    $notes, qr/^7\.009\n    \* test release checks\n    \* Portable test fixes/m,
    'promote unreleased notes into the new release'
);
unlike( $notes, qr/^Unreleased$/m, 'do not leave release fixes under an unreleased heading' );

open my $bad, '>', "$dir/Encoder/Changes" or die $!;
print {$bad} "7.007\n    * Different history\n";
close $bad or die $!;
chdir $dir or die $!;
open my $failed, '-|', $^X, $script, '7.009', 'test mismatched histories' or die $!;
{ local $/; my $ignored = <$failed> }
ok( !close($failed), 'mismatched distribution histories block release preparation' );
chdir $cwd or die $!;
open my $unchanged, '<', "$dir/Decoder/Changes" or die $!;
is( do { local $/; <$unchanged> }, $notes, 'failed preparation leaves other changelogs unchanged' );
done_testing;
