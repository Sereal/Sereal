#!perl
use 5.014;
use strict;
use warnings;
use Config;
use Cwd qw(abs_path getcwd);
use File::Copy qw(copy);
use File::Path qw(make_path);
use File::Temp qw(tempdir);
use FindBin;
use Test::More;

# Run from Perl/: perl author_tools/test_zstd_build.t [Perl-source-root]
# Repository-only integration test: generate real MakeMaker/Zstd makefiles,
# but use tiny archive members so no compiler or full Zstd build is needed.
plan skip_all => 'requires fork and Unix ar' if $^O eq 'MSWin32';
my $make = $Config{make} || 'make';
open my $version, '-|', $make, '--version' or plan skip_all => 'make unavailable';
my $make_version = do { local $/; <$version> };
close $version;
plan skip_all => 'parallel regression requires GNU make'
    unless $make_version =~ /GNU Make/;

my $source = abs_path(shift || "$FindBin::Bin/..");
my $original_dir = getcwd();
my $work = tempdir('zstd-build-XXXXXXXX', DIR => $original_dir, CLEANUP => 1);
make_path("$work/zstd/common", "$work/other");
copy("$source/shared/zstd/Makefile.PL", "$work/zstd/Makefile.PL") or die $!;

sub write_file {
    my ($path, $contents) = @_;
    open my $fh, '>', $path or die "$path: $!";
    print {$fh} $contents;
    close $fh or die "$path: $!";
}
sub read_file {
    open my $fh, '<', $_[0] or die "$_[0]: $!";
    local $/;
    return <$fh>;
}
sub run_logged {
    my (@command) = @_;
    my $pid = fork();
    die "fork: $!" unless defined $pid;
    if (!$pid) {
        open STDOUT, '>>', "$work/commands.log" or die $!;
        open STDERR, '>&', STDOUT or die $!;
        exec @command;
        die "exec @command: $!";
    }
    waitpid($pid, 0);
    my $status = $?;
    diag(read_file("$work/commands.log")) if $status;
    return $status == 0;
}
sub archive_members {
    open my $fh, '-|', 'ar', 't', 'zstd/libzstd.o' or die $!;
    my @members = <$fh>;
    close $fh or die 'ar t failed';
    chomp @members;
    return \@members;
}
sub archive_calls {
    return scalar(() = read_file('zstd/archive-calls.log') =~ /^archive$/mg);
}

# The delay lets independent recursive makes overlap reliably on the old
# build graph. Record invocations while using the real archive tool.
write_file("$work/archive.pl", <<'AR');
use strict;
use warnings;
open my $log, '>>', 'archive-calls.log' or die $!;
print {$log} "archive\n";
close $log;
select undef, undef, undef, 1;
exec 'ar', @ARGV;
die "ar: $!";
AR
write_file("$work/other/Makefile.PL", q{use ExtUtils::MakeMaker;
WriteMakefile(NAME => 'Sereal::OtherBuildTest', VERSION => '0.001', DIR => [], PM => {});
});
my $tools = "$source/shared/inc/Sereal/BuildTools.pm";
$tools =~ s/(['\\])/\\$1/g;
write_file("$work/Makefile.PL", "use ExtUtils::MakeMaker;\nrequire '$tools';\n"
    . "WriteMakefile(NAME => 'Sereal::ZstdBuildTest', VERSION => '0.001', "
    . q{DIR => ['zstd', 'other'], PM => {}, OBJECT => 'zstd/libzstd$(OBJ_EXT)', LD => }
    . "'$^X $work/check-archive.pl');\n");
# MakeMaker detects link code, but OBJECT excludes this file. The fake
# linker checks ordering without compiling or loading a real extension.
write_file("$work/link.c", "/* link detection fixture */\n");
write_file("$work/check-archive.pl", <<'LINK');
die "archive missing before link" unless -s 'zstd/libzstd.o';
my $output;
for my $i (0 .. $#ARGV - 1) {
    $output = $ARGV[$i + 1] if $ARGV[$i] eq '-o';
}
die "missing linker output argument" unless defined $output;
open my $fh, '>', $output or die $!;
print {$fh} "fake extension\n";
close $fh or die $!;
open my $marker, '>', 'linked' or die $!;
close $marker or die $!;
LINK
write_file("$work/zstd/common/member.c", "/* archive fixture */\n");
write_file("$work/zstd/member.o", "expected archive member\n");
write_file("$work/zstd/stray.o", "unrelated object\n");
utime time - 20, time - 20, "$work/zstd/common/member.c" or die $!;
utime time - 10, time - 10, "$work/zstd/member.o" or die $!;
chdir $work or die $!;
local $ENV{PERL_MM_USE_DEFAULT} = 1;
local $ENV{NO_ASM} = 1;
ok(run_logged($^X, 'Makefile.PL'), 'generate real MakeMaker and Zstd makefiles');
my @build = ($make, '-j8', "AR=$^X $work/archive.pl");
ok(run_logged(@build), 'parallel subdirectory and archive targets succeed');
like(read_file("$work/commands.log"), qr{Entering directory [^\n]*/other},
    'other subdirectories retain normal MakeMaker builds');
ok(-f 'linked', 'linker runs after the archive is complete');
is(archive_calls(), 1, 'only one recursive build writes the archive');
is_deeply(archive_members(), ['member.o'], 'archive excludes unrelated objects');

# Seed stale members and force an incremental rebuild. Replacing the
# archive must remove old members, including a
# nested archive left behind by the old wildcard recipe.
make_path('zstd/nested');
copy('zstd/libzstd.o', 'zstd/nested/libzstd.o') or die $!;
ok(run_logged('ar', 'rcs', 'zstd/libzstd.o', 'zstd/stray.o', 'zstd/nested/libzstd.o'),
    'seed stale object and nested archive members');
# The archive remains newer than the child Makefile: only the child make
# can detect that its object changed. The parent must still invoke it.
my $object_time = (stat 'zstd/member.o')[9];
utime $object_time - 30, $object_time - 30, 'Makefile.PL', 'zstd/Makefile.PL' or die $!;
utime $object_time - 20, $object_time - 20, 'Makefile', 'zstd/Makefile' or die $!;
utime $object_time - 5, $object_time - 5, 'zstd/libzstd.o' or die $!;
ok(run_logged(@build), 'parallel incremental rebuild succeeds');
is(archive_calls(), 2, 'incremental rebuild writes the archive once');
is_deeply(archive_members(), ['member.o'], 'incremental rebuild removes stale members');
ok(run_logged(@build), 'repeat parallel build succeeds');
is(archive_calls(), 2, 'repeat build does not rewrite an unchanged archive');

# Strawberry Perl need not provide a Unix rm executable. Reject any such
# invocation while exercising both archive replacement and wildcard cleanup.
make_path('no-rm');
write_file('no-rm/rm', "#!/bin/sh\necho 'unexpected Unix rm invocation' >&2\nexit 1\n");
chmod 0755, 'no-rm/rm' or die $!;
{
    local $ENV{PATH} = "$work/no-rm:$ENV{PATH}";
    utime time - 10, time - 10, 'zstd/libzstd.o' or die $!;
    utime time - 5, time - 5, 'zstd/member.o' or die $!;
    ok(run_logged($make, '-C', 'zstd', "AR=$^X $work/archive.pl"),
        'archive replacement does not require Unix rm');
    is_deeply(archive_members(), ['member.o'], 'portable removal preserves archive members');
    ok(run_logged($make, '-C', 'zstd', 'clean'), 'cleanup does not require Unix rm');
    ok(!-e 'zstd/member.o' && !-e 'zstd/stray.o' && !-e 'zstd/libzstd.o',
        'portable cleanup removes objects and archive');
}
if ($ENV{SEREAL_BUILD_TEST_LOG}) {
    copy("$work/commands.log", $ENV{SEREAL_BUILD_TEST_LOG}) or die $!;
}
chdir $original_dir or die $!;
done_testing;
