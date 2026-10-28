#!/usr/bin/env perl
use strict;
use warnings;
use Cwd qw(abs_path getcwd);
use Config;
use File::Copy qw(copy);
use File::Find qw(find);
use File::Path qw(make_path);
use File::Spec;
use File::Temp qw(tempdir);
use Getopt::Long qw(GetOptions);
use JSON::PP qw(encode_json);
use POSIX qw(_exit);
use version ();

my $PERL = abs_path(File::Spec->catdir(File::Spec->curdir()));
my $SCRIPT = abs_path($0);
my $ROOT = abs_path(File::Spec->catdir((File::Spec->splitpath($SCRIPT))[1], '..'));
my $DECODER = File::Spec->catdir($ROOT, 'Decoder');
my $TOOLS = File::Spec->catdir($ROOT, 'author_tools');
my ($plan, $fetch, $corpus_only, $self_test, $work_dir);
GetOptions(
    'plan' => \$plan,
    'fetch' => \$fetch,
    'corpus-only' => \$corpus_only,
    'self-test' => \$self_test,
    'work-dir=s' => \$work_dir,
) or die "Invalid options\n";

sub newer {
    my ($a, $b) = @_;
    return version->parse($a) > version->parse($b);
}
sub select_refs {
    my ($tags, $current_major) = @_;
    my %selected = ($current_major => 'main');
    my %latest;
    for my $tag (@$tags) {
        next unless $tag =~ /^Sereal-Encoder-(\d+(?:\.\d+)+(?:_\d+)?)$/;
        my $v = $1;
        next if $v =~ /_/;
        my ($major) = split /\./, $v;
        next if $major < 2 || $major == $current_major;
        $latest{$major} = [$v, $tag] if !$latest{$major} || newer($v, $latest{$major}[0]);
    }
    $selected{$_} = $latest{$_}[1] for keys %latest;
    return { map { $_ => $selected{$_} } sort { $a <=> $b } keys %selected };
}
sub existing_branch_worktree {
    my ($branch, $log) = @_;
    return undef if !defined $branch;
    return undef if run_cmd(['git', 'worktree', 'list', '--porcelain'], $ROOT, {}, $log);
    open my $fh, '<', $log or die $!;
    my ($path, $found);
    while (my $line = <$fh>) {
        chomp $line;
        if ($line =~ /^worktree (.+)$/) { $path = $1 }
        elsif ($line eq "branch refs/heads/$branch") { $found = $path }
    }
    close $fh;
    return $found;
}
sub run_cmd {
    my ($cmd, $cwd, $env, $log) = @_;
    my $pid = fork();
    die "fork failed: $!" unless defined $pid;
    if (!$pid) {
        open STDOUT, '>', $log or die "open $log: $!";
        open STDERR, '>&STDOUT' or die "redirect stderr: $!";
        chdir $cwd or die "chdir $cwd: $!";
        %ENV = (%ENV, %{$env || {}});
        exec {$cmd->[0]} @$cmd;
        die "exec $cmd->[0]: $!";
    }
    waitpid($pid, 0);
    return 255 if $? == -1;
    return 128 + ($? & 127) if $? & 127;
    return $? >> 8;
}
sub copy_snapshot {
    my ($from, $to) = @_;
    make_path($to);
    find({
        no_chdir => 1,
        wanted => sub {
            my $src = $File::Find::name;
            my $rel = File::Spec->abs2rel($src, $from);
            return if $rel eq '.git' || $rel =~ m{^\.git/} ||
                $rel =~ m{(?:^|/)(?:blib|Makefile)(?:/|$)} ||
                $rel =~ m{(?:^|/)(?:MYMETA\.(?:json|yml)|pm_to_blib|[^/]+\.(?:o|bs))$};
            my $dst = File::Spec->catfile($to, $rel);
            if (-d $src) { make_path($dst); return }
            return unless -f $src;
            my (undef, $dir) = File::Spec->splitpath($dst);
            make_path($dir) if $dir && !-d $dir;
            copy($src, $dst) or die "copy $src -> $dst: $!";
        },
    }, $from);
}

if ($self_test) {
    my $selected = select_refs([qw(Sereal-Encoder-0.37 Sereal-Encoder-1.99 Sereal-Encoder-2.09 Sereal-Encoder-2.12 Sereal-Encoder-2.100 Sereal-Encoder-3.015 Sereal-Encoder-4.025 Sereal-Encoder-5.011 Sereal-Encoder-6.000_004)], 6);
    die "numeric version selection failed\n" unless join(',', map { $selected->{$_} } sort { $a <=> $b } keys %$selected) eq 'Sereal-Encoder-2.12,Sereal-Encoder-3.015,Sereal-Encoder-4.025,Sereal-Encoder-5.011,main';
    my $signal_log = File::Spec->catfile(File::Spec->tmpdir(), "sereal-matrix-signal-$$.log");
    my $signal_status = run_cmd([$^X, '-e', 'kill 9, $$'], $ROOT, {}, $signal_log);
    unlink $signal_log;
    die "child signal exit was not reported as failure\n" unless $signal_status == 137;
    print "selection checks passed\n";
    exit 0;
}

if ($fetch) {
    my $rc = run_cmd(['git', 'fetch', 'origin', '--tags', 'main'], $ROOT, {}, File::Spec->devnull());
    die "git fetch failed ($rc)\n" if $rc;
}
my $tag_log = File::Spec->catfile(File::Spec->tmpdir(), "sereal-encoder-tags-$$.log");
die "git tag failed\n" if run_cmd(['git', 'tag', '--list', 'Sereal-Encoder-*'], $ROOT, {}, $tag_log);
open my $tf, '<', $tag_log or die $!;
my @tags = <$tf>; chomp @tags;
close $tf;
unlink $tag_log;
open my $evf, '<', File::Spec->catfile($ROOT, 'Encoder', 'lib', 'Sereal', 'Encoder.pm') or die $!;
my $encoder_source = do { local $/; <$evf> }; close $evf;
$encoder_source =~ /\$VERSION\s*=\s*'([0-9]+(?:\.[0-9]+)?(?:_[0-9]+)?)/ or die "cannot read current Encoder VERSION";
my $current_major = (split /\./, $1)[0];
my $refs = select_refs(\@tags, $current_major);
my %commits;
for my $major (keys %$refs) {
    my $log = File::Spec->catfile(File::Spec->tmpdir(), "sereal-encoder-commit-$$.log");
    my $commit_ref = "$refs->{$major}^{commit}";
    die "git rev-parse failed for $refs->{$major}\n" if run_cmd(['git', 'rev-parse', $commit_ref], $ROOT, {}, $log);
    open my $fh, '<', $log or die $!; chomp($commits{$major} = <$fh>); close $fh; unlink $log;
}
if ($plan) {
    my %sources = map { $_ => ($_ == $current_major ? 'main' : "maint_perl/v$_") } keys %$refs;
    print JSON::PP->new->canonical->pretty->encode({ selection => $refs, source_branches => \%sources, commits => \%commits });
    exit 0;
}

$work_dir ||= tempdir('sereal-encoder-matrix-XXXXXX', TMPDIR => 1, CLEANUP => 0);
$work_dir = abs_path($work_dir) || do { make_path($work_dir); abs_path($work_dir) };
make_path($work_dir);
$ENV{CCACHE_DIR} = File::Spec->catdir($work_dir, 'ccache');
make_path($ENV{CCACHE_DIR});
my %results = (workspace => $work_dir, selection => $refs, commits => \%commits, entries => {});
my $decoder_configure = run_cmd([$^X, 'Makefile.PL'], $DECODER, {}, File::Spec->catfile($work_dir, 'current-decoder-configure.log'));
die "current Decoder configure failed; see current-decoder-configure.log" if $decoder_configure;
my $decoder_build = run_cmd(['make', '-j2'], $DECODER, {}, File::Spec->catfile($work_dir, 'current-decoder-build.log'));
die "current Decoder build failed; see current-decoder-build.log" if $decoder_build;
for my $major (sort { $a <=> $b } keys %$refs) {
    my $ref = $refs->{$major};
    my $entry = $results{entries}{$major} = { ref => $ref, commit => $commits{$major}, status => 'pending' };
    my $wt = $major == $current_major ? $ROOT : File::Spec->catdir($work_dir, 'worktrees', "major-$major");
    my $branch = $major == $current_major ? undef : "maint_perl/v$major";
    make_path(File::Spec->catdir($work_dir, 'worktrees'));
    my $ok = eval {
        unless ($major == $current_major || -e File::Spec->catfile($wt, '.git')) {
            my @cmd;
            if (!defined $branch) {
                @cmd = ('git', 'worktree', 'add', '--detach', $wt, $ref);
            }
            else {
                my $existing = existing_branch_worktree($branch, File::Spec->catfile($work_dir, "worktree-list-$major.log"));
                if ($existing) { $wt = $existing }
                else {
                    my $local = run_cmd(['git', 'show-ref', '--verify', '--quiet', "refs/heads/$branch"], $ROOT, {}, File::Spec->devnull());
                    my $remote = run_cmd(['git', 'show-ref', '--verify', '--quiet', "refs/remotes/origin/$branch"], $ROOT, {}, File::Spec->devnull());
                    @cmd = $local == 0 ? ('git', 'worktree', 'add', $wt, $branch)
                        : $remote == 0 ? ('git', 'worktree', 'add', '-b', $branch, $wt, "origin/$branch")
                        : ('git', 'worktree', 'add', '-b', $branch, $wt, $ref);
                }
            }
            if (@cmd) {
                my $rc = run_cmd(\@cmd, $ROOT, {}, File::Spec->catfile($work_dir, "worktree-$major.log"));
                die "worktree creation failed; see worktree-$major.log" if $rc;
            }
        }
        my $source_commit_log = File::Spec->catfile($work_dir, "source-commit-$major.log");
        my $source_rc = run_cmd(['git', 'rev-parse', 'HEAD'], $wt, {}, $source_commit_log);
        die "cannot read source commit for major $major" if $source_rc;
        open my $scf, '<', $source_commit_log or die $!;
        chomp(my $source_commit = <$scf>); close $scf;
        $results{source_commits}{$major} = $source_commit;
        my $ancestry_rc = run_cmd(['git', 'merge-base', '--is-ancestor', $commits{$major}, 'HEAD'], $wt, {}, File::Spec->catfile($work_dir, "source-ancestry-$major.log"));
        die "source branch does not descend from selected ref $ref" if $ancestry_rc;
        unless ($major == $current_major) {
            my $dirty_log = File::Spec->catfile($work_dir, "historical-worktree-status-$major.log");
            my $dirty_rc = run_cmd(['git', 'status', '--porcelain'], $wt, {}, $dirty_log);
            die "cannot inspect historical source worktree" if $dirty_rc;
            open my $hf, '<', $dirty_log or die $!;
            my $dirty = <$hf>; close $hf;
            die "historical worktree for $branch has uncommitted changes" if defined $dirty && length $dirty;
        }
        my $branch_log = File::Spec->catfile($work_dir, "source-branch-$major.log");
        run_cmd(['git', 'branch', '--show-current'], $wt, {}, $branch_log);
        open my $bf, '<', $branch_log or die $!;
        my $actual_branch = <$bf>; chomp $actual_branch if defined $actual_branch; close $bf;
        $entry->{source_branch} = $actual_branch || $ref;
        my $src;
        if ($major == $current_major) {
            # Test the actual current candidate, including an unreleased version
            # bump in the invoking main checkout.
            $src = File::Spec->catdir($ROOT, 'Encoder');
            my $status_log = File::Spec->catfile($work_dir, "source-working-tree-$major.log");
            my $status_rc = run_cmd(['git', 'status', '--porcelain'], $ROOT, {}, $status_log);
            die "cannot inspect main source checkout" if $status_rc;
            open my $sf, '<', $status_log or die $!;
            my $status = do { local $/; <$sf> }; close $sf;
            $results{source_worktree_status}{$major} = length($status || '') ? 'modified' : 'clean';
        }
        else {
            my $snapshot = File::Spec->catdir($work_dir, 'build-sources', "major-$major");
            unless (-d $snapshot) {
                copy_snapshot($wt, $snapshot);
                my $rc = run_cmd(['git', 'init', '-q'], $snapshot, {}, File::Spec->catfile($work_dir, "snapshot-git-$major.log"));
                die "snapshot git init failed" if $rc;
            }
            copy_snapshot($wt, $snapshot);
            $src = File::Spec->catdir($snapshot, 'Perl', 'Encoder');
        }
        my %build_env = (USE_UNALIGNED => 1, NO_ASM => 1);
        my $rc = run_cmd([$^X, 'Makefile.PL'], $src, \%build_env, File::Spec->catfile($work_dir, "encoder-$major-configure.log"));
        die "Encoder configure failed; see encoder-$major-configure.log" if $rc;
        $rc = run_cmd(['make', '-j2'], $src, {}, File::Spec->catfile($work_dir, "encoder-$major-build.log"));
        die "Encoder build failed; see encoder-$major-build.log" if $rc;
        my $corpus = File::Spec->catdir($work_dir, 'corpora', "encoder-major-$major");
        make_path($corpus);
        my $encoder_lib = File::Spec->catdir($src, 'blib', 'lib');
        my $env = { PERL5LIB => join($Config{path_sep}, $encoder_lib, File::Spec->catdir($src, 'blib', 'arch')) };
        $rc = run_cmd([$^X, File::Spec->catfile($TOOLS, 'encoder_compat_generate.pl'),
                       File::Spec->catfile($TOOLS, 'encoder_compat_cases.pl'),
                       File::Spec->catdir($encoder_lib, 'Sereal'), $corpus, $ref, $commits{$major}, $source_commit],
                      $src, $env, File::Spec->catfile($work_dir, "corpus-generate-$major.log"));
        die "corpus generation failed; see corpus-generate-$major.log" if $rc;
        my $current_perl5lib = join($Config{path_sep}, File::Spec->catdir($DECODER, 'blib', 'lib'), File::Spec->catdir($DECODER, 'blib', 'arch'), File::Spec->catdir($ROOT, 'shared', 't', 'lib'));
        $rc = run_cmd([$^X, File::Spec->catfile($TOOLS, 'encoder_compat_validate.pl'),
                       File::Spec->catfile($TOOLS, 'encoder_compat_cases.pl'), $corpus,
                       File::Spec->catdir($DECODER, 'blib', 'lib', 'Sereal')],
                      $DECODER, { PERL5LIB => $current_perl5lib }, File::Spec->catfile($work_dir, "corpus-validate-$major.log"));
        die "current Decoder rejected historical corpus; see corpus-validate-$major.log" if $rc;
        $entry->{corpus} = $corpus;
        $entry->{status} = 'corpus passed';
        unless ($corpus_only) {
            my $test_lib = join($Config{path_sep}, File::Spec->catdir($DECODER, 'blib', 'lib'), File::Spec->catdir($DECODER, 'blib', 'arch'),
                                File::Spec->catdir($src, 'blib', 'lib'), File::Spec->catdir($src, 'blib', 'arch'), File::Spec->catdir($ROOT, 'shared', 't', 'lib'));
            $rc = run_cmd(['make', "PERL=$^X", 'test'], $DECODER, { PERL5LIB => $test_lib }, File::Spec->catfile($work_dir, "decoder-suite-$major.log"));
            if ($rc) { $entry->{status} = 'Decoder suite failed'; $entry->{suite_exit} = $rc }
            else { $entry->{status} = 'passed' }
        }
        1;
    };
    if (!$ok) {
        my $error = $@ || 'unknown failure';
        $error =~ s/\s+\z//;
        $entry->{status} = 'BUILD/CORPUS FAIL';
        $entry->{error} = $error;
    }
}
open my $out, '>', File::Spec->catfile($work_dir, 'results.json') or die $!;
print {$out} JSON::PP->new->canonical->pretty->encode(\%results); close $out;
print JSON::PP->new->canonical->pretty->encode(\%results);
exit scalar(grep { $_->{status} ne 'passed' && $_->{status} ne 'corpus passed' } values %{$results{entries}}) ? 1 : 0;
