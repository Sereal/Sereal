#!/usr/bin/env perl
use 5.014;
use strict;
use warnings;

package Sereal::ReleaseCI;
use Getopt::Long qw(GetOptionsFromArray);
use JSON::PP     qw(decode_json);
use Time::HiRes  ();

sub command {
    my (@command) = @_;
    open my $fh, '-|', @command or die "Start @command: $!\n";
    my $out = do { local $/; <$fh> };
    close $fh or die "Command failed: @command\n";
    return $out // '';
}

sub wait_for_ci {
    my ( $sha, %opt ) = @_;
    my $run      = $opt{command} || \&command;
    my $clock    = $opt{clock}   || sub { Time::HiRes::time() };
    my $sleep    = $opt{sleep}   || sub { Time::HiRes::sleep( $_[0] ) };
    my $deadline = $clock->() + ( $opt{timeout} || 7200 );
    while (1) {
        my $rows = decode_json(
            $run->(
                'gh',       'run', 'list',    '--workflow', 'ci.yml',
                '--commit', $sha,  '--event', 'push', '--limit', '20',
                '--json',   'databaseId,status,conclusion,headSha'
            ) );
        die "Unexpected CI run list\n" unless ref($rows) eq 'ARRAY';
        my ($latest) = sort { $b->{databaseId} <=> $a->{databaseId} }
            grep { ( $_->{headSha} || '' ) eq $sha } @$rows;
        if ( $latest && $latest->{status} eq 'completed' ) {
            die "CI did not pass for $sha: $latest->{conclusion}\n"
                unless ( $latest->{conclusion} || '' ) eq 'success';
            die "Invalid workflow run ID\n" unless $latest->{databaseId} =~ /\A\d+\z/;
            my $detail = decode_json(
                $run->( 'gh', 'run', 'view', $latest->{databaseId}, '--json', 'jobs' ) );
            die "Missing CI job list\n" unless ref( $detail->{jobs} ) eq 'ARRAY';
            my @checks =
                grep { ( $_->{name} || '' ) eq 'Perl release checks' } @{ $detail->{jobs} };
            die "The required 'Perl release checks' job did not pass\n"
                unless @checks && !grep { ( $_->{conclusion} || '' ) ne 'success' } @checks;
            print "CI and Perl release checks passed for $sha (run $latest->{databaseId}).\n";
            return;
        }
        die "Timed out waiting for CI for $sha\n" if $clock->() >= $deadline;
        print "Waiting for CI for $sha"
            . ( $latest ? " ($latest->{status})" : ' (not queued yet)' ) . "\n";
        $sleep->( $opt{interval} || 30 );
    }
}

sub main {
    my @args = @_;
    my ( $timeout, $interval ) = ( 7200, 30 );
    GetOptionsFromArray( \@args, 'timeout=i' => \$timeout, 'interval=i' => \$interval )
        or die "Usage: $0 [--timeout SECONDS] [--interval SECONDS]\n";
    die "Unexpected arguments\n"        if @args;
    die "Invalid timeout or interval\n" if $timeout < 1 || $interval < 15;
    my $sha = command( 'git', 'rev-parse', 'HEAD' );
    chomp $sha;
    die "Invalid git revision\n" unless $sha =~ /\A[0-9a-f]{40,64}\z/;
    die "Tracked changes are not committed; CI would not cover them\n"
        if length command( 'git', 'status', '--porcelain', '--untracked-files=no' );
    wait_for_ci( $sha, timeout => $timeout, interval => $interval );
    my $current = command( 'git', 'rev-parse', 'HEAD' );
    chomp $current;
    die "HEAD changed while waiting for CI\n" unless $current eq $sha;
    die "Tracked files changed while waiting for CI\n"
        if length command( 'git', 'status', '--porcelain', '--untracked-files=no' );
}

unless (caller) {
    eval { main(@ARGV) };
    if ($@) { warn $@; exit 1 }
}
1;
