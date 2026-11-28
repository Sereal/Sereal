use 5.014;
use strict;
use warnings;
use Test::More;
use File::Copy qw(copy);
use File::Path qw(make_path);
use File::Temp qw(tempdir);
use FindBin;

plan skip_all => 'the regeneration command requires Bash and Unix processes'
    if $^O eq 'MSWin32';

sub write_file {
    my ( $path, $contents ) = @_;
    open my $handle, '>', $path or die "Open $path: $!";
    print {$handle} $contents or die "Write $path: $!";
    close $handle             or die "Close $path: $!";
}

sub read_file {
    my ($path) = @_;
    open my $handle, '<', $path or die "Open $path: $!";
    local $/;
    return <$handle>;
}

sub run_regeneration {
    my ($failure)          = @_;
    my $directory          = tempdir( CLEANUP => 1 );
    my @source_directories = (
        'Decoder',
        'Encoder',
        'Sereal',
        'shared',
        'cpantesters',
        'author_tools',
    );
    make_path( "$directory/bin", map { "$directory/$_" } @source_directories );
    for my $source_directory (@source_directories) {
        write_file( "$directory/$source_directory/fixture.t", "use strict;\n" );
    }
    write_file( "$directory/top level.pl", "use strict;\n" );
    copy( "$FindBin::Bin/../../clean_regen_and_tidy", "$directory/clean_regen_and_tidy" )
        or die "Copy regeneration command: $!";

    # Stub only the tools that can modify the checkout. The actual Bash
    # command, find pipeline, filename handling and exit propagation run.
    write_file( "$directory/bin/git", <<'GIT');
#!/bin/bash
printf 'git %s\n' "$*" >> "$REGEN_LOG"
GIT
    write_file( "$directory/bin/perl", <<'PERL');
#!/bin/bash
printf 'perl %s\n' "$*" >> "$REGEN_LOG"
if [[ "$REGEN_FAIL" == generator ]]; then
    exit 17
fi
PERL
    write_file( "$directory/bin/perltidy", <<'TIDY');
#!/bin/bash
printf 'tidy %s\n' "${@: -1}" >> "$REGEN_LOG"
if [[ "$REGEN_FAIL" == tidy ]]; then
    exit 19
fi
TIDY
    chmod 0755, map { "$directory/bin/$_" } ( 'git', 'perl', 'perltidy' )
        or die "Make tool stubs executable: $!";
    local $ENV{PATH}       = "$directory/bin:$ENV{PATH}";
    local $ENV{REGEN_LOG}  = "$directory/actions.log";
    local $ENV{REGEN_FAIL} = $failure;
    my $pid = fork();
    die "Fork: $!" unless defined $pid;

    if ( !$pid ) {
        open STDOUT, '>',  "$directory/output.log" or die $!;
        open STDERR, '>&', STDOUT                  or die $!;
        exec 'bash', "$directory/clean_regen_and_tidy";
        die "Start regeneration command: $!";
    }
    waitpid( $pid, 0 );
    my $status = $? >> 8;
    return ( $status, read_file("$directory/actions.log"), read_file("$directory/output.log") );
}

my ( $status, $actions, $output ) = run_regeneration('none');
is( $status, 0, 'regeneration succeeds from outside its own directory' ) or diag $output;
like(
    $actions, qr/^tidy \.\/top level\.pl$/m,
    'format top-level Perl scripts without splitting filenames'
);
like( $actions, qr/^tidy cpantesters\/fixture\.t$/m,  'include the report collector' );
like( $actions, qr/^tidy author_tools\/fixture\.t$/m, 'include maintainer tools' );
is(
    scalar( () = $actions =~ /^git clean -dfx$/mg ), 2,
    'successful run completes both cleanup steps'
);

( $status, $actions ) = run_regeneration('generator');
is( $status, 17, 'generator failure stops regeneration' );
unlike( $actions, qr/^tidy /m, 'do not format after a generation failure' );
is(
    scalar( () = $actions =~ /^git clean -dfx$/mg ), 1,
    'do not mask generation failure with final cleanup'
);

( $status, $actions ) = run_regeneration('tidy');
is( $status, 19, 'formatter failure propagates through the find pipeline' );
is( scalar( () = $actions =~ /^tidy /mg ), 1, 'stop at the first formatter failure' );
is(
    scalar( () = $actions =~ /^git clean -dfx$/mg ), 1,
    'do not mask formatting failure with final cleanup'
);

done_testing;
