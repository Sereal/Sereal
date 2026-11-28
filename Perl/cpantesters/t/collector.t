use 5.014;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempdir);
use File::Spec;
use FindBin;
use HTTP::Response;
use JSON::PP;

require "$FindBin::Bin/../fetch-reports.pl";
my $class = 'Sereal::CPANTesters::Collector';
my $id1   = '611ef0ee-eaae-11f1-a28f-6209cf400910';
my $id2   = 'bf3c574c-eab4-11f1-8831-9c506e8775ea';
my $now   = 10000;
my ( @sleeps, @requests );

{

    package Local::UA;
    sub new { bless { responses => [ @_[ 1 .. $#_ ] ] }, $_[0] }

    sub request {
        my ( $self, $req ) = @_;
        push @requests, { url => "" . $req->uri, time => $now, request => $req };
        die "Unexpected network request: " . $req->uri unless @{ $self->{responses} };
        return shift @{ $self->{responses} };
    }
}

sub response {
    my ( $data, $code, @headers ) = @_;
    return HTTP::Response->new(
        $code || 200,                                       '',
        [ 'Content-Type' => 'application/json', @headers ], JSON::PP->new->utf8->encode($data) );
}

sub collector {
    my (%opt) = @_;
    return $class->new(
        output  => tempdir( CLEANUP => 1 ), delay       => 10,       retries     => 2,
        timeout => 1,                       cache_hours => 6,        max_reports => 50,
        dist    => ['Sereal-Decoder'],      grades      => ['fail'], version     => '7.008',
        clock   => sub { $now },            sleep => sub { push @sleeps, $_[0]; $now += $_[0] },
        %opt,
    );
}

sub row {
    my ($id) = @_;
    return {
        dist     => 'Sereal-Decoder', version => '7.008',  grade  => 'fail',
        guid     => $id || $id1,      perl    => '5.18.4', osname => 'linux',
        platform => 'x86_64-linux',   date    => '2026-11-27T22:03:00Z'
    };
}

sub report {
    my ($id) = @_;
    return {
        id           => $id || $id1,
        distribution => { name => 'Sereal-Decoder', version => '7.008' },
        result       => {
            grade  => 'fail',
            output => {
                uncategorized => "#   Failed test 'use_undef array element uses PL_sv_undef'\n",
                test          => "diagnostic \x{e9}\n", build => "build output\n",
            }
        },
    };
}

sub quiet_collect {
    my ($c) = @_;
    open my $fh, '>', \my $text or die $!;
    local *STDOUT = $fh;
    return $c->collect;
}

eval { Sereal::CPANTesters::Collector::main() };
like( $@, qr/--version is required/, 'never infer a release from the stale aggregate API' );

my $rows = Sereal::CPANTesters::Collector::select_summaries(
    [ row(), row() ], 'Sereal-Decoder', '7.008',
    'fail'
);
is( @$rows, 1, 'duplicate GUIDs are deduplicated' );
eval {
    Sereal::CPANTesters::Collector::select_summaries( [ row() ], 'Sereal-Encoder', '7.008', undef );
};
like( $@, qr/Unexpected distribution/, 'reject summary for another distribution' );
eval {
    Sereal::CPANTesters::Collector::select_summaries(
        [ { %{ row() }, guid => '../escape' } ],
        'Sereal-Decoder', '7.008', undef
    );
};
like( $@, qr/invalid report GUID/, 'reject identifiers that could become paths' );
eval { Sereal::CPANTesters::Collector::validate_report( report($id2), row() ) };
like( $@, qr/Malformed report/, 'reject report whose ID does not match index' );

{
    @requests = (); @sleeps = ();
    my $output = tempdir( CLEANUP => 1 );
    my $c      = collector(
        output => $output,
        ua     => Local::UA->new(
            response( [ row(), row() ], 200, ETag => 'index-v1' ), response( report() ),
        ) );
    is( quiet_collect($c), 0, 'collect full report from API summary' );
    is( @requests,         2, 'one index request and one deduplicated report request' );
    is_deeply( \@sleeps, [10], 'requests have required idle spacing' );
    is( $requests[1]{time} - $requests[0]{time}, 10, 'clock confirms request pacing' );
    my $manifest = Sereal::CPANTesters::Collector::read_json("$output/manifest.json");
    is( $manifest->{reports}{$id1}{download_status},    'saved', 'manifest records saved report' );
    is( @{ $manifest->{reports}{$id1}{failure_lines} }, 1, 'extract failed assertion for triage' );
    open my $fh, '<:encoding(UTF-8)', "$output/reports/$id1.txt" or die $!;
    my $text = do { local $/; <$fh> };
    like( $text, qr/build output/,      'preserve build output' );
    like( $text, qr/diagnostic \x{e9}/, 'UTF-8 text round trips' );
    undef $c;
    $c = collector( output => $output, offline => 1, ua => Local::UA->new() );
    is( quiet_collect($c), 0, 'offline run reconstructs results without network' );
    is( @requests,         2, 'offline mode issues no requests' );
    undef $c;
    $c = collector(
        output        => $output,
        refresh_index => 1,
        ua            => Local::UA->new(
            HTTP::Response->new( 304, 'Not Modified' ),
        ) );
    is( quiet_collect($c), 0, 'refresh revalidates index and reuses immutable report' );
    is( $requests[-1]{request}->header('If-None-Match'), 'index-v1', 'send conditional request' );
    is( @requests, 3, 'refresh does not download report again' );
}

{
    my $c = collector( ua => Local::UA->new( response( [ row(), row($id2) ] ) ), max_reports => 0 );
    is( quiet_collect($c), 0, 'index-only mode succeeds' );
    my $m = Sereal::CPANTesters::Collector::read_json("$c->{output}/manifest.json");
    is( scalar( keys %{ $m->{reports} } ),    2,         'index-only mode records all candidates' );
    is( $m->{reports}{$id1}{download_status}, 'pending', 'download cap marks reports pending' );
}

{
    my $output = tempdir( CLEANUP => 1 );
    my $c      = collector(
        output => $output,
        ua     => Local::UA->new( response( [ row() ] ), response( report() ) ) );
    quiet_collect($c);
    undef $c;
    $c = collector(
        output => $output, check => 1, triage => "$output/triage.json",
        ua     => Local::UA->new() );
    is( quiet_collect($c), 2, 'release check flags unreviewed report' );
    Sereal::CPANTesters::Collector::write_json(
        "$output/triage.json", {
            $id1 => {
                status   => 'fixed', reason => 'portable undef inspection',
                evidence => ['953 regression on Perl 5.16.3']
            },
        } );
    is( quiet_collect($c), 0, 'release check accepts documented review' );
    Sereal::CPANTesters::Collector::write_json(
        "$output/triage.json",
        { $id1 => { status => 'fixed' } } );
    is( quiet_collect($c), 2, 'a fixed flag without reason and evidence is insufficient' );
}

{
    my $output    = tempdir( CLEANUP => 1 );
    my $challenge = HTTP::Response->new(
        200, 'OK', [ 'Content-Type' => 'text/html' ],
        '<title>CPAN Testers Matrix - Anti-Bot Verification</title><body id="js-required">&#9313; plus &#9312;</body>'
    );
    my $c      = collector( output => $output, ua => Local::UA->new($challenge) );
    my $before = @requests;
    eval { quiet_collect($c) };
    like( $@, qr/Robot challenge/, 'detect successful HTTP response containing challenge' );
    is( @requests - $before, 1, 'challenge is not retried' );
    my @cache = glob "$output/cache/*";
    is( @cache, 0, 'challenge never poisons JSON cache' );
    my @saved = glob "$output/challenges/*.html";
    is( @saved, 1, 'preserve challenge for manual handling' );
}

{
    my $c = collector(
        ua => Local::UA->new(
            HTTP::Response->new( 429, 'Too Many Requests', [ 'Retry-After' => '3600' ] ) ) );
    my $start = $now;
    eval { quiet_collect($c) };
    like( $@, qr/cooldown saved/, 'long Retry-After stops the run' );
    is( $c->{rate}{next_request}, $start + 3600, 'persist full server cooldown' );
    my $output = $c->{output};
    undef $c;
    $c = collector( output => $output, ua => Local::UA->new() );
    eval { quiet_collect($c) };
    like( $@, qr/Server cooldown until/, 'restart respects server cooldown' );
}

{
    my $c = collector(
        ua => Local::UA->new(
            HTTP::Response->new(
                503, 'Unavailable', [ 'Retry-After' => HTTP::Date::time2str( $now + 45 ) ]
            ),
            response( [ row() ] ),
        ),
        max_reports => 0
    );
    my $start = $now;
    is( quiet_collect($c), 0,  'temporary failure retries successfully' );
    is( $now - $start,     45, 'HTTP-date Retry-After is honored' );
}

{
    my $c = collector(
        ua => Local::UA->new(
            HTTP::Response->new( 302, 'Found', [ 'Location' => 'https://example.org/' ] ) ) );
    eval { quiet_collect($c) };
    like( $@, qr/cross-origin redirect/, 'do not follow redirects to other services' );
}

{
    my $output = tempdir( CLEANUP => 1 );
    my $c      = collector(
        output => $output,
        ua     => Local::UA->new( response( [ row() ] ), HTTP::Response->new( 404, 'Not Found' ) )
    );
    is( quiet_collect($c), 0, 'not-yet-available report does not block collection' );
    my $m = Sereal::CPANTesters::Collector::read_json("$output/manifest.json");
    is( $m->{reports}{$id1}{download_status}, 'not_available', 'record unavailable report' );
    undef $c;
    $c = collector(
        output => $output, refresh_index => 1,
        ua     => Local::UA->new( response( [ row() ] ) ) );
    is( quiet_collect($c), 0, 'index refresh respects negative report cache' );
}

{
    my $output = tempdir( CLEANUP => 1 );
    my $c      = collector( output => $output, ua => Local::UA->new() );
    eval { collector( output => $output, ua => Local::UA->new() ) };
    like( $@, qr/Another collector/, 'concurrent runs cannot multiply request rate' );
}

{
    my $c = collector( check => 1, ua => Local::UA->new( response( [] ) ) );
    eval { quiet_collect($c) };
    like( $@, qr/No reports available/, 'empty index cannot greenlight a release' );
}

{
    my $c = collector( offline => 1, ua => Local::UA->new() );
    eval { quiet_collect($c) };
    like( $@, qr/Offline cache miss/, 'offline mode fails clearly when cache is incomplete' );
}

{
    my $c = collector(
        ua => Local::UA->new(
            response( [ row(), row($id2) ] ), response( report() ),
            HTTP::Response->new( 200, 'OK', [], '<title>Anti-Bot Verification</title>' ),
        ) );
    eval { quiet_collect($c) };
    like( $@, qr/Robot challenge/, 'later challenge interrupts collection' );
    my $m = Sereal::CPANTesters::Collector::read_json("$c->{output}/manifest.json");
    is(
        $m->{reports}{$id1}{download_status}, 'saved',
        'earlier report remains recorded after interruption'
    );
    ok( -f "$c->{output}/reports/$id1.json", 'earlier report survives interruption' );
}

{
    my $c = collector(
        ua => Local::UA->new(
            HTTP::Response->new(
                200, 'OK', [ 'Content-Type' => 'application/json' ], '{incomplete'
            ),
        ) );
    eval { quiet_collect($c) };
    like( $@, qr/Expected JSON/, 'reject malformed JSON response' );
    my @cache = glob "$c->{output}/cache/*";
    is( @cache, 0, 'invalid JSON is not cached' );
}

eval { Sereal::CPANTesters::Collector::main( '--version', '7.008', '--delay', '0' ) };
like( $@, qr/at least 5 seconds/, 'CLI prevents disabling polite pacing' );
eval { Sereal::CPANTesters::Collector::main( '--version', '7.008', '--check', '--offline' ) };
like( $@, qr/cannot be combined/, 'pre-release check requires fresh network indexes' );

{
    require IO::Compress::Gzip;
    my $json = JSON::PP->new->utf8->encode( [ row() ] );
    my $compressed;
    IO::Compress::Gzip::gzip( \$json => \$compressed ) or die 'gzip fixture failed';
    my $c = collector(
        max_reports => 0,
        ua          => Local::UA->new(
            HTTP::Response->new(
                200, 'OK', [ 'Content-Type' => 'application/json', 'Content-Encoding' => 'gzip' ],
                $compressed
            ),
        ) );
    is( quiet_collect($c), 0, 'accept compressed indexes to reduce bandwidth' );
    like(
        $requests[-1]{request}->header('Accept-Encoding'), qr/gzip/,
        'advertise supported compression'
    );
}

{
    my $r = report();
    $r->{result}{output}{test} = '<title>Anti-Bot Verification</title> is test output';
    my $c = collector( ua => Local::UA->new( response( [ row() ] ), response($r) ) );
    is( quiet_collect($c), 0, 'HTML snippets within valid JSON reports are not challenges' );
}

{
    my $pass = { %{ row($id2) }, grade => 'pass' };
    my $c    = collector(
        grades => [qw(fail unknown na)],
        ua     => Local::UA->new( response( [ row(), $pass ] ), response( report() ) ) );
    my $before = @requests;
    is( quiet_collect($c),   0, 'collect non-passes and retain passing index counts' );
    is( @requests - $before, 2, 'do not request passing report details' );
    my $m = Sereal::CPANTesters::Collector::read_json("$c->{output}/manifest.json");
    is( $m->{selection}{index_counts}{'Sereal-Decoder'}{pass}, 1, 'PASS count comes from index' );
    ok( !exists $m->{reports}{$id2}, 'passing report is not in the download ledger' );
}
eval { Sereal::CPANTesters::Collector::main( '--version', '7.008', '--grades', 'pass' ) };
like( $@, qr/Invalid or empty --grades/, 'CLI refuses passing-report downloads' );
eval { Sereal::CPANTesters::Collector::main( '--version', '7.008', '--check', '--grades', 'fail' ) };
like(
    $@, qr/must cover fail,unknown,na/,
    'release check cannot omit build failures or support exclusions'
);

done_testing;
