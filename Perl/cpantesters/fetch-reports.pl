#!/usr/bin/env perl
use 5.014;
use strict;
use warnings;

package Sereal::CPANTesters::Collector;

use Cwd         qw(abs_path);
use Digest::SHA qw(sha256_hex);
use Fcntl       qw(LOCK_EX LOCK_NB);
use File::Path  qw(make_path);
use File::Spec;
use File::Temp qw(tempfile);
use FindBin;
use Getopt::Long qw(GetOptionsFromArray);
use HTTP::Cookies::Netscape;
use HTTP::Date qw(str2time time2str);
use HTTP::Message;
use HTTP::Request;
use JSON::PP;
use LWP::UserAgent;
use Time::HiRes ();
use URI;
use URI::Escape qw(uri_escape_utf8);

my $JSON = JSON::PP->new->utf8->canonical->pretty;
my $API  = 'https://api.cpantesters.org/v3';
my $GUID = qr/[0-9a-f]{8}-(?:[0-9a-f]{4}-){3}[0-9a-f]{12}/i;

sub usage {
    return <<'USAGE';
Usage: perl fetch-reports.pl [options]
  --dist NAME          Repeatable; defaults to Sereal-Decoder and Sereal-Encoder
  --version VERSION    Required: the published version to review
  --grades LIST        Default: fail,unknown,na (passing reports are index-only)
  --output DIR         Cache and reports directory (default: cpantesters-reports
                      beside the repository, so release cleanups preserve it)
  --delay SECONDS      Idle time between HTTP requests (default 10; minimum 5)
  --max-reports N      New report downloads per run (default 50; 0 = index only)
  --cache-hours HOURS  Index freshness (default 6; minimum 1)
  --refresh-index      Revalidate indexes; downloaded reports are still reused
  --offline           Use cached data only; make no network requests
  --check             Refresh indexes and fail if any selected non-pass report
                      is missing or lacks a review in --triage
  --triage FILE       Review decisions (default: triage.json beside script)
  --timeout SECONDS    HTTP timeout (default 60)
  --retries N          Extra attempts for temporary errors (default 2; maximum 3)
  --cookies FILE       Optional browser-exported Netscape cookie file
  --user-agent TEXT    Identify your client/contact (default Sereal-CPANTesters/0.1)
  --help
USAGE
}

sub main {
    my @args = @_;
    my %opt  = (
        output => File::Spec->catdir(
            abs_path( File::Spec->catdir( $FindBin::Bin, '..', '..', '..' ) ), 'cpantesters-reports'
        ),
        delay      => 10, max_reports => 50, cache_hours => 6,
        timeout    => 60, retries     => 2,  grades      => 'fail,unknown,na',
        triage     => File::Spec->catfile( $FindBin::Bin, 'triage.json' ),
        user_agent => 'Sereal-CPANTesters/0.1 (+https://github.com/Sereal/Sereal)',
    );
    GetOptionsFromArray(
        \@args,
        'dist=s@'       => \$opt{dist},        'version=s'     => \$opt{version},
        'grades=s'      => \$opt{grades},      'output=s'      => \$opt{output},
        'delay=f'       => \$opt{delay},       'max-reports=i' => \$opt{max_reports},
        'cache-hours=f' => \$opt{cache_hours}, 'timeout=i'     => \$opt{timeout},
        'retries=i'     => \$opt{retries},     'refresh-index' => \$opt{refresh_index},
        'offline'       => \$opt{offline},     'cookies=s'     => \$opt{cookies},
        'check'         => \$opt{check},       'triage=s'      => \$opt{triage},
        'user-agent=s'  => \$opt{user_agent},  'help'          => \$opt{help},
    ) or die usage();
    die usage() if @args;
    if ( $opt{help} ) { print usage(); return 0 }
    die "--version is required; select the published release explicitly\n"
        unless defined $opt{version};
    $opt{refresh_index} = 1                    if $opt{check};
    die "--delay must be at least 5 seconds\n" if $opt{delay} < 5;
    die "--cache-hours must be at least 1\n"   if $opt{cache_hours} < 1;
    die "--max-reports must be nonnegative\n"  if $opt{max_reports} < 0;
    die "--timeout must be positive\n"         if $opt{timeout} < 1;
    die "--retries must be between 0 and 3\n"  if $opt{retries} < 0 || $opt{retries} > 3;
    die "--offline and --refresh-index cannot be combined\n"
        if $opt{offline} && $opt{refresh_index};
    $opt{dist} ||= [qw(Sereal-Decoder Sereal-Encoder)];

    for ( @{ $opt{dist} } ) {
        die "Invalid distribution: $_\n" unless /\A[A-Za-z0-9]+(?:-[A-Za-z0-9]+)*\z/;
    }
    $opt{grades} = [ split /,/, lc $opt{grades} ];
    die "Invalid or empty --grades\n"
        unless @{ $opt{grades} }
        && !grep { !/\A(?:fail|unknown|na)\z/ } @{ $opt{grades} };
    if ( $opt{check} ) {
        my %grades = map { $_ => 1 } @{ $opt{grades} };
        die "--check must cover fail,unknown,na\n" if grep { !$grades{$_} } qw(fail unknown na);
    }
    if ( defined $opt{version} ) {
        die "Invalid version\n" unless $opt{version} =~ /\A[vV]?[0-9][0-9A-Za-z._-]*\z/;
    }
    return __PACKAGE__->new(%opt)->collect;
}

sub new {
    my ( $class, %opt ) = @_;
    make_path( map { File::Spec->catdir( $opt{output}, $_ ) } qw(cache reports challenges) );
    open my $lock, '>>', File::Spec->catfile( $opt{output}, 'collector.lock' ) or die $!;
    flock( $lock, LOCK_EX | LOCK_NB ) or die "Another collector is using $opt{output}\n";
    my $ua = $opt{ua} || LWP::UserAgent->new(
        agent             => $opt{user_agent}, timeout  => $opt{timeout},
        max_redirect      => 0,                max_size => 32 * 1024 * 1024,
        protocols_allowed => ['https'],        ssl_opts => { verify_hostname => 1 },
    );
    if ( $opt{cookies} ) {
        die "Cookie file does not exist: $opt{cookies}\n" unless -f $opt{cookies};
        $ua->cookie_jar( HTTP::Cookies::Netscape->new( file => $opt{cookies}, autosave => 0 ) );
    }
    my $self = bless {
        %opt, ua => $ua, lock => $lock,
        clock => $opt{clock} || sub { Time::HiRes::time() },
        sleep => $opt{sleep} || sub { Time::HiRes::sleep( $_[0] ) },
    }, $class;
    $self->{rate_file} = File::Spec->catfile( $opt{output}, 'rate.json' );
    $self->{rate}      = -f $self->{rate_file} ? read_json( $self->{rate_file} ) : {};
    return $self;
}

sub read_json {
    my ($path) = @_;
    open my $fh, '<:raw', $path or die "Read $path: $!\n";
    local $/;
    return $JSON->decode(<$fh>);
}

sub write_bytes {
    my ( $path, $bytes ) = @_;
    my ( $fh, $tmp ) =
        tempfile( '.fetch-XXXXXXXX', DIR => ( File::Spec->splitpath($path) )[1], UNLINK => 1 );
    binmode $fh, ':raw';
    print {$fh} $bytes or die "Write $tmp: $!\n";
    close $fh          or die "Close $tmp: $!\n";
    rename $tmp, $path or die "Rename $tmp to $path: $!\n";
}

sub write_json { write_bytes( $_[0], $JSON->encode( $_[1] ) ) }

sub cache_file {
    my ( $self, $url ) = @_;
    return File::Spec->catfile( $self->{output}, 'cache', sha256_hex($url) . '.json' );
}

sub set_cooldown {
    my ( $self, $seconds ) = @_;
    $self->{rate}{next_request} = $self->{clock}->() + $seconds;
    write_json( $self->{rate_file}, $self->{rate} );
}

sub request {
    my ( $self, $req ) = @_;
    my $wait = ( $self->{rate}{next_request} || 0 ) - $self->{clock}->();

    # Persist long Retry-After values, then stop instead of keeping a
    # process asleep for hours. Restarting cannot evade the cooldown.
    die "Server cooldown until " . time2str( $self->{rate}{next_request} ) . "; resume later\n"
        if $wait > 60;
    $self->{sleep}->($wait) if $wait > 0;
    $self->set_cooldown( $self->{delay} );
    warn "GET " . $req->uri . "\n";
    my $res = $self->{ua}->request($req);
    $self->set_cooldown( $self->{delay} );
    return $res;
}

sub get_json {
    my ( $self, $path, %opt ) = @_;
    my $url    = "$API$path";
    my $file   = $self->cache_file($url);
    my $cached = -f $file ? read_json($file) : undef;
    die "Cache URL mismatch: $file\n" if $cached && $cached->{url} ne $url;
    my $ttl     = $cached                && $cached->{missing} ? 3600 : $self->{cache_hours} * 3600;
    my $refresh = $self->{refresh_index} && !$opt{immutable};
    if (
        $cached
        && (   $self->{offline}
            || ( !$refresh && $self->{clock}->() - $cached->{fetched_at} < $ttl )
            || ( $opt{immutable} && !$cached->{missing} ) ) )
    {
        return undef if $cached->{missing};
        $opt{validate}->( $cached->{data} );
        return $cached->{data};
    }
    die "Offline cache miss: $url\n" if $self->{offline};
    my $target = $url;
    my ( $attempt, $redirects ) = ( 0, 0 );
    while (1) {
        my $req = HTTP::Request->new( GET => $target );
        $req->header( Accept            => 'application/json' );
        $req->header( 'Accept-Encoding' => scalar HTTP::Message::decodable() );
        if ( $cached && !$cached->{missing} ) {
            $req->header( 'If-None-Match'     => $cached->{etag} ) if $cached->{etag};
            $req->header( 'If-Modified-Since' => $cached->{last_modified} )
                if $cached->{last_modified};
        }
        my $res = $self->request($req);
        if ( $res->code == 304 && $cached && !$cached->{missing} ) {
            $opt{validate}->( $cached->{data} );
            $cached->{fetched_at} = $self->{clock}->();
            write_json( $file, $cached );
            return $cached->{data};
        }
        my $body = $res->decoded_content;
        $body = '' unless defined $body;
        my $is_html = ( $res->header('Content-Type') || '' ) =~ /html/i
            || $body =~ /\A\s*(?:<!doctype\s+html|<html|<head|<title|<script)\b/i;

        # Check the response, including HTTP 200 challenge pages, before
        # retrying or writing anything to the successful-response cache.
        if (   $is_html
            && $body =~
            /(?:<title[^>]*>[^<]*(?:anti.bot|verification|just a moment)|id=["']js-required|cf-chl-|g-recaptcha|h-captcha|Botchecker|Cookie Test Failed)/i
            || $res->header('cf-mitigated') && $res->header('cf-mitigated') eq 'challenge' )
        {
            my $saved =
                File::Spec->catfile( $self->{output}, 'challenges', sha256_hex($url) . '.html' );
            write_bytes( $saved, $res->decoded_content( charset => 'none' ) );
            die "Robot challenge at $target; saved $saved. Open the URL in a browser,\n"
                . "complete the check, export Netscape cookies, then resume with --cookies FILE\n";
        }
        if ( $res->is_redirect && $res->header('Location') ) {
            my $next   = URI->new_abs( $res->header('Location'), $target );
            my $origin = URI->new($API);
            die "Refusing cross-origin redirect to $next\n"
                unless $next->scheme eq 'https'
                && $next->authority eq $origin->authority;
            die "Too many redirects at $url\n" if ++$redirects > 3;
            $target = "$next";
            next;
        }
        if ( $res->code == 429 || $res->code =~ /\A(?:500|502|503|504|599)\z/ ) {
            my $retry   = $res->header('Retry-After');
            my $seconds = 30 * ( 2**$attempt );
            if ( defined $retry ) {
                my $requested =
                    $retry =~ /\A\d+\z/ ? $retry : ( str2time($retry) || 0 ) - $self->{clock}->();
                $seconds = $requested if $requested > $seconds;
            }
            $seconds = $self->{delay} if $seconds < $self->{delay};
            $self->set_cooldown($seconds);
            die "HTTP " . $res->code . " at $target; cooldown saved, resume later\n"
                if $attempt++ >= $self->{retries} || $seconds > 60;
            next;
        }
        if ( $res->code == 404 && $opt{allow_missing} ) {
            write_json( $file, { url => $url, fetched_at => $self->{clock}->(), missing => 1 } );
            warn "Report not available yet: $url (cached for one hour)\n";
            return undef;
        }
        die "HTTP " . $res->status_line . " at $target\n" unless $res->is_success;
        die "Truncated response from $target\n" if $res->header('Client-Aborted');
        my $data = eval { $JSON->decode( $res->decoded_content( charset => 'none' ) ) };
        die "Expected JSON from $target; received invalid JSON or an unrecognized challenge\n"
            if $@;
        $opt{validate}->($data);
        write_json(
            $file, {
                url           => $url, fetched_at => $self->{clock}->(), data => $data,
                etag          => scalar $res->header('ETag'),
                last_modified => scalar $res->header('Last-Modified'),
            } );
        return $data;
    }
}

sub validate_array {
    die "Expected an array of API records\n"
        unless ref( $_[0] ) eq 'ARRAY' && !grep { ref($_) ne 'HASH' } @{ $_[0] };
}

sub select_summaries {
    my ( $rows, $dist, $version, $grade ) = @_;
    validate_array($rows);
    my %seen;
    my @out;
    for my $row (@$rows) {
        die "Unexpected distribution/version/grade in summary response\n"
            unless ( $row->{dist} || '' ) eq $dist
            && defined( $row->{version} )
            && "$row->{version}" eq "$version"
            && ( $row->{grade} || '' ) =~ /\A(?:pass|fail|unknown|na)\z/i
            && ( !defined($grade) || lc( $row->{grade} ) eq $grade );
        my $id = lc( $row->{guid} || '' );
        die "Missing or invalid report GUID\n" unless $id =~ /\A$GUID\z/;
        next if $seen{$id}++;
        push @out, { %$row, guid => $id };
    }
    return \@out;
}

sub report_text {
    my ($report) = @_;
    my $out = $report->{result}{output};
    return join "\n", map { "===== $_ =====\n$out->{$_}\n" } sort keys %$out;
}

sub validate_report {
    my ( $report, $row ) = @_;
    die "Malformed report $row->{guid}\n"
        unless ref($report) eq 'HASH'
        && lc( $report->{id} || '' ) eq $row->{guid}
        && ref( $report->{distribution} ) eq 'HASH'
        && ( $report->{distribution}{name} || '' ) eq $row->{dist}
        && defined( $report->{distribution}{version} )
        && "$report->{distribution}{version}" eq "$row->{version}"
        && ref( $report->{result} ) eq 'HASH'
        && lc( $report->{result}{grade} || '' ) eq lc $row->{grade}
        && ref( $report->{result}{output} ) eq 'HASH'
        && !grep { ref($_) } values %{ $report->{result}{output} };
}

sub collect {
    my ($self)        = @_;
    my $manifest_file = File::Spec->catfile( $self->{output}, 'manifest.json' );
    my $manifest      = -f $manifest_file ? read_json($manifest_file) : { reports => {} };
    my ( @selected, %seen, %versions, %index_counts );
    for my $dist ( @{ $self->{dist} } ) {
        next if exists $versions{$dist};
        my $version = $self->{version};
        die "A published version is required\n" unless defined $version;
        $versions{$dist} = $version;
        warn "Indexing $dist $version\n";

        # One version-scoped summary request supplies every grade. Changing
        # local filters can reuse it without making another server request.
        my $rows = $self->get_json(
            '/summary/' . uri_escape_utf8($dist) . '/' . uri_escape_utf8($version),
            validate => sub { select_summaries( $_[0], $dist, $version, undef ) } );
        die "No reports available for $dist $version; cannot establish release coverage\n"
            if $self->{check} && !@$rows;
        my %wanted = map { $_ => 1 } @{ $self->{grades} };
        $index_counts{$dist} = { map { $_ => 0 } qw(pass fail unknown na) };
        for my $row ( @{ select_summaries( $rows, $dist, $version, undef ) } ) {
            $index_counts{$dist}{ lc $row->{grade} }++;
            next unless $wanted{ lc $row->{grade} };
            next if lc( $row->{grade} ) eq 'pass';
            push @selected, $row unless $seen{ $row->{guid} }++;
        }
        print "$dist $version: "
            . join( ' ', map { uc($_) . '=' . $index_counts{$dist}{$_} } qw(pass fail unknown na) )
            . "\n";
    }

    @selected = sort { ( $b->{date} || '' ) cmp( $a->{date} || '' ) || $a->{guid} cmp $b->{guid} }
        @selected;
    $manifest->{selection} = {
        versions     => \%versions,                     grades => $self->{grades},
        generated_at => time2str( $self->{clock}->() ), source => $API,
        index_counts => \%index_counts,
        ids          => [ map { $_->{guid} } @selected ] };
    for my $row (@selected) {
        my $entry = $manifest->{reports}{ $row->{guid} } ||= {};
        %$entry = ( %$entry, %$row, url => "https://www.cpantesters.org/cpan/report/$row->{guid}" );
    }
    write_json( $manifest_file, $manifest );
    my ( $new, $saved, $pending ) = ( 0, 0, 0 );
    for my $row (@selected) {
        my $id            = $row->{guid};
        my $entry         = $manifest->{reports}{$id};
        my $path          = "/report/$id";
        my $cache         = $self->cache_file("$API$path");
        my $cached        = -f $cache ? read_json($cache) : undef;
        my $needs_request = !$cached
            || ( $cached->{missing} && $self->{clock}->() - $cached->{fetched_at} >= 3600 );
        if ( $needs_request && !$self->{offline} && $new >= $self->{max_reports} ) {
            $entry->{download_status} = 'pending';
            $pending++;
            next;
        }
        $new++ if $needs_request && !$self->{offline};
        my $report = $self->get_json(
            $path, immutable => 1, allow_missing => 1,
            validate => sub { validate_report( $_[0], $row ) } );
        if ($report) {
            my $text       = report_text($report);
            my $text_bytes = $text;
            utf8::encode($text_bytes);
            write_json( File::Spec->catfile( $self->{output}, 'reports', "$id.json" ), $report );
            write_bytes(
                File::Spec->catfile( $self->{output}, 'reports', "$id.txt" ),
                $text_bytes
            );
            $entry->{failure_lines} = [
                grep {
                    /Failed test|Failed \d+\/|No rule to make|(?:fatal )?error:|Error (?:code|executing)|Segmentation fault|Assertion .*failed/i
                } split /\n/,
                $text
            ];
            $entry->{download_status} = 'saved';
            $saved++;
        }
        else {
            $entry->{download_status} = 'not_available';
            $pending++;
        }
        write_json( $manifest_file, $manifest );
    }
    write_json( $manifest_file, $manifest );
    my @tsv = ( join "\t", qw(dist version grade perl osname platform guid download_status url) );
    for my $row (@selected) {
        my $entry = $manifest->{reports}{ $row->{guid} };
        push @tsv, join "\t",
            map { my $v = $entry->{$_} // ''; $v =~ s/[\t\r\n]/ /g; $v }
            qw(dist version grade perl osname platform guid download_status url);
    }
    my $tsv = join( "\n", @tsv ) . "\n";
    utf8::encode($tsv);
    write_bytes( File::Spec->catfile( $self->{output}, 'summary.tsv' ), $tsv );
    print "Selected "
        . scalar(@selected)
        . "; saved $saved; pending $pending; new report attempts $new\n";
    print "Reports and index: $self->{output}\n";
    if ( $self->{check} ) {
        my $triage = -f $self->{triage} ? read_json( $self->{triage} ) : {};
        my @unresolved;
        for my $row (@selected) {
            next if lc( $row->{grade} ) eq 'pass';
            my $id     = $row->{guid};
            my $review = $triage->{$id};
            next
                if $manifest->{reports}{$id}{download_status} eq 'saved'
                && ref($review) eq 'HASH'
                && ( $review->{status} || '' ) =~ /\A(?:fixed|external)\z/
                && !ref( $review->{reason} )
                && defined( $review->{reason} )
                && $review->{reason} =~ /\S/
                && ref( $review->{evidence} ) eq 'ARRAY'
                && @{ $review->{evidence} }
                && !( grep { ref($_) || !defined($_) || !/\S/ } @{ $review->{evidence} } );
            my $why =
                ref($review) eq 'HASH'
                ? ( $review->{reason} || 'incomplete review' )
                : 'unreviewed';
            $why = $manifest->{reports}{$id}{download_status}
                if $manifest->{reports}{$id}{download_status} ne 'saved';
            push @unresolved, "$row->{dist} $row->{version} $row->{grade} $id: $why\n"
                . "    https://www.cpantesters.org/cpan/report/$id";
        }
        if (@unresolved) {
            warn "Reports requiring review:\n" . join( '', map { "  $_\n" } @unresolved );
            return 2;
        }
        print "All selected non-pass reports have documented reviews.\n";
    }
    return 0;
}

unless (caller) {
    my $exit = eval { main(@ARGV) };
    if ($@) { warn $@; exit 1 }
    exit $exit;
}
1;
