use 5.014;
use strict;
use warnings;
use Test::More;
use FindBin;
use JSON::PP qw(encode_json);
require "$FindBin::Bin/../../author_tools/check_release_ci.pl";
my $sha = 'a' x 40;
my $now = 0;

sub check {
    my (@responses) = @_;
    my @commands;
    eval {
        Sereal::ReleaseCI::wait_for_ci(
            $sha, timeout => 60, interval => 30,
            clock   => sub { $now }, sleep => sub { $now += $_[0] },
            command => sub {
                push @commands, [@_];
                die 'unexpected command' unless @responses;
                return encode_json( shift @responses );
            } );
    };
    return ( $@, \@commands );
}
my $success =
    [ { databaseId => 123, headSha => $sha, status => 'completed', conclusion => 'success' } ];
my ( $error, $commands ) = check(
    [], $success,
    { jobs => [ { name => 'Perl release checks', conclusion => 'success' } ] } );
is( $error, '', 'wait for the exact revision and require successful release aggregate' );
is( $now,   30, 'poll without hammering GitHub' );
ok( grep( $_ eq $sha, @{ $commands->[0] } ), 'query exact commit' );
($error) = check(
    [ { databaseId => 123, headSha => $sha, status => 'completed', conclusion => 'failure' } ] );
like( $error, qr/CI did not pass/, 'failed CI blocks release' );
($error) =
    check( $success, { jobs => [ { name => 'Perl release checks', conclusion => 'skipped' } ] } );
like( $error, qr/required.*did not pass/, 'skipped aggregate cannot approve release' );
($error) = check( $success, { jobs => [] } );
like(
    $error, qr/required.*did not pass/,
    'old workflow without new checks cannot approve release'
);
($error) = check(
    [ { databaseId => 999, headSha => 'b' x 40, status => 'completed', conclusion => 'success' } ],
    [], [] );
like( $error, qr/Timed out/, 'success for another commit cannot approve release' );
done_testing;
