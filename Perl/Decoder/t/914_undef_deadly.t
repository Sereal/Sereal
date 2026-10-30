#!perl
use strict;
use warnings;
use File::Spec;
use lib File::Spec->catdir(qw(t lib));
BEGIN { lib->import('lib') if !-d 't' }
use Test::More;
use Scalar::Util qw(refaddr);
use IPC::Open3;
use Symbol                     qw(gensym);
use Sereal::TestSet            qw(Header varint array_slot_ref);
use Sereal::TestSet::Corpus    qw(read_packet);
use Sereal::Decoder            qw(decode_sereal);
use Sereal::Decoder::Constants qw(:all);

is( SRL_EXT_UNDEF_DEADLY, 0xff, 'UNDEF_DEADLY extension ID is 0xff' );
my $header = Header(6);
my $tag    = chr(SRL_HDR_EXTEND) . chr(SRL_EXT_UNDEF_DEADLY);
is( unpack( 'H*', $tag ), '3eff', 'wire sequence is 3e ff' );
my $message = qr/Encountered UNDEF_DEADLY extended tag/;

for my $options ( {}, { undef_deadly_policy => undef }, { undef_deadly_policy => 'undef' } ) {
    my @warnings;
    local $SIG{__WARN__} = sub { push @warnings, @_ };
    my $decoder = Sereal::Decoder->new($options);
    ok( !defined $decoder->decode( $header . $tag ), 'default policy returns undef' );
    is_deeply(
        $decoder->decode( $header . chr( SRL_HDR_ARRAYREF + 2 ) . $tag . $tag ),
        [ undef, undef ], 'default policy returns nested undef values'
    );
    is( scalar @warnings, 0, 'default policy is silent' );
}
for my $policy (qw(undef warn die)) {
    my @warnings;
    local $SIG{__WARN__} = sub { push @warnings, @_ };
    my $decoder = Sereal::Decoder->new( { undef_deadly_policy => $policy } );
    for my $body ( $tag, chr( SRL_HDR_ARRAYREF + 1 ) . $tag ) {
        @warnings = ();
        my $ok = eval { $decoder->decode( $header . $body ); 1 };
        if ( $policy eq 'die' ) {
            ok( !$ok, 'die policy rejects the tag' );
            like( $@, $message, 'die policy identifies the tag' );
        }
        else {
            ok( $ok, "$policy policy accepts the tag" ) or diag $@;
        }
        is( scalar @warnings, $policy eq 'warn' ? 1 : 0, "$policy policy warning count" );
        like( $warnings[0], $message, 'warning identifies the tag' ) if @warnings;
        is( $decoder->decode( $header . chr(7) ), 7, 'decoder remains reusable' );
    }
    @warnings = ();
    is(
        $decoder->decode( $header . chr( SRL_HDR_SHORT_BINARY + 2 ) . $tag ), $tag,
        "$policy policy treats tag bytes inside strings as data"
    );
    my $packet = $header . chr(7);
    is( $decoder->decode( $packet . $tag ), 7,     "$policy policy ignores trailing tag bytes" );
    is( $decoder->bytes_consumed, length($packet), 'trailing bytes do not alter framing' );
    is( scalar @warnings,         0,               'unparsed tag bytes do not warn' );
}
for my $invalid ( '', 'UNDEF', 'Warn', ' die', 'ignore', 0 ) {
    my $ok = eval { Sereal::Decoder->new( { undef_deadly_policy => $invalid } ); 1 };
    ok( !$ok, "invalid policy '$invalid' is rejected" );
    like( $@, qr/Invalid undef_deadly_policy/, 'invalid policy has a useful error' );
}
{
    my @warnings;
    local $SIG{__WARN__} = sub { push @warnings, @_ };
    ok(
        !defined decode_sereal( $header . $tag, { undef_deadly_policy => 'warn' } ),
        'functional interface accepts warn policy'
    );
    is( scalar @warnings, 1, 'functional interface warns' );
}
for my $use_undef ( 0, 1 ) {
    my $tracked = chr( SRL_HDR_EXTEND | SRL_HDR_TRACK_FLAG ) . chr(SRL_EXT_UNDEF_DEADLY);
    my $packet  = $header . chr( SRL_HDR_ARRAYREF + 2 ) . $tracked . chr(SRL_HDR_ALIAS) . varint(2);
    my $value   = Sereal::Decoder->new( { use_undef => $use_undef } )->decode($packet);
    is_deeply( $value, [ undef, undef ], "use_undef=$use_undef decodes a tracked sentinel" );
    is(
        refaddr( array_slot_ref($value, 0) ), refaddr( array_slot_ref($value, 1) ),
        'ALIAS targets the original undef scalar'
    );
}
{
    my $decoder = Sereal::Decoder->new( { undef_deadly_policy => 'warn' } );
    my @seen;
    my $nested_warning = '';
    local *STDERR;
    open STDERR, '>', \$nested_warning or die $!;
    local $SIG{__WARN__} = sub {
        push @seen, @_;
        if ( @seen == 1 ) {
            $decoder->decode( $header . $tag );
        }
    };
    ok( !defined $decoder->decode( $header . $tag ), 'warning handler can reenter the decoder' );

    # Perl suppresses __WARN__ hooks during a warning handler, so the nested
    # warning goes directly to STDERR.
    like( $nested_warning, $message, 'reentrant clone keeps the warn policy' );
}
{
    my $decoder = Sereal::Decoder->new( { undef_deadly_policy => 'warn' } );
    {
        local $SIG{__WARN__} = sub { die "fatal sentinel warning\n" };
        eval { $decoder->decode( $header . $tag ) };
        like( $@, qr/fatal sentinel warning/, 'fatal warning propagates' );
    }
    is( $decoder->decode( $header . chr(7) ), 7, 'decoder recovers after a fatal warning' );
}
{
    my $packet = $header . chr( SRL_HDR_ARRAYREF + 2 ) . $tag . chr(7);
    local $SIG{__WARN__} = sub { $packet = 'changed' x 4096 };
    eval { Sereal::Decoder->new( { undef_deadly_policy => 'warn' } )->decode($packet) };
    like(
        $@, qr/input scalar was modified by a callback/,
        'warning handler cannot invalidate the input buffer'
    );
}
{
    my $decoder = Sereal::Decoder->new( { undef_deadly_policy => 'die' } );
    my $packet  = Header( 6, $tag ) . chr(7);
    is( $decoder->decode($packet), 7, 'unrequested header values are not encountered' );
    eval { $decoder->decode_only_header($packet) };
    like( $@, $message, 'policy applies when header data is decoded' );
}
for my $body ( chr(SRL_HDR_EXTEND), chr(SRL_HDR_EXTEND) . chr(0) ) {
    eval { Sereal::Decoder->new->decode( $header . $body ) };
    like(
        $@, qr/Unexpected termination|Unknown extended tag ID 0/,
        'truncated and unassigned extensions are rejected'
    );
}
eval { Sereal::Decoder->new->decode( Header(5) . $tag ) };
like( $@, qr/EXTEND tag requires protocol version 6/, 'sentinel requires protocol v6' );
# Generated with Sereal::Encoder 6; this keeps the decoder test independent
# of the installed encoder version.
my $undef_packet = read_v6_corpus('undef');
isnt( $undef_packet, $header . $tag, 'compliant encoder never emits the sentinel for undef' );
{
    my ( $in, $out );
    my $err = gensym;
    my $pid = open3( $in, $out, $err, $^X, '-Mblib', 'author_tools/hobodecoder.pl' );
    binmode $in;
    print {$in} $header . $tag;
    close $in;
    my $stdout = do { local $/; <$out> };
    my $stderr = do { local $/; <$err> };
    waitpid( $pid, 0 );
    is( $?, 0, 'hobodecoder can inspect the sentinel' ) or diag $stderr;
    like( $stdout, qr/UNDEF_DEADLY.*undef/, 'hobodecoder displays its name and undef meaning' );
}
done_testing;

sub read_v6_corpus {
    my ($name) = @_;
    return read_packet(
        File::Spec->catdir( 't', 'data', 'packet_corpus' ),
        'decoder-regressions', "v6/$name"
    );
}
