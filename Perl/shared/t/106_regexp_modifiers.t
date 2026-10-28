#!perl
use strict;
use warnings;
use lib 't/lib';
use Test::More;
use File::Spec;
use Sereal::TestSet qw(Header short_string have_encoder_and_decoder regexp_modifier_test_cases);
use Sereal::Decoder;
use Sereal::TestSet::Corpus qw(read_packet);
use re qw(regexp_pattern);

# Live round trips need the encoder that first preserved these modifiers.
my $both = have_encoder_and_decoder('6.000004');
my @cases = regexp_modifier_test_cases();
my $corpus = File::Spec->catdir('t', 'data', 'packet_corpus');
my $decoder = Sereal::Decoder->new;

sub check_regexp {
    my ( $re, $original, $text, $matches, $name ) = @_;
    my ( undef, $want_flags ) = regexp_pattern($original);
    my ( undef, $got_flags )  = regexp_pattern($re);
    is(
        join( '', sort split //, $got_flags ), join( '', sort split //, $want_flags ),
        "$name preserves modifiers"
    );
    is( scalar( $text =~ $re ) ? 1 : 0, $matches, "$name preserves matching" );
    if ( $want_flags =~ /n/ ) {
        my @captures = $text =~ $re;
        is_deeply( \@captures, [1], "$name has no unnamed captures" );
    }
    if ( $want_flags =~ /p/ ) {
        $text =~ $re;
        ok( defined ${^MATCH}, "$name preserves /p match copy" );
    }
}

for my $version ( 1 .. 6 ) {
    for my $case (@cases) {
        my ( $modifiers, $pattern, $text, $matches, $supported ) = @$case;
        my $name   = "v$version /$modifiers ($pattern)";
        my $packet = Header($version) . "\x31" . short_string($pattern) . short_string($modifiers);
        if ( !$supported ) {
            my ( $missing, $required ) =
                  $modifiers =~ /xx/ ? ( 'xx', '5.26' )
                : $modifiers =~ /n/  ? ( 'n',  '5.22' )
                : $modifiers eq 'p' ? ( 'p', '5.10' )
                :                     ( substr( $modifiers, 0, 1 ), '5.14' );
            eval { $decoder->decode($packet) };
            like(
                $@,
                qr/Regexp modifier '\/\Q$missing\E' requires Perl \Q$required\E or later/,
                "$name reports the required Perl version"
            );
            next;
        }
        my $original = eval 'qr/' . $pattern . '/' . $modifiers;
        die "Cannot compile test regexp $name: $@" if $@;
        my $decoded = eval { $decoder->decode($packet) };
        is( $@, '', "$name decodes" );
        next if !$decoded;
        check_regexp( $decoded, $original, $text, $matches, "$name wire" );
        my $fixture = read_packet($corpus, 'regexp-modifiers',
            "v$version/" . unpack('H*', $modifiers) . '/' . unpack('H*', $pattern));
        check_regexp($decoder->decode($fixture), $original, $text, $matches, "$name corpus");
        if ($both) {
            my $encoder   = Sereal::Encoder->new( { protocol_version => $version } );
            my $roundtrip = $decoder->decode( $encoder->encode($original) );
            check_regexp( $roundtrip, $original, $text, $matches, "$name round trip" );
        }
    }
    for my $invalid ( 'z', 'g', 'c', 'e', 'r', 'o', "\0", 'aaa', 'ua', 'dl', 'dd', 'll', 'uu' ) {
        my $packet = Header($version) . "\x31" . short_string('a') . short_string($invalid);
        eval { $decoder->decode($packet) };
        like(
            $@,
            qr/(?:Unsupported regexp modifier byte|Invalid regexp modifier|Repeated regexp character set modifier|Conflicting regexp character set modifiers|requires Perl)/,
            "v$version rejects invalid modifier bytes " . unpack( 'H*', $invalid ) );
        is(
            $decoder->decode( Header($version) . "\x07" ), 7,
            "v$version decoder reusable after invalid modifiers"
        );
    }
}

{
    use locale;
    use feature 'unicode_strings';
    for my $modifiers ( '', 'd' ) {
        my $packet = Header(6) . "\x31" . short_string('\\w') . short_string($modifiers);
        my $re     = $decoder->decode($packet);
        my ( undef, $flags ) = regexp_pattern($re);
        unlike( $flags, qr/[lu]/, "default charset does not inherit caller pragmas (/$modifiers)" );
        ok( "\xe9" !~ $re, "default charset retains byte semantics (/$modifiers)" );
    }
}

done_testing;
