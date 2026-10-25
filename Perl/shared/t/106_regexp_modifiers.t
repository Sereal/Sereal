#!perl
use strict;
use warnings;
use lib 't/lib';
use Test::More;
use Sereal::TestSet qw(Header short_string have_encoder_and_decoder);
use Sereal::Decoder;
use re qw(regexp_pattern);

# First stable releases: /msix are present in Perl 5.000, /p is 5.10,
# /dluaa is 5.14, /n is 5.22 and /xx is 5.26. Check the development
# introductions too: a reserved flag bit does not imply working /xx.
# Perl origin/blead commits: 87e95b7ff3 (/p), 94b03d7d32 (charset
# suffixes), 41d7c59e80 (/n), and 77c8f26370 (/xx).
my $charset       = $] >= 5.013010;
my $nocapture     = $] >= 5.021008;
my $extended_more = $] >= 5.025009;
my $both          = have_encoder_and_decoder();
my @cases         = (
    [ 'm',         '^b',           "a\nb",     1, 1 ],
    [ 's',         'a.b',          "a\nb",     1, 1 ],
    [ 'i',         'a',            'A',        1, 1 ],
    [ 'x',         'a b',          'ab',       1, 1 ],
    [ 'p',         'a',            'a',        1, $] >= 5.010 ],
    [ 'd',         '\\w',          "\xe9",     0, $charset ],
    [ 'u',         '\\w',          "\xe9",     1, $charset ],
    [ 'ui',        'foo\\xdf',     'fooss',    1, $charset ],
    [ 'l',         '[[:alpha:]]',  'a',        1, $charset ],
    [ 'a',         '\\w',          "\xe9",     0, $charset ],
    [ 'ai',        'k',            "\x{212a}", 1, $charset ],
    [ 'aai',       'k',            "\x{212a}", 0, $charset ],
    [ 'n',         '(a)',          'a',        1, $nocapture ],
    [ 'xx',        '[ a]',         ' ',        0, $extended_more ],
    [ 'xx',        '[ a]',         'a',        1, $extended_more ],
    [ 'xxx',       '[ a]',         ' ',        0, $extended_more ],
    [ 'uimsxnp',   '^(a) . b$',    "A\nb",     1, $charset && $nocapture ],
    [ 'aaimsxxnp', '^([ a]) . b$', "A\nb",     1, $extended_more ],
    [ '',          '(?i:a)',       'A',        1, 1 ],
);
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
