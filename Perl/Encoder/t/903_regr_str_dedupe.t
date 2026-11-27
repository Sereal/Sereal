#!perl
use strict;
use warnings;
use File::Spec;
use Scalar::Util qw(refaddr);
use Test::More;

use lib File::Spec->catdir(qw(t lib));
BEGIN { lib->import('lib') if !-d 't' }

use Sereal::TestSet            qw(have_encoder_and_decoder);
use Sereal::Encoder            qw(encode_sereal_with_header_data);
use Sereal::Encoder::Constants qw(:all);

plan skip_all => 'Did not find right version of decoder'
    if !have_encoder_and_decoder();

sub make_utf8_same_bytes {
    my ($raw) = @_;
    my $s = $raw;
    utf8::decode($s) or die "failed to decode utf8 test string";
    die "expected utf8 flag on" unless utf8::is_utf8($s);
    return $s;
}

# 1. Repeated strings across len 0..3, 4..31 (SHORT_BINARY), >= 32 (BINARY),
#    and STR_UTF8 across protocol versions V1, V2, V6, V7.
for my $version ( 1, 2, 6, 7 ) {
    for my $opt (qw(dedupe_strings aliased_dedupe_strings)) {
        my $s0    = "";
        my $s1    = "a";
        my $s2    = "ab";
        my $s3    = "abc";
        my $short = "short_bin_12";                 # len 12 (SHORT_BINARY)
        my $med   = "medium_str_0123456789_abc";    # len 25 (SHORT_BINARY)
        my $long  = "long_binary_payload_" x 3;     # len 60 (BINARY)
        my $utf_s = "\x{100}\x{200}\x{300}";        # len 6 bytes, STR_UTF8
        my $utf_l = ( "\x{2603}_snowman_" x 5 );    # >= 32 bytes, STR_UTF8

        my $input = [
            $s0,    $s0,  $s1,   $s1,    $s2, $s2, $s3, $s3,
            $short, $med, $long, $utf_s, $utf_l,
            $short, $med, $long, $utf_s, $utf_l,
            $short, $med, $long, $utf_s, $utf_l,
        ];

        my $enc   = Sereal::Encoder->new( { protocol_version => $version, $opt => 1 } );
        my $plain = Sereal::Encoder->new( { protocol_version => $version } );

        my $pkt       = $enc->encode($input);
        my $plain_pkt = $plain->encode($input);

        ok(
            length($pkt) < length($plain_pkt),
            "v$version $opt: deduped packet ("
                . length($pkt)
                . "B) smaller than plain ("
                . length($plain_pkt) . "B)"
        );

        my $dec = Sereal::Decoder->new( { validate_utf8 => 1 } )->decode($pkt);
        is_deeply( $dec, $input, "v$version $opt: repeated lengths 0..60 + UTF-8 roundtrip" );

        # Lengths <= 3 are not deduped/aliased as scalar values
        isnt( refaddr( \$dec->[4] ), refaddr( \$dec->[5] ), "v$version $opt: len=2 not aliased" );
        isnt( refaddr( \$dec->[6] ), refaddr( \$dec->[7] ), "v$version $opt: len=3 not aliased" );

        # Lengths > 3 aliasing semantics
        for my $pair ( [ 8, 13 ], [ 9, 14 ], [ 10, 15 ], [ 11, 16 ], [ 12, 17 ], [ 8, 18 ] ) {
            my ( $i, $j ) = @$pair;
            if ( $opt eq 'aliased_dedupe_strings' ) {
                is(
                    refaddr( \$dec->[$i] ), refaddr( \$dec->[$j] ),
                    "v$version $opt: indices $i and $j are aliased"
                );
            }
            else {
                isnt(
                    refaddr( \$dec->[$i] ), refaddr( \$dec->[$j] ),
                    "v$version $opt: indices $i and $j are independent copies"
                );
            }
        }
        ok(
            utf8::is_utf8( $dec->[11] ) && utf8::is_utf8( $dec->[16] ),
            "v$version $opt: short UTF-8 flag preserved"
        );
        ok(
            utf8::is_utf8( $dec->[12] ) && utf8::is_utf8( $dec->[17] ),
            "v$version $opt: long UTF-8 flag preserved"
        );
        ok(
            !utf8::is_utf8( $dec->[8] ) && !utf8::is_utf8( $dec->[10] ),
            "v$version $opt: binary flags stay off"
        );
    }
}

# 2. UTF-8 vs binary separation (identical raw bytes with differing SvUTF8,
#    including both multi-byte UTF-8 and upgraded ASCII strings)
for my $version ( 1, 6 ) {
    for my $opt (qw(dedupe_strings aliased_dedupe_strings)) {
        my $bin4  = "\xc3\xa9\xc3\xa9";              # 4 raw bytes, SvUTF8 OFF
        my $utf4  = make_utf8_same_bytes($bin4);     # exact same 4 raw bytes, SvUTF8 ON
        my $bin40 = "\xc3\xa9" x 20;                 # 40 raw bytes, SvUTF8 OFF
        my $utf40 = make_utf8_same_bytes($bin40);    # exact same 40 raw bytes, SvUTF8 ON
        my $bin_k = "\xc3\xa9key";                   # 5 raw bytes, SvUTF8 OFF
        my $utf_k = make_utf8_same_bytes($bin_k);    # exact same 5 raw bytes, SvUTF8 ON
        my $asc_b = "ascii_same_bytes";
        my $asc_u = "ascii_same_bytes";
        utf8::upgrade($asc_u);

        my $input = [
            $bin4,  $utf4,  $bin4,  $utf4,
            $bin40, $utf40, $bin40, $utf40,
            { $bin_k => $bin_k },
            { $utf_k => $utf_k },
            $bin_k, $utf_k,
            $asc_b, $asc_u, $asc_b, $asc_u,
        ];

        my $enc =
            Sereal::Encoder->new( { protocol_version => $version, $opt => 1, sort_keys => 1 } );
        my $pkt = $enc->encode($input);
        my $dec = Sereal::Decoder->new( { validate_utf8 => 1 } )->decode($pkt);

        # Check exact string values AND UTF-8 flags independently
        is( length( $dec->[0] ), 4, "v$version $opt: bin4 byte length" );
        ok( !utf8::is_utf8( $dec->[0] ), "v$version $opt: bin4 SvUTF8 OFF" );
        is( $dec->[0], $bin4, "v$version $opt: bin4 value" );

        ok( utf8::is_utf8( $dec->[1] ), "v$version $opt: utf4 SvUTF8 ON" );
        is( $dec->[1], $utf4, "v$version $opt: utf4 value" );

        ok( !utf8::is_utf8( $dec->[2] ), "v$version $opt: repeated bin4 SvUTF8 OFF" );
        ok( utf8::is_utf8( $dec->[3] ),  "v$version $opt: repeated utf4 SvUTF8 ON" );

        ok( !utf8::is_utf8( $dec->[4] ), "v$version $opt: bin40 SvUTF8 OFF" );
        ok( utf8::is_utf8( $dec->[5] ),  "v$version $opt: utf40 SvUTF8 ON" );
        is( $dec->[4], $bin40, "v$version $opt: bin40 value" );
        is( $dec->[5], $utf40, "v$version $opt: utf40 value" );

        ok( !utf8::is_utf8( $dec->[10] ), "v$version $opt: bin_k value SvUTF8 OFF" );
        ok( utf8::is_utf8( $dec->[11] ),  "v$version $opt: utf_k value SvUTF8 ON" );

        ok( !utf8::is_utf8( $dec->[12] ), "v$version $opt: ascii binary SvUTF8 OFF" );
        ok( utf8::is_utf8( $dec->[13] ),  "v$version $opt: ascii upgraded SvUTF8 ON" );
        ok( !utf8::is_utf8( $dec->[14] ), "v$version $opt: repeated ascii binary SvUTF8 OFF" );
        ok( utf8::is_utf8( $dec->[15] ),  "v$version $opt: repeated ascii upgraded SvUTF8 ON" );

        isnt(
            refaddr( \$dec->[0] ), refaddr( \$dec->[1] ),
            "v$version $opt: bin4 != utf4 refaddr"
        );
        isnt(
            refaddr( \$dec->[4] ), refaddr( \$dec->[5] ),
            "v$version $opt: bin40 != utf40 refaddr"
        );
        isnt(
            refaddr( \$dec->[10] ), refaddr( \$dec->[11] ),
            "v$version $opt: bin_k != utf_k refaddr"
        );
        isnt(
            refaddr( \$dec->[12] ), refaddr( \$dec->[13] ),
            "v$version $opt: ascii bin != ascii utf8 refaddr"
        );

        if ( $opt eq 'aliased_dedupe_strings' ) {
            is( refaddr( \$dec->[0] ), refaddr( \$dec->[2] ), "v$version $opt: bin4 aliases bin4" );
            is( refaddr( \$dec->[1] ), refaddr( \$dec->[3] ), "v$version $opt: utf4 aliases utf4" );
            is(
                refaddr( \$dec->[4] ), refaddr( \$dec->[6] ),
                "v$version $opt: bin40 aliases bin40"
            );
            is(
                refaddr( \$dec->[5] ), refaddr( \$dec->[7] ),
                "v$version $opt: utf40 aliases utf40"
            );
            is(
                refaddr( \$dec->[12] ), refaddr( \$dec->[14] ),
                "v$version $opt: ascii bin aliases ascii bin"
            );
            is(
                refaddr( \$dec->[13] ), refaddr( \$dec->[15] ),
                "v$version $opt: ascii utf8 aliases ascii utf8"
            );
        }
    }
}

# 3. Interleaved shared hash keys and allow_alias ordering edge cases:
#    (a) hash key -> scalar(allow_alias=1) -> scalar(allow_alias=1)
#    (b) scalar(allow_alias=0) -> scalar(allow_alias=1) -> scalar(allow_alias=1)
#    (b2) hash key -> scalar(allow_alias=0) -> scalar(allow_alias=1) -> scalar(allow_alias=0)
#    (c) scalar(allow_alias=1) -> hash key -> scalar(allow_alias=1)
for my $version ( 1, 6 ) {
    my $shared_val = "shared_key_and_value_string";
    my $utf_key    = "utf8_key_\x{263a}_shared";
    my $short_key  = "k2";
    my $empty_key  = "";

    my $enc_dedupe = Sereal::Encoder->new(
        { protocol_version => $version, dedupe_strings => 1, sort_keys => 1 } );
    my $enc_alias = Sereal::Encoder->new(
        { protocol_version => $version, aliased_dedupe_strings => 1, sort_keys => 1 } );
    my $enc_plain = Sereal::Encoder->new( { protocol_version => $version, sort_keys => 1 } );

    # (a) Hash key first, then repeated scalar values (including empty and short keys)
    {
        my $data = [ {
                $shared_val => $shared_val, $utf_key => $utf_key, $short_key => $short_key,
                $empty_key  => "e1"
            },
            $shared_val,
            $utf_key,
            $short_key,
            {
                $shared_val => "other", $utf_key => "other2", $short_key => "other3",
                $empty_key  => "e2"
            },
        ];

        my $pkt_dedupe = $enc_dedupe->encode($data);
        my $pkt_alias  = $enc_alias->encode($data);
        my $pkt_plain  = $enc_plain->encode($data);

        is(
            ( () = $pkt_dedupe =~ /\Q$shared_val\E/g ), 1,
            "v$version (a) dedupe: shared_val literal once"
        );
        is(
            ( () = $pkt_alias =~ /\Q$shared_val\E/g ), 1,
            "v$version (a) aliased: shared_val literal once"
        );
        my $utf_bytes = $utf_key;
        utf8::encode($utf_bytes);
        is(
            ( () = $pkt_dedupe =~ /\Q$utf_bytes\E/g ), 1,
            "v$version (a) dedupe: utf_key literal once"
        );
        is(
            ( () = $pkt_alias =~ /\Q$utf_bytes\E/g ), 1,
            "v$version (a) aliased: utf_key literal once"
        );
        ok( length($pkt_alias) < length($pkt_plain), "v$version (a) aliased smaller than plain" );

        my $dec_alias = Sereal::Decoder->new( { validate_utf8 => 1 } )->decode($pkt_alias);
        is_deeply( $dec_alias, $data, "v$version (a) aliased: roundtrip" );
        is(
            refaddr( \$dec_alias->[0]{$shared_val} ), refaddr( \$dec_alias->[1] ),
            "v$version (a) aliased: 2nd scalar aliases 1st scalar (which COPYed hash key)"
        );
        is(
            refaddr( \$dec_alias->[0]{$utf_key} ), refaddr( \$dec_alias->[2] ),
            "v$version (a) aliased: 2nd UTF-8 scalar aliases 1st scalar"
        );
        $dec_alias->[1] = "mutated";
        is(
            $dec_alias->[0]{$shared_val}, "mutated",
            "v$version (a): aliased scalar values share SV"
        );
        ok( exists $dec_alias->[0]{$shared_val}, "v$version (a): hash key remains intact" );

        my $dec_dedupe = Sereal::Decoder->new( { validate_utf8 => 1 } )->decode($pkt_dedupe);
        is_deeply( $dec_dedupe, $data, "v$version (a) dedupe: roundtrip" );
        isnt(
            refaddr( \$dec_dedupe->[0]{$shared_val} ), refaddr( \$dec_dedupe->[1] ),
            "v$version (a) dedupe: scalar values are independent COPYs"
        );
    }

    # (b) Scalar (allow_alias=0 via \$a) first, then scalar(allow_alias=1), then scalar(allow_alias=1)
    {
        my $a    = $shared_val;
        my $b    = $shared_val;
        my $data = [ \$a, $shared_val, $shared_val, \$b ];
        my $pkt  = $enc_alias->encode($data);
        is( ( () = $pkt =~ /\Q$shared_val\E/g ), 1, "v$version (b) aliased: literal once" );
        my $dec = Sereal::Decoder->new->decode($pkt);
        is_deeply( $dec, $data, "v$version (b) aliased: roundtrip" );
        is(
            refaddr( $dec->[0] ), refaddr( \$dec->[1] ),
            "v$version (b): 2nd scalar aliases 1st referent"
        );
        is(
            refaddr( $dec->[0] ), refaddr( \$dec->[2] ),
            "v$version (b): 3rd scalar aliases 1st referent"
        );
        isnt(
            refaddr( $dec->[0] ), refaddr( $dec->[3] ),
            "v$version (b): 2nd REFN keeps distinct referent"
        );
    }

    # (b2) Hash key -> scalar(allow_alias=0 via \$a) -> scalar(allow_alias=1) -> scalar(allow_alias=0 via \$b)
    {
        my $a    = $shared_val;
        my $b    = $shared_val;
        my $data = [ { $shared_val => 1 }, \$a, $shared_val, \$b, $shared_val ];
        my $pkt  = $enc_alias->encode($data);
        is( ( () = $pkt =~ /\Q$shared_val\E/g ), 1, "v$version (b2) aliased: literal once" );
        my $dec = Sereal::Decoder->new->decode($pkt);
        is_deeply( $dec, $data, "v$version (b2) aliased: roundtrip" );
        is(
            refaddr( $dec->[1] ), refaddr( \$dec->[2] ),
            "v$version (b2): scalar aliases \$a referent (COPY of key)"
        );
        is(
            refaddr( $dec->[1] ), refaddr( \$dec->[4] ),
            "v$version (b2): later scalar still aliases \$a referent"
        );
        isnt(
            refaddr( $dec->[1] ), refaddr( $dec->[3] ),
            "v$version (b2): \$b stays a distinct referent"
        );
    }

    # (c) Scalar(allow_alias=1) first -> hash key -> scalar(allow_alias=1)
    {
        my $val_first = [ $shared_val, { $shared_val => 1 }, $shared_val ];
        my $vf_pkt    = $enc_alias->encode($val_first);
        is(
            ( () = $vf_pkt =~ /\Q$shared_val\E/g ), 1,
            "v$version (c) aliased: val-first literal once"
        );
        my $vf_dec = Sereal::Decoder->new->decode($vf_pkt);
        is_deeply( $vf_dec, $val_first, "v$version (c) aliased: val-first roundtrip" );
        is(
            refaddr( \$vf_dec->[0] ), refaddr( \$vf_dec->[2] ),
            "v$version (c) aliased: val-first aliases"
        );
    }

    # no_shared_hashkeys + dedupe_strings: keys are NOT shared, scalars ARE deduped
    {
        my $enc_noshare = Sereal::Encoder->new( {
            protocol_version   => $version,
            dedupe_strings     => 1,
            no_shared_hashkeys => 1,
            sort_keys          => 1,
        } );
        my $k    = "unshared_key_string";
        my $v    = "deduped_scalar_value";
        my $data = [ { $k => $v }, { $k => $v }, $v ];
        my $pkt  = $enc_noshare->encode($data);
        is( ( () = $pkt =~ /\Q$k\E/g ), 2, "v$version no_shared_hashkeys: key appears twice" );
        is(
            ( () = $pkt =~ /\Q$v\E/g ), 1,
            "v$version no_shared_hashkeys: scalar value still deduped"
        );
        is_deeply(
            Sereal::Decoder->new->decode($pkt), $data,
            "v$version no_shared_hashkeys: roundtrip"
        );
    }
}

# 4. User header data (encode_sereal_with_header_data & OO $enc->encode($body, $hdr))
#    Verify independent dedupe tables between header and body, and valid offsets >= 1
#    even when header starts immediately with a deduplicated string at offset 1.
for my $version ( 2, 6, 7 ) {
    for my $opt (qw(dedupe_strings aliased_dedupe_strings)) {
        my $shared = "shared_between_header_and_body";
        my $hdr_u  = "header_utf8_\x{2603}_repeated";
        my $body_u = "body_utf8_\x{2603}_repeated";

        my $hdr  = [ $shared, $hdr_u,  $shared, $hdr_u,  { $shared => $shared } ];
        my $body = [ $shared, $body_u, $shared, $body_u, { $shared => $shared } ];

        # Functional API
        my $pkt = encode_sereal_with_header_data(
            $body, $hdr,
            { protocol_version => $version, $opt => 1, sort_keys => 1 } );
        my $dec      = Sereal::Decoder->new( { validate_utf8 => 1 } );
        my $got_hdr  = $dec->decode_only_header($pkt);
        my $got_body = $dec->decode($pkt);
        my $got_both = $dec->decode_with_header($pkt);

        is_deeply( $got_hdr,  $hdr,            "v$version $opt func header: decode_only_header" );
        is_deeply( $got_body, $body,           "v$version $opt func header: decode body" );
        is_deeply( $got_both, [ $hdr, $body ], "v$version $opt func header: decode_with_header" );

        # Literal bytes for $shared appear exactly twice (once in header, once in body)
        is(
            ( () = $pkt =~ /\Q$shared\E/g ), 2,
            "v$version $opt func header: shared string once in header + once in body"
        );

        if ( $opt eq 'aliased_dedupe_strings' ) {
            is(
                refaddr( \$got_hdr->[0] ), refaddr( \$got_hdr->[2] ),
                "v$version $opt: header strings aliased within header"
            );
            is(
                refaddr( \$got_body->[0] ), refaddr( \$got_body->[2] ),
                "v$version $opt: body strings aliased within body"
            );
            isnt(
                refaddr( \$got_both->[0][0] ), refaddr( \$got_both->[1][0] ),
                "v$version $opt: header and body never alias each other"
            );
        }

        # OO API reusing encoder across header+body calls
        my $enc =
            Sereal::Encoder->new( { protocol_version => $version, $opt => 1, sort_keys => 1 } );
        for my $call ( 1 .. 3 ) {
            my $p = $enc->encode( $body, $hdr );
            my $b = $dec->decode_with_header($p);
            is_deeply( $b, [ $hdr, $body ], "v$version $opt OO header call $call: roundtrip" );
        }
    }
}

# 5. Buffer reallocation stress (50,000 strings / > 2 MiB)
{
    my @stress;
    my @unique_pool = map {
        $_ % 3 == 0
            ? sprintf( "short_%04d", $_ )                                                # SHORT_BINARY
            : $_ % 3 == 1 ? sprintf( "long_payload_string_%04d_", $_ ) . ( "x" x 30 )    # BINARY
            : sprintf( "utf8_\x{100}_%04d_", $_ ) . ( "\x{2603}" x 5 )                   # STR_UTF8
    } 0 .. 1999;

    for my $i ( 0 .. 49999 ) {
        if ( $i < 2000 ) {
            push @stress, $unique_pool[$i];
        }
        elsif ( $i % 5 == 0 ) {
            push @stress, sprintf( "tail_unique_string_item_%06d_padding_bytes", $i );
        }
        else {
            push @stress, $unique_pool[ $i % 2000 ];
        }
    }

    for my $opt (qw(dedupe_strings aliased_dedupe_strings)) {
        my $enc = Sereal::Encoder->new( { $opt => 1 } );
        my $pkt = $enc->encode( \@stress );
        ok( length($pkt) > 100_000, "stress $opt: large payload encoded (" . length($pkt) . "B)" );

        my $big_unique =
            [ map { sprintf( "big_unique_entry_%05d_", $_ ) . ( "Z" x 40 ) } 0 .. 49999 ];
        my $big_pkt = $enc->encode($big_unique);
        ok(
            length($big_pkt) > 2 * 1024 * 1024,
            "stress $opt: >2 MiB payload (" . length($big_pkt) . "B)"
        );

        my $dec1 = Sereal::Decoder->new( { validate_utf8 => 1 } )->decode($pkt);
        is( scalar(@$dec1), 50000, "stress $opt: 50k element count" );
        is_deeply( $dec1, \@stress, "stress $opt: 50k element roundtrip" );
        if ( $opt eq 'aliased_dedupe_strings' ) {
            is(
                refaddr( \$dec1->[1] ), refaddr( \$dec1->[48001] ),
                "stress aliased: head/tail binary alias"
            );
            is(
                refaddr( \$dec1->[2] ), refaddr( \$dec1->[48002] ),
                "stress aliased: head/tail utf8 alias"
            );
            is(
                refaddr( \$dec1->[1] ), refaddr( \$dec1->[24001] ),
                "stress aliased: head/mid binary alias"
            );
        }

        my $dec2 = Sereal::Decoder->new->decode($big_pkt);
        is( scalar(@$dec2), 50000,                "stress $opt: >2 MiB 50k unique count" );
        is( $dec2->[0],     $big_unique->[0],     "stress $opt: >2 MiB first elem" );
        is( $dec2->[25000], $big_unique->[25000], "stress $opt: >2 MiB mid elem" );
        is( $dec2->[49999], $big_unique->[49999], "stress $opt: >2 MiB last elem" );
    }
}

# 6. SvGMAGICAL tied scalar returning strings
{

    package RegrDedupeTied;
    sub TIESCALAR { bless { vals => $_[1], idx => 0 }, $_[0] }

    sub FETCH {
        my $self = shift;
        my $v    = $self->{vals}[ $self->{idx}++ ];
        return $v;
    }
}
{
    for my $opt (qw(dedupe_strings aliased_dedupe_strings)) {
        tie my $t1, 'RegrDedupeTied', [ "tied_repeated_val",  "tied_other" ];
        tie my $t2, 'RegrDedupeTied', [ "tied_repeated_val",  "tied_other" ];
        tie my $t3, 'RegrDedupeTied', [ "tied_utf8_\x{2603}", "tied_other" ];
        tie my $t4, 'RegrDedupeTied', [ "tied_utf8_\x{2603}", "tied_other" ];

        my $enc = Sereal::Encoder->new( { $opt => 1 } );
        my $pkt = $enc->encode( [ $t1, $t2, $t3, $t4 ] );
        my $dec = Sereal::Decoder->new( { validate_utf8 => 1 } )->decode($pkt);
        is_deeply(
            $dec,
            [
                "tied_repeated_val", "tied_repeated_val", "tied_utf8_\x{2603}", "tied_utf8_\x{2603}"
            ],
            "tied scalar $opt: FETCH called once per element and deduped"
        );
        if ( $opt eq 'aliased_dedupe_strings' ) {
            is( refaddr( \$dec->[0] ), refaddr( \$dec->[1] ), "tied $opt: binary aliased" );
            is( refaddr( \$dec->[2] ), refaddr( \$dec->[3] ), "tied $opt: utf8 aliased" );
        }
    }
}

# 7. Re-entrant FREEZE callback invoking the SAME $enc with dedupe_strings / aliased_dedupe_strings
our $REENTRANT_ENC;
{

    package RegrDedupeReentrant;

    sub FREEZE {
        my ($self) = @_;

        # Re-entrantly encode a large structure with overlapping AND distinct strings
        my @inner = (
            $self->{shared},
            $self->{shared},
            ( map { "inner_only_string_$_" } 1 .. 400 ),
            $self->{shared},
            "inner_utf8_\x{2603}_dup",
            "inner_utf8_\x{2603}_dup",
        );
        return $main::REENTRANT_ENC->encode( \@inner );
    }

    sub THAW {
        my ( $class, $ser, $pkt ) = @_;
        my $decoded = Sereal::Decoder->new( { validate_utf8 => 1 } )->decode($pkt);
        return bless { decoded => $decoded }, $class;
    }

    package RegrDedupeDieFreeze;

    sub FREEZE {
        my ($self) = @_;
        if ( $self->{reenter} ) {
            my $inner = $main::REENTRANT_ENC->encode( [ "inner_before_die", "inner_before_die" ] );
            die "boom after re-entrant encode (" . length($inner) . ")";
        }
        die "boom inside FREEZE";
    }
}
{
    for my $opt (qw(dedupe_strings aliased_dedupe_strings)) {
        my $enc = Sereal::Encoder->new( { $opt => 1, freeze_callbacks => 1 } );
        local $REENTRANT_ENC = $enc;

        my $shared     = "outer_and_inner_shared_string";
        my $outer_only = "outer_only_deduped_string";
        my $obj        = bless { shared => $shared }, 'RegrDedupeReentrant';
        my $data       = [
            $shared,
            $outer_only,
            $obj,
            $shared,
            $outer_only,
            "outer_utf8_\x{2603}_dup",
            "outer_utf8_\x{2603}_dup",
        ];

        my $pkt = $enc->encode($data);
        my $dec = Sereal::Decoder->new( { validate_utf8 => 1 } )->decode($pkt);

        is( $dec->[0], $shared,                   "reentrant $opt: outer[0] matches" );
        is( $dec->[1], $outer_only,               "reentrant $opt: outer[1] matches" );
        is( $dec->[3], $shared,                   "reentrant $opt: outer[3] matches after FREEZE" );
        is( $dec->[4], $outer_only,               "reentrant $opt: outer[4] matches after FREEZE" );
        is( $dec->[5], "outer_utf8_\x{2603}_dup", "reentrant $opt: outer utf8[5]" );
        is( $dec->[6], "outer_utf8_\x{2603}_dup", "reentrant $opt: outer utf8[6]" );

        my $inner = $dec->[2]{decoded};
        is( scalar(@$inner), 405,                       "reentrant $opt: inner decoded length" );
        is( $inner->[0],     $shared,                   "reentrant $opt: inner[0]" );
        is( $inner->[1],     $shared,                   "reentrant $opt: inner[1]" );
        is( $inner->[402],   $shared,                   "reentrant $opt: inner[402]" );
        is( $inner->[403],   "inner_utf8_\x{2603}_dup", "reentrant $opt: inner utf8" );

        if ( $opt eq 'aliased_dedupe_strings' ) {
            is(
                refaddr( \$dec->[0] ), refaddr( \$dec->[3] ),
                "reentrant aliased: outer shared aliased across FREEZE"
            );
            is(
                refaddr( \$dec->[1] ), refaddr( \$dec->[4] ),
                "reentrant aliased: outer_only aliased across FREEZE"
            );
            is(
                refaddr( \$inner->[0] ), refaddr( \$inner->[1] ),
                "reentrant aliased: inner strings aliased independently"
            );
            is(
                refaddr( \$inner->[0] ), refaddr( \$inner->[402] ),
                "reentrant aliased: inner head/tail aliased"
            );
        }
    }
}

# 8. Post-croak recovery on reusable encoder (croak_on_bless, FREEZE dying, header croak)
{
    for my $opt (qw(dedupe_strings aliased_dedupe_strings)) {

        # 8a. croak_on_bless mid-encode after populating str_dedupe_tbl
        my $enc = Sereal::Encoder->new( { $opt => 1, croak_on_bless => 1 } );
        my @pre = map { "pre_croak_string_" . ( $_ % 200 ) } 1 .. 600;
        my $ok  = eval {
            $enc->encode( [ @pre, bless( { a => 1 }, 'ForbiddenClass' ), @pre ] );
            1;
        };
        ok( !$ok, "post-croak $opt: croak_on_bless died as expected" );
        like( $@, qr/croak_on_bless/, "post-croak $opt: expected error message" );

        # Immediately reuse the same encoder with overlapping and fresh strings
        my $after = [
            "pre_croak_string_1", "fresh_after_croak", "pre_croak_string_1",
            "fresh_after_croak"
        ];
        my $pkt = $enc->encode($after);
        my $dec = Sereal::Decoder->new->decode($pkt);
        is_deeply( $dec, $after, "post-croak $opt: reusable encoder clean after croak_on_bless" );
        if ( $opt eq 'aliased_dedupe_strings' ) {
            is(
                refaddr( \$dec->[0] ), refaddr( \$dec->[2] ),
                "post-croak aliased: overlapping string aliases cleanly"
            );
            is(
                refaddr( \$dec->[1] ), refaddr( \$dec->[3] ),
                "post-croak aliased: fresh string aliases cleanly"
            );
        }

        # 8b. croak_on_bless while encoding user header data
        $ok = eval {
            $enc->encode(
                [ "body_after_hdr_croak", "body_after_hdr_croak" ],
                [ "hdr_before_croak",     "hdr_before_croak", bless( {}, 'ForbiddenHdr' ) ] );
            1;
        };
        ok( !$ok, "post-croak $opt: header croak_on_bless died as expected" );
        my $pkt_after_hdr = $enc->encode(
            [ "hdr_before_croak", "hdr_before_croak" ],
            [ "hdr_before_croak", "hdr_before_croak" ] );
        my $dec_after_hdr = Sereal::Decoder->new->decode_with_header($pkt_after_hdr);
        is_deeply(
            $dec_after_hdr,
            [
                [ "hdr_before_croak", "hdr_before_croak" ],
                [ "hdr_before_croak", "hdr_before_croak" ]
            ],
            "post-croak $opt: reusable encoder clean after header croak"
        );

        # 8c. FREEZE dying mid-encode (both direct and after re-entrant encode)
        my $fenc = Sereal::Encoder->new( { $opt => 1, freeze_callbacks => 1 } );
        local $REENTRANT_ENC = $fenc;
        for my $reenter ( 0, 1 ) {
            my $fok = eval {
                $fenc->encode( [
                    "before_freeze_die",
                    "before_freeze_die",
                    bless( { reenter => $reenter }, 'RegrDedupeDieFreeze' ),
                    "after_freeze_die",
                ] );
                1;
            };
            ok( !$fok, "post-croak $opt: FREEZE die (reenter=$reenter) died as expected" );
            my $fpkt =
                $fenc->encode( [ "before_freeze_die", "inner_before_die", "before_freeze_die" ] );
            my $fdec = Sereal::Decoder->new->decode($fpkt);
            is_deeply(
                $fdec,
                [ "before_freeze_die", "inner_before_die", "before_freeze_die" ],
                "post-croak $opt: reusable encoder clean after FREEZE die (reenter=$reenter)"
            );
        }
    }
}

# 9. Reusable $enc->encode(...) across alternating giant, medium, and tiny payloads
#    to exercise table growth, clearing, and proportional shrinking.
{
    for my $opt (qw(dedupe_strings aliased_dedupe_strings)) {
        my $enc   = Sereal::Encoder->new( { $opt => 1 } );
        my @sizes = ( 50, 6000, 10, 300, 6000, 0, 250, 100, 5000, 15 );
        for my $round ( 0 .. $#sizes ) {
            my $n      = $sizes[$round];
            my $prefix = "round_${round}_unique_";
            my @items  = map {
                my $s = $prefix . $_ . ( ( $_ % 2 ) ? "_\x{100}" : "_bin" );
                ( $s, $s );
            } 1 .. $n;
            my $pkt = $enc->encode( \@items );
            my $dec = Sereal::Decoder->new( { validate_utf8 => 1 } )->decode($pkt);
            is_deeply( $dec, \@items, "reuse $opt round $round (n=$n): clean roundtrip" );
        }
    }
}

done_testing();
