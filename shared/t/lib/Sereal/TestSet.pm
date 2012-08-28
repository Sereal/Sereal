package # Hide from PAUSE
  Sereal::TestSet;

use strict;
use warnings;

use File::Spec;
use Scalar::Util qw(weaken);
use Test::More;
use Test::LongString;
use Data::Dumper;

# Dynamically load constants from whatever is being tested
our ($Class, $ConstClass);
BEGIN {
  if (defined $INC{"Sereal/Encoder.pm"}
      and $INC{"Sereal/Encoder.pm"} =~ /\bblib\b/)
  {
    $Class = 'Sereal::Encoder';
  }
  else {
    $Class = 'Sereal::Decoder';
  }
  $ConstClass = $Class . "::Constants";
  eval "use $ConstClass ':all'; 1"
  or do {
    my $err = $@ || 'Zombie Error';
    die "Failed to load/import constants from '$ConstClass': $err";
  };
}

use Exporter;
our @ISA = qw(Exporter);
our @EXPORT_OK = qw(
  $Header @BasicTests $Class $ConstClass
  FBIT
  hobodecode
  integer short_string varint array array_fbit
  hash dump_bless
  have_encoder_and_decoder
  run_roundtrip_tests
);

our %EXPORT_TAGS = (all => \@EXPORT_OK);

use constant FBIT => 128;

sub hobodecode {
  open my $fh, "| $^X -Mblib author_tools/hobodecoder.pl -e" or die $!;
  print $fh @_;
  close $fh;
}

sub array {
  chr(SRL_HDR_ARRAY) . varint(0+@_) . join("", @_) . chr(SRL_HDR_TAIL)
}

sub array_fbit {
  chr(SRL_HDR_ARRAY+FBIT) . varint(0+@_) . join("", @_) . chr(SRL_HDR_TAIL)
}

sub hash {
  chr(SRL_HDR_HASH) . varint(int(@_/2)) . join("", @_) . chr(SRL_HDR_TAIL)
}

sub dump_bless {
  # this hack does not support UTF8 class names, but that's not supported by
  # most releases of perl anyway
  chr(SRL_HDR_BLESS)
  .$_[0]
  .(
    ref($_[1])
    ? chr(SRL_HDR_COPY).varint(${$_[1]})
    : (
      (length($_[1]) >= 2**6)
      ? chr(SRL_HDR_STRING).varint(length($_[1])).$_[1]
      : chr(length($_[1]) + 0x40).$_[1]
    )
  )
}

sub short_string {
  die if length($_[0]) > 2**6-1;
  return chr(0b0100_0000 + length($_[0])).$_[0];
}

sub integer {
  if ($_[0] < 0) {
    return $_[0] < -16
            ? die("zigzag not implemented in test suite")
            : chr(0b0001_0000 + abs($_[0]));
  }
  else {
    return $_[0] > 15
            ? varint($_[0])
            : chr(0b0000_0000 + $_[0]);
  }
}

sub varint {
  my $n = shift;
  my $out = '';
  while ($n >= 0x80) {
    $out .= chr( ($n & 0x7f) | 0x80 );
    $n >>= 7;
  }
  $out .= chr($n);
  return $out;
}


my $ary_ref_for_repeating = [5,6];
my $scalar_ref_for_repeating = \9;
my $weak_thing;
our $Header = SRL_MAGIC_STRING . chr(SRL_PROTOCOL_VERSION) . chr(0);
$weak_thing = [\$weak_thing, 1];
weaken($weak_thing->[0]);

our @BasicTests = (
  # warning: this hardcodes the POS/NEG headers
  [1, chr(0b0000_0001), "encode 1"],
  [0, chr(0b0000_0000), "encode 0"],
  [-1, chr(0b0001_0000), "encode -1"],
  [undef, chr(SRL_HDR_UNDEF), "encode undef"],
  ["", chr(0b0100_0000), "encode empty string"],
  ["1", chr(0b0100_0001) . "1", "encode string '1'"],
  ["91a", chr(0b0100_0011) . "91a", "encode string '91a'"],
  ["abc" x 1000, chr(SRL_HDR_STRING).varint(3000).("abc" x 1000), "long ASCII string"],
  [\1, chr(SRL_HDR_REF).varint(0).chr(0b0000_0001), "scalar ref to int"],
  [[], array(), "empty array ref"],
  [[1,2,3], array(chr(0b0000_0001), chr(0b0000_0010), chr(0b0000_0011)), "array ref"],
  [1000, chr(SRL_HDR_VARINT).varint(1000), "large int"],
  [ [1..1000],
    array(
      (map chr, (1..SRL_POS_MAX_SIZE)),
      (map chr(SRL_HDR_VARINT) . varint($_), ((SRL_POS_MAX_SIZE+1) .. 1000))
    ),
    "array ref with pos and varints"
  ],
  [{}, chr(SRL_HDR_HASH).varint(0).chr(SRL_HDR_TAIL), "empty hash ref"],
  [{foo => "baaaaar"},
       chr(SRL_HDR_HASH).varint(1)
      .chr(0b0100_0111)."baaaaar"
      .chr(0b0100_0011)."foo"
      .chr(SRL_HDR_TAIL)
      , "simple hash ref"],
  [$scalar_ref_for_repeating, chr(SRL_HDR_REF).varint(0).chr(0b0000_1001), "scalar ref to constant"],
  [[$scalar_ref_for_repeating, $scalar_ref_for_repeating],
    do {
      my $content = chr(SRL_HDR_ARRAY) .varint(2);
      $content   .= chr(SRL_HDR_REF);
      $content   .= chr(0);
      my $pos = length($Header) + length($content);

      $content    .= chr(0b1000_1001)
                    .chr(SRL_HDR_REF)
                    .varint($pos)
                    .chr(SRL_HDR_TAIL);
      $content
    }, "repeated substructure (REUSE): scalar ref"],
  [[$ary_ref_for_repeating, $ary_ref_for_repeating],
    do {
      my $content = chr(SRL_HDR_ARRAY)
                    .varint(2);
      my $pos = length($Header) + length($content);
      $content   .= array_fbit(chr(0b0000_0101), chr(0b0000_0110))
                    .chr(SRL_HDR_REUSE)
                    .varint($pos)
                    .chr(SRL_HDR_TAIL);
      $content
    }, "repeated substructure (REUSE): array"],
  [[\$ary_ref_for_repeating, [1, $ary_ref_for_repeating]],
    do {
      my $content = chr(SRL_HDR_ARRAY)
                    . varint(2)
                    . chr(SRL_HDR_REF)
                    . chr(0);
      my $pos = length($Header) + length($content);
      $content   .= array_fbit(
                        chr(0b0000_0101),
                        chr(0b0000_0110)
                    )
                 . array(
                        chr(0b0000_0001),
                        chr(SRL_HDR_REUSE) . varint($pos)
                   )
                 . chr(SRL_HDR_TAIL);
      $content
    }, "repeated substructure (REUSE): asymmetric"],
  [
    $weak_thing,
    chr(SRL_HDR_ARRAY + FBIT)
    .varint(2)
    .chr(SRL_HDR_PAD)
    .chr(SRL_HDR_REF)
    .varint(0)
    .chr(SRL_HDR_REUSE)
    .varint(5)
    .chr(0b0000_0001)
    .chr(SRL_HDR_TAIL),
    "weak thing copy (requires PAD)"
  ],
  [
    \$weak_thing,
    chr(SRL_HDR_REF)
    .varint(0)
    .chr(SRL_HDR_ARRAY + FBIT)
    .varint(2)
    .chr(SRL_HDR_WEAKEN)
    .chr(SRL_HDR_REF)
    .varint(7)
    .chr(0b0000_0001)
    .chr(SRL_HDR_TAIL),
    "weak thing ref"
  ],
  sub { \@_ } ->(
    $weak_thing,
    chr(SRL_HDR_ARRAY + FBIT)
    .varint(2)
    .chr(SRL_HDR_WEAKEN)
    .chr(SRL_HDR_REF)
    .varint(5)
    .chr(0b0000_0001)
    .chr(SRL_HDR_TAIL),
    "weak thing alias"
   ),
  [
    do { my @array; $array[0]=\$array[1]; $array[1]=\$array[0]; \@array },
    chr(SRL_HDR_ARRAY)
    .varint(2)
    .chr(SRL_HDR_REF + FBIT)
    .varint(0)
    .chr(SRL_HDR_REF + FBIT)
    .varint(7)
    .chr(SRL_HDR_ALIAS)
    .varint(9)
    .chr(SRL_HDR_TAIL),
    "scalar cross"
  ],
  [
    do { my @array; $array[0]=\$array[1]; $array[1]=\$array[0]; weaken($array[1]); weaken($array[0]); \@array },
    chr(SRL_HDR_ARRAY)
    .varint(2)
    .chr(SRL_HDR_WEAKEN + FBIT)
    .chr(SRL_HDR_REF)
    .varint(0)
    .chr(SRL_HDR_WEAKEN + FBIT)
    .chr(SRL_HDR_REF)
    .varint(7)
    .chr(SRL_HDR_ALIAS)
    .varint(10)
    .chr(SRL_HDR_TAIL),
    "weak scalar cross"
  ],
  [
    bless([],"foo"),
    dump_bless(array(), "foo"),
    "bless [], 'foo' (2)"
  ],
  [
    do { my $qr= bless qr/foo/ix,"bar"; [ $qr, $qr ] },
    join("",
        chr(SRL_HDR_ARRAY),
        varint(2),
        chr(SRL_HDR_BLESS),
        chr(SRL_HDR_REGEXP + FBIT),
        chr(0b0100_0011)."foo",
        chr(0b0100_0010)."ix",
        chr(0b0100_0011)."bar",
        chr(SRL_HDR_REUSE),
        varint(8),
        chr(SRL_HDR_TAIL)
    ),
    "blessed regexp with reuse"
  ],
  [
    do { my $o1=bless [], "foo"; my $o2=bless [], "foo"; [ $o1, $o2, $o1, $o2 ] },
    join("",
        chr(SRL_HDR_ARRAY),
            varint(4),
            chr(SRL_HDR_BLESS),
                chr(SRL_HDR_ARRAY + FBIT),varint(0),chr(SRL_HDR_TAIL),
                chr(0b0100_0011)."foo",
            chr(SRL_HDR_BLESS),
                chr(SRL_HDR_ARRAY + FBIT),varint(0),chr(SRL_HDR_TAIL),
                chr(SRL_HDR_COPY),varint(11),
            chr(SRL_HDR_REUSE),varint(8),
            chr(SRL_HDR_REUSE),varint(16),
            chr(SRL_HDR_TAIL)
    ),
    "blessed arrays with reuse"
  ],
  [
    [bless([], "foo"), bless([], "foo")],
    do {
      my $content = chr(SRL_HDR_ARRAY).varint(2)
                    .chr(SRL_HDR_BLESS).array();
      my $pos = length($Header) + length($content);
      $content .= chr(3 + 0x40)."foo"
                  .dump_bless( array(), \$pos )
                  .chr(SRL_HDR_TAIL);
      $content
    },
    "[bless([], 'foo'), bless([], 'foo')]"
  ],
  [
    bless([bless {}, "foo"], "foo"),
    do {
      my $content = chr(SRL_HDR_BLESS)
                    .chr(SRL_HDR_ARRAY)
                    .varint(1)
                    .dump_bless(hash(), "foo");
      my $pos = length($Header) + length($content) - 4;
      $content .= chr(SRL_HDR_TAIL).chr(SRL_HDR_COPY).varint($pos);
      $content
    },
    "bless [bless {}, 'foo'], 'foo'"
  ],
  [
    qr/foo/,
    dump_bless(
      chr(SRL_HDR_REGEXP)
      .chr(0b0100_0011)
      ."foo"
      .chr(0b0100_0000),
      "Regexp"
    ),
    "qr/foo/"
  ],
  [
    qr/(?i-xsm:foo)/,
    dump_bless(
      chr(SRL_HDR_REGEXP)
      .chr(0b0100_1100)
      ."(?i-xsm:foo)"
      .chr(0b0100_0000),
      "Regexp"
    ),
    "qr/(?i-xsm:foo)/"
  ],
  [
    qr/foo/i,
    dump_bless(
      chr(SRL_HDR_REGEXP)
      .chr(0b0100_0011)
      ."foo"
      .chr(0b0100_0001)
      ."i",
      "Regexp"
    ),
    "qr/foo/i"
  ],
  [
    [{foo => 1}, {foo => 2}],
    sub {
      my $opt = shift;
      if ($opt->{no_shared_hashkeys}) {
        return array(
          hash(integer(1), short_string("foo")),
          hash(integer(2), short_string("foo")),
        );
      }
      else {
        return array(
          hash(integer(1), short_string("foo")),
          hash(integer(2), chr(SRL_HDR_COPY).varint(12)),
        );
      }
    },
    "duplicate hash keys"
  ],
);


sub get_git_top_dir {
  my @dirs = (0, 1, 2);
  for my $d (@dirs) {
    my $tdir = File::Spec->catdir(map File::Spec->updir, 1..$d);
    my $gdir = File::Spec->catdir($tdir, '.git');
    if (-d $gdir) {
      return $tdir;
    }
  }
  return();
}

sub have_encoder_and_decoder {
  my $need = $Class =~ /Encoder/ ? "Decoder" : "Encoder";
  my $need_class = "Sereal::$need";
  my $v = $Class->VERSION;

  if (defined(my $top_dir = get_git_top_dir())) {
    my $blib_dir = File::Spec->catdir($top_dir, "Sereal-$need", "blib");
    if (-d $blib_dir) {
      require blib;
      blib->import($blib_dir);
    }
  }

  eval "use $need_class; 1"
  or do {
    note("Could not locate $need_class for testing");
    return();
  };
  my $cmp_v = $need_class->VERSION;
  if (not defined $cmp_v or not $cmp_v eq $v) {
    note("Could not load correct version of $need_class for testing (got: $cmp_v, needed $v)");
    return();
  }
  return 1;
}

our @ScalarRoundtripTests = (
  # name, structure
  ["undef", undef],
  ["small int", 3],
  ["small negative int", -8],
  ["largeish int", 100000],
  ["largeish negative int", -302001],
  ["float", 0.2],
  ["short ascii string", "fooo"],
  ["short latin1 string", "Müller"],
  ["short utf8 string", do {use utf8; " עדיין ח"}],
  ["long ascii string", do{"abc" x 1000}],
  ["long latin1 string", "üll" x 1000],
  ["long utf8 string", do {use utf8; " עדיין חשב" x 1000}],
  ["long utf8 string with only ascii", do {use utf8; "foo" x 1000}],
  ["long utf8 string with only latin1 subset", do {use utf8; "üll" x 1000}],
  ["simple regexp", qr/foo/],
  ["regexp with inline modifiers", qr/(?i-xsm:foo)/],
  ["regexp with modifiers", qr/foo/i],
);

our @RoundtripTests = (
  @ScalarRoundtripTests,

  ["[{foo => 1}, {foo => 2}] - repeated hash keys",
    [{foo => 1}, {foo => 2}] ],

  (map {["scalar ref to " . $_->[0], \($_->[1])]} @ScalarRoundtripTests),
  (map {["nested scalar ref to " . $_->[0], \\($_->[1])]} @ScalarRoundtripTests),
  (map {["array ref to " . $_->[0], [$_->[1]]]} @ScalarRoundtripTests),
  (map {["hash ref to " . $_->[0], {foo => $_->[1]}]} @ScalarRoundtripTests),
  (map {["array ref to duplicate " . $_->[0], [$_->[1], $_->[1]]]} @ScalarRoundtripTests),
  (map {["array ref to aliases " . $_->[0], sub {\@_}->($_->[1], $_->[1])]} @ScalarRoundtripTests),
  (map {["array ref to scalar refs to same " . $_->[0], [\($_->[1]), \($_->[1])]]} @ScalarRoundtripTests),
);


sub run_roundtrip_tests {
  #my $decoder = Sereal::Decoder->new;
  my $encoder = Sereal::Encoder->new;

  foreach my $meth (
                    ['functional',
                      sub {Sereal::Encoder::encode_sereal(shift)},
                      sub {Sereal::Decoder::decode_sereal(shift)}],
                    ['object-oriented',
                      sub {$encoder->encode(shift)},
                      sub {Sereal::Decoder::decode_sereal(shift)}], # no OO version of Decoder yet
                    )
  {
    my ($mname, $enc, $dec) = @$meth;

    foreach my $rt (@RoundtripTests) {
      my ($name, $data) = @$rt;
      my $s = $enc->($data);
      ok(defined $s, "$name ($mname, defined)")
        or do {
          if (defined $ENV{DEBUG_SEREAL}) {
            note("Data was: " . Data::Dumper::Dumper($data));
            note("Output was: " . (defined($s) ? $s : "<undef>"));
          }
          next;
        };
      my $d = $dec->($s);
      ok(defined($d) || !defined($data), "$name ($mname, defined2)");
      my $s2 = $enc->($d);
      ok(defined $s2, "$name ($mname, defined3)");
      is_deeply($d, $data, "$name ($mname, deeply)");
      is_string($s2, $s, "$name ($mname, serialized)");
    }
  } # end serialization method iteration
}



1;
