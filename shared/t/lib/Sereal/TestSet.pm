package # Hide from PAUSE
  Sereal::TestSet;

use strict;
use warnings;

use Scalar::Util qw(weaken);

# Dynamically load constants from whatever is being tested
BEGIN {
  my $class;
  if (defined $INC{"Sereal/Encoder.pm"}
      and $INC{"Sereal/Encoder.pm"} =~ /\bblib\b/)
  {
    $class = 'Sereal::Encoder::Constants';
  }
  else {
    $class = 'Sereal::Decoder::Constants';
  }
  eval "use $class ':all'; 1"
  or do {
    my $err = $@ || 'Zombie Error';
    die "Failed to load/import constants from '$class': $err";
  };
}

use Exporter;
our @ISA = qw(Exporter);
our @EXPORT_OK = qw(
  $Header @BasicTests
  FBIT
  hobodecode
  varint array array_fbit
  hash dump_bless
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
);

1;
