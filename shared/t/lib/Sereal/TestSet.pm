package # Hide from PAUSE
  Sereal::TestSet;

use strict;
use warnings;

use File::Spec;
use Scalar::Util qw(weaken);
use Test::More;
use Test::LongString;
use Data::Dumper;
use Devel::Peek;
use Encode qw(encode_utf8);

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
  open my $fh, "| $^X -Mblib=../Encoder -Mblib=../Decoder author_tools/hobodecoder.pl -e" or die $!;
  print $fh @_;
  close $fh;
}

sub array {
  chr(SRL_HDR_REFN).
  chr(SRL_HDR_ARRAY) . varint(0+@_) . join("", @_)
}

sub array_fbit {
  chr(SRL_HDR_REFN).
  chr(SRL_HDR_ARRAY+FBIT) . varint(0+@_) . join("", @_)
}

sub hash {
  chr(SRL_HDR_REFN).
  chr(SRL_HDR_HASH) . varint(int(@_/2)) . join("", @_)
}

sub dump_bless {
  # this hack does not support UTF8 class names, but that's not supported by
  # most releases of perl anyway
  chr(SRL_HDR_BLESS)
  .(
    ref($_[1])
    ? chr(SRL_HDR_COPY).varint(${$_[1]})
    : (
      (length($_[1]) >= 2**6)
      ? chr(SRL_HDR_STRING).varint(length($_[1])).$_[1]
      : chr(length($_[1]) + SRL_HDR_ASCII_LOW).$_[1]
    )
  )
  . $_[0]
}

sub short_string {
  die if length($_[0]) > SRL_MASK_ASCII_LEN;
  return chr(SRL_HDR_ASCII_LOW + length($_[0])) . $_[0];
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
  die "varint cannot be negative" if $n < 0;
  my $out = '';
  while ($n >= 0x80) {
    $out .= chr( ($n & 0x7f) | 0x80 );
    $n >>= 7;
  }
  $out .= chr($n);
  return $out;
}

our $Header = SRL_MAGIC_STRING . chr(SRL_PROTOCOL_VERSION) . chr(0);

my $ary_ref_for_repeating = [5,6];
my $scalar_ref_for_repeating = \9;

my $weak_thing; $weak_thing = [\$weak_thing, 1]; weaken($weak_thing->[0]);

my $unicode1= "Ba\xDF Ba\xDF"; my $unicode2= "\x{168}nix! \x{263a}"; utf8::upgrade($unicode1); utf8::upgrade($unicode2);


our @BasicTests = (
  # warning: this hardcodes the POS/NEG headers
  [1, chr(0b0000_0001), "encode 1"],
  [0, chr(0b0000_0000), "encode 0"],
  [-1, chr(0b0001_0000), "encode -1"],
  [undef, chr(SRL_HDR_UNDEF), "encode undef"],
  ["", short_string(""), "encode empty string"],
  ["1", short_string("1"), "encode string '1'"],
  ["91a", short_string("91a"), "encode string '91a'"],
  ["abc" x 1000, chr(SRL_HDR_STRING).varint(3000).("abc" x 1000), "long ASCII string"],
  [\1, chr(SRL_HDR_REFN).chr(0b0000_0001), "scalar ref to int"],
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

  [{}, chr(SRL_HDR_REFN).chr(SRL_HDR_HASH).varint(0), "empty hash ref"],
  [{foo => "baaaaar"},
       chr(SRL_HDR_REFN).chr(SRL_HDR_HASH).varint(1)
      .short_string("foo")
      .short_string("baaaaar")
      , "simple hash ref"],
  [$scalar_ref_for_repeating, chr(SRL_HDR_REFN).chr(0b0000_1001), "scalar ref to constant"],
  [[$scalar_ref_for_repeating, $scalar_ref_for_repeating],
    do {
      my $content = chr(SRL_HDR_REFN).chr(SRL_HDR_ARRAY) .varint(2);
      $content   .= chr(SRL_HDR_REFN);
      my $pos = length($Header) + length($content);

      $content    .= chr(0b1000_1001)
                    .chr(SRL_HDR_REFP)
                    .varint($pos)
      ;
      $content
    }, "repeated substructure (REUSE): scalar ref"],
  [[$ary_ref_for_repeating, $ary_ref_for_repeating],
    do {
      my $content = chr(SRL_HDR_REFN) . chr(SRL_HDR_ARRAY)
                    .varint(2);
      my $pos = length($Header) + length($content) + 1;
      $content   .= array_fbit(chr(0b0000_0101), chr(0b0000_0110))
                    .chr(SRL_HDR_REFP)
                    .varint($pos)
      ;
      $content
    }, "repeated substructure (REUSE): array"],
  [[\$ary_ref_for_repeating, [1, $ary_ref_for_repeating]],
    do {
      my $content = chr(SRL_HDR_REFN) . chr(SRL_HDR_ARRAY) . varint(2)
                    . chr(SRL_HDR_REFN);
      my $pos = length($Header) + length($content) + 1;
      $content .= array_fbit(
                        chr(0b0000_0101),
                        chr(0b0000_0110)
                    )
                    . array(
                        chr(0b0000_0001),
                        chr(SRL_HDR_REFP) . varint($pos)
                    )
      ;
      $content
    }, "repeated substructure (REUSE): asymmetric"],
  [
    $weak_thing,
    chr(SRL_HDR_REFN).chr(SRL_HDR_ARRAY + FBIT)
    .varint(2)
    .chr(SRL_HDR_PAD)
    .chr(SRL_HDR_REFN)
    .chr(SRL_HDR_REFP)
    .varint(length($Header)+1)
    .chr(0b0000_0001)
    ,
    "weak thing copy (requires PAD)"
  ],
  [
    \$weak_thing,
    chr(SRL_HDR_REFN)
    . chr(SRL_HDR_REFN + FBIT)
        . chr(SRL_HDR_ARRAY) . varint(2)
            .chr(SRL_HDR_WEAKEN) . chr(SRL_HDR_REFP) . varint(length($Header)+1)
            .chr(0b0000_0001)
    ,
    "weak thing ref"
  ],
  sub { \@_ } ->(
    $weak_thing,
    chr(SRL_HDR_REFN + FBIT)
        .chr(SRL_HDR_ARRAY).varint(2)
            .chr(SRL_HDR_WEAKEN).chr(SRL_HDR_REFP).varint(length($Header))
            .chr(0b0000_0001)
    ,
    "weak thing (aliased root)"
   ),
  [
    do { my @array; $array[0]=\$array[1]; $array[1]=\$array[0]; \@array },
    chr(SRL_HDR_REFN).chr(SRL_HDR_ARRAY) . varint(2)
    .chr(SRL_HDR_REFN + FBIT)
    .chr(SRL_HDR_REFP + FBIT)
    .varint(length($Header)+3)
    .chr(SRL_HDR_ALIAS)
    .varint(length($Header)+4)
    ,
    "scalar cross"
  ],
  [
    do { my @array; $array[0]=\$array[1]; $array[1]=\$array[0]; weaken($array[1]); weaken($array[0]); \@array },
    chr(SRL_HDR_REFN).chr(SRL_HDR_ARRAY)
    .varint(2)
    .chr(SRL_HDR_WEAKEN + FBIT)
    .chr(SRL_HDR_REFN)
    .chr(SRL_HDR_WEAKEN + FBIT)
    .chr(SRL_HDR_REFP)
    .varint(length($Header)+3)
    .chr(SRL_HDR_ALIAS)
    .varint(length($Header)+5)
    ,
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
        chr(SRL_HDR_REFN) . chr(SRL_HDR_ARRAY),
        varint(2),
        chr(SRL_HDR_BLESS),
        short_string("bar"),
        chr(SRL_HDR_REFN),
        chr(SRL_HDR_REGEXP + FBIT),
        short_string("foo"),
        short_string("ix"),
        chr(SRL_HDR_REFP),
        varint(length($Header)+9),
    ),
    "blessed regexp with reuse"
  ],
  [
    do { my $o1=bless [], "foo"; my $o2=bless [], "foo"; [ $o1, $o2, $o1, $o2 ] },
    join("",
        chr(SRL_HDR_REFN).chr(SRL_HDR_ARRAY),
            varint(4),
            chr(SRL_HDR_BLESS),
                short_string("foo"),
                chr(SRL_HDR_REFN).chr(SRL_HDR_ARRAY + FBIT),varint(0),
            chr(SRL_HDR_BLESS),
                chr(SRL_HDR_COPY),varint(length($Header)+4),
                chr(SRL_HDR_REFN).chr(SRL_HDR_ARRAY + FBIT),varint(0),
            chr(SRL_HDR_REFP),varint(length($Header)+9),
            chr(SRL_HDR_REFP),varint(length($Header)+15),
    ),
    "blessed arrays with reuse"
  ],
  [
    [bless([], "foo"), bless([], "foo")],
    do {
      my $content = chr(SRL_HDR_REFN) . chr(SRL_HDR_ARRAY) . varint(2)
                    .chr(SRL_HDR_BLESS);
      my $pos = length($Header) + length($content);
      $content .= short_string("foo")
                  . array()
                  . dump_bless( array(), \$pos )
      ;
      $content
    },
    "reused classname empty array"
  ],
  [
    bless([bless {}, "foo"], "foo"),
    do {
      my $content = chr(SRL_HDR_BLESS);
      my $pos = length($Header) + length($content);
      $content .= short_string("foo")
                  . chr(SRL_HDR_REFN) . chr(SRL_HDR_ARRAY) . varint(1)
                    . chr(SRL_HDR_BLESS)
                    . chr(SRL_HDR_COPY) . varint($pos)
                    . chr(SRL_HDR_REFN) . chr(SRL_HDR_HASH) . varint(0)
      ;
      $content
    },
    "wrapped objects"
  ],
  [
    qr/foo/,
    dump_bless(

      chr(SRL_HDR_REFN)
      .chr(SRL_HDR_REGEXP)
      .short_string("foo")
      .short_string(""),
      "Regexp"
    ),
    "qr/foo/"
  ],
  [
    qr/(?i-xsm:foo)/,
    dump_bless(
      chr(SRL_HDR_REFN)
      .chr(SRL_HDR_REGEXP)
      .short_string("(?i-xsm:foo)")
      .short_string(""),
      "Regexp"
    ),
    "qr/(?i-xsm:foo)/"
  ],
  [
    qr/foo/i,
    dump_bless(
      chr(SRL_HDR_REFN)
      .chr(SRL_HDR_REGEXP)
      .short_string("foo")
      .short_string("i"),
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
          hash(
              short_string("foo"),
              integer(1),
          ),
          hash(
              short_string("foo"),
              integer(2),
          ),
        );
      }
      else {
        return array(
          hash(
              short_string("foo"),
              integer(1),
          ),
          hash(
              chr(SRL_HDR_COPY) . varint(length($Header)+6),
              integer(2),
          ),
        );
      }
    },
    "duplicate hash keys"
  ],
  [
      { $unicode1 => $unicode2 },
      hash(
          chr(SRL_HDR_STRING_UTF8) . varint(bytes::length($unicode1)) . encode_utf8($unicode1),
          chr(SRL_HDR_STRING_UTF8) . varint(bytes::length($unicode2)) . encode_utf8($unicode2),
      ),
      "simple unicode hash key and value"
  ]
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
    my $blib_dir = File::Spec->catdir($top_dir, $need, "blib");
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
  ["float", 123013.139],
  ["negative float",-1234.59],
  ["small float",0.41],
  ["negative small float",-0.13],
  ["small int", 123],
  ["empty string", ''],
  ["simple array", []],
  ["empty hash", {}],
  ["simple hash", { foo => 'bar' }],
  ["undef value", { foo => bar => baz => undef }],
  ["simple array", [ 1 ]],
  ["nested simple", [ 1, [ 2 ] ] ],
  ["deep nest", [1,2,[3,4,{5=>6,7=>{8=>[]},9=>{}},{},[]]]],
  ["complex hash", {
    foo => 123,
    bar => -159.23 ,
    'baz' =>"foo",
    'bop \''=> "\10"
    ,'bop \'\\'=> "\x{100}" ,
    'bop \'x\\x'    =>"x\x{100}"   , 'bing' =>   "x\x{100}",
    x=>'y', z => 'p', i=> '1', l=>" \10", m=>"\10 ", n => " \10 ",
  }],
  ["more complex", {
    foo => [123],
    "bar" => [-159.23 , { 'baz' => "foo", }, ],
    'bop \''=> { "\10" => { 'bop \'\\'=> "\x{100}", h=>{
    'bop \'x\\x'    =>"x\x{100}"   , 'bing' =>   "x\x{100}",
    x=>'y',}, z => 'p' ,   }   ,
    i    =>  '1' ,}, l=>" \10", m=>"\10 ", n => " \10 ",
    o => undef ,p=>undef,
  }],
  ['var strings', [ "\$", "\@", "\%" ]],
  [ "quote keys", { "" => '"', "'" => "" }],
  [ "ref to foo", \"foo" ],
  [ "double ref to foo", \\"foo"],
  [ "refy array", \\["foo"]],
  [ "reffy hash", \\\{foo=>\"bar"}],
  [ "blessed array", bless(\[],"foo")],
  [ "utf8 string", "123\\277ABC\\x{DF}456"],
  [ "escaped string", "\\012\345\267\145123\\277ABC\\x{DF}456"],
  [ "more escapes", "\\0123\0124"],
  [ "ref to undef", \undef],
  [ "negative big num", -4123456789],
  [ "positive big num", 4123456789],
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

if (eval "use Array::RefElem (av_store hv_store); 1") {
    my $x= "alias!";
    my (@av,%hv);
    av_store(@av,0,$x);
    av_store(@av,1,$x);
    hv_store(%hv,"x", $x);
    hv_store(%hv,"y", $x);
    push @RoundtripTests,
        [\@av,"alias in array"],
        [\%hv,"alias in hash"],
        [[\@av,\%hv,\$x], "alias hell"];
}


sub run_roundtrip_tests {
  for my $opt (['plain', {}],
               ['snappy', {snappy => 1}])
  {
    run_roundtrip_tests_internal(@$opt);
  }
}

sub run_roundtrip_tests_internal {
  my ($ename, $opt) = @_;
  my $decoder = Sereal::Decoder->new($opt);
  my $encoder = Sereal::Encoder->new($opt);

  foreach my $meth (
                    ['functional',
                      sub {Sereal::Encoder::encode_sereal(shift, $opt)},
                      sub {Sereal::Decoder::decode_sereal(shift, $opt)}],
                    ['object-oriented',
                      sub {$encoder->encode(shift)},
                      sub {$decoder->decode(shift)}],
                    )
  {
    my ($mname, $enc, $dec) = @$meth;

    foreach my $rt (@RoundtripTests) {
      my ($name, $data) = @$rt;
      my $encoded = $enc->($data);
      ok(defined $encoded, "$name ($ename, $mname, encoded defined)")
        or do {
          if (defined $ENV{DEBUG_SEREAL}) {
            note("Data was: " . Data::Dumper::Dumper($data));
            note("Output was: " . (defined($encoded) ? $encoded : "<undef>"));
          }
          next;
        };
      my $decoded= $dec->($encoded);
      ok(defined($decoded) || !defined($data), "$name ($ename, $mname, decoded definedness)");
      my $encoded2 = $enc->($decoded);
      ok(defined $encoded2, "$name ($ename, $mname, encoded2 defined)");
      is_deeply($decoded, $data, "$name ($ename, $mname, decoded vs data)")
        or do {
          if ($ENV{DEBUG_DUMP}) {
            Dump($decoded);
            Dump($data);
          }
        };
      my $ret;
      if ($name=~/complex/) {
          $ret = is(length($encoded2), length($encoded),"$name ($ename, $mname, length encoded2 vs length encoded)");
      } else {
          $ret = is_string($encoded2, $encoded, "$name ($ename, $mname, encoded2 vs encoded)");
      }
      $ret or do {
        if ($ENV{DEBUG_DUMP}) {
          Dump($decoded);
          Dump($data);
        } elsif ($ENV{DEBUG_HOBO}) {
          open my $pipe,"| perl -Mblib=../Encoder/blib -Mblib=../Decoder/blib author_tools/hobodecoder.pl -e"
            or die "Dead: $!";
          print $pipe $encoded;
          close $pipe;
          open $pipe,"| perl -Mblib=../Encoder/blib -Mblib=../Decoder/blib author_tools/hobodecoder.pl -e"
            or die "Dead: $!";
          print $pipe $encoded2;
          close $pipe;
        }
      };
    }
  } # end serialization method iteration
}



1;
