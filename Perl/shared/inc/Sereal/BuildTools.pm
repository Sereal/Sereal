package inc::Sereal::BuildTools;
use strict;
use warnings;

sub link_files {
  my $shared_dir = shift;
  # This fires from a git source tree only.
  # Right now, all devs are on Linux. Feel free to make portable.
  eval {
    if (-d "../../.git" and -d $shared_dir) {
      # overwrite by default
      require File::Find;
      require File::Path;
      require File::Spec;
      File::Find::find(
        { no_chdir => 1,
          wanted => sub {
            my $f = $_;
            s/^\Q$shared_dir\E\/?// or die $_;
            return unless $_;
            
            if (-d $f) {
              File::Path::mkpath($_)
            }
            elsif (-f $f) {
              return if $f =~ /(?:\.bak|\.sw[po]|~)$/;
              my @d = File::Spec->splitdir($_);
              my $fname = pop @d;
              my $ref = join "/", ("..") x scalar(@d);
              my $subd = join "/", @d;
              chdir $subd if length($ref);
              symlink(join("/", grep length, $ref, $shared_dir, $subd, $fname), $fname);
              chdir($ref) if length($ref);
            }
          },
        }, $shared_dir
      );
    }
    1
  } or warn $@;
}

# This section deals with extracting constants from the protocol
# definition and including them as Perl constants. Not pretty, I know.
# Regenerate constants if module available.
sub generate_constant_includes {
  my $namespace = shift;
  my $constant_namespace = $namespace . "::Constants";
  my $file = $constant_namespace;
  $file =~ s/::/\//g;
  $file = "lib/$file";
  my $dir = $file;
  $file .= '.pm';

  if (eval { use ExtUtils::Constant qw(WriteConstants); 1 }) {
    require File::Path;
    my $fragment = $dir;
    $fragment =~ s/(?:En|De)coder\/?$//;
    File::Path::mkpath($fragment);
    print "Generating constant exports for Perl...\n";
    open my $fh, "<", "srl_protocol.h" or die $!;
    my (@string_const, @int_const);
    while (<$fh>) {
      if (/^#\s*define\s*(SRL_\w+)\s*(.*?)(?:\/\*|$)/) {
        my ($name, $value) = ($1, $2);
        next if $name =~ /_H_$/ or $name =~ /SET/ or $value =~ /"/;
        push @int_const, $name;
      }
    }
    close $fh;
    WriteConstants(
        NAME => $constant_namespace,
        NAMES => \@int_const,
    );
    open my $ofh, ">", $file or die $!;
    print $ofh <<HERE;
# Genereated code! Do not modify! See inc/Sereal/BuildTools.pm instead

package $constant_namespace;
use strict;
use warnings;
use Carp qw(croak);
use $namespace; our \$VERSION= \$$namespace\::VERSION; # for XSLoading
our \@ISA = qw(Exporter);
require Exporter;
our \@EXPORT_OK;
BEGIN { \@EXPORT_OK = qw(
HERE
    print $ofh "    $_\n" for (@int_const);
    print $ofh <<'HERE';
  );
  my $code;
  foreach my $constname (@EXPORT_OK) {
    my ($error, $val) = constant($constname);
    if ($error) { croak($error); }
    $code .= "sub $constname () {$val}\n";
  }
  eval "$code\n1;" or do {
    my $err = $@ || 'Zombie error';
    die "Failed to generate constant subs: $err\n Code was:\n$code\n";
  };
}

sub SRL_MAGIC_STRING ()                 { "=srl" }
sub SRL_MAGIC_STRING_HIGHBIT ()         { "=\xF3rl" }
sub SRL_MAGIC_STRING_HIGHBIT_UTF8 ()    { "=\xC3\xB3rl" }

push @EXPORT_OK, qw(
    SRL_MAGIC_STRING
    SRL_MAGIC_STRING_HIGHBIT
    SRL_MAGIC_STRING_HIGHBIT_UTF8
);

# start autoupdated section - do not modify directly

our (%TAG_INFO_HASH, @TAG_INFO_ARRAY);
@TAG_INFO_ARRAY = (
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'POS',
    'masked' => 1,
    'comment' => 'small positive integer - value in low 4 bits (identity)',
    'value' => 0,
    'name' => 'POS_0',
    'masked_val' => 0,
    'type_value' => 0
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'POS',
    'masked' => 1,
    'value' => 1,
    'name' => 'POS_1',
    'masked_val' => 1,
    'type_value' => 0
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'POS',
    'masked' => 1,
    'value' => 2,
    'name' => 'POS_2',
    'masked_val' => 2,
    'type_value' => 0
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'POS',
    'masked' => 1,
    'value' => 3,
    'name' => 'POS_3',
    'masked_val' => 3,
    'type_value' => 0
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'POS',
    'masked' => 1,
    'value' => 4,
    'name' => 'POS_4',
    'masked_val' => 4,
    'type_value' => 0
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'POS',
    'masked' => 1,
    'value' => 5,
    'name' => 'POS_5',
    'masked_val' => 5,
    'type_value' => 0
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'POS',
    'masked' => 1,
    'value' => 6,
    'name' => 'POS_6',
    'masked_val' => 6,
    'type_value' => 0
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'POS',
    'masked' => 1,
    'value' => 7,
    'name' => 'POS_7',
    'masked_val' => 7,
    'type_value' => 0
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'POS',
    'masked' => 1,
    'value' => 8,
    'name' => 'POS_8',
    'masked_val' => 8,
    'type_value' => 0
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'POS',
    'masked' => 1,
    'value' => 9,
    'name' => 'POS_9',
    'masked_val' => 9,
    'type_value' => 0
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'POS',
    'masked' => 1,
    'value' => 10,
    'name' => 'POS_10',
    'masked_val' => 10,
    'type_value' => 0
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'POS',
    'masked' => 1,
    'value' => 11,
    'name' => 'POS_11',
    'masked_val' => 11,
    'type_value' => 0
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'POS',
    'masked' => 1,
    'value' => 12,
    'name' => 'POS_12',
    'masked_val' => 12,
    'type_value' => 0
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'POS',
    'masked' => 1,
    'value' => 13,
    'name' => 'POS_13',
    'masked_val' => 13,
    'type_value' => 0
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'POS',
    'masked' => 1,
    'value' => 14,
    'name' => 'POS_14',
    'masked_val' => 14,
    'type_value' => 0
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'POS',
    'masked' => 1,
    'comment' => 'small positive integer - value in low 4 bits (identity)',
    'value' => 15,
    'name' => 'POS_15',
    'masked_val' => 15,
    'type_value' => 0
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'NEG',
    'masked' => 1,
    'comment' => 'small negative integer - value in low 4 bits (k+32)',
    'value' => 16,
    'name' => 'NEG_16',
    'masked_val' => 16,
    'type_value' => 16
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'NEG',
    'masked' => 1,
    'value' => 17,
    'name' => 'NEG_15',
    'masked_val' => 15,
    'type_value' => 16
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'NEG',
    'masked' => 1,
    'value' => 18,
    'name' => 'NEG_14',
    'masked_val' => 14,
    'type_value' => 16
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'NEG',
    'masked' => 1,
    'value' => 19,
    'name' => 'NEG_13',
    'masked_val' => 13,
    'type_value' => 16
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'NEG',
    'masked' => 1,
    'value' => 20,
    'name' => 'NEG_12',
    'masked_val' => 12,
    'type_value' => 16
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'NEG',
    'masked' => 1,
    'value' => 21,
    'name' => 'NEG_11',
    'masked_val' => 11,
    'type_value' => 16
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'NEG',
    'masked' => 1,
    'value' => 22,
    'name' => 'NEG_10',
    'masked_val' => 10,
    'type_value' => 16
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'NEG',
    'masked' => 1,
    'value' => 23,
    'name' => 'NEG_9',
    'masked_val' => 9,
    'type_value' => 16
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'NEG',
    'masked' => 1,
    'value' => 24,
    'name' => 'NEG_8',
    'masked_val' => 8,
    'type_value' => 16
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'NEG',
    'masked' => 1,
    'value' => 25,
    'name' => 'NEG_7',
    'masked_val' => 7,
    'type_value' => 16
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'NEG',
    'masked' => 1,
    'value' => 26,
    'name' => 'NEG_6',
    'masked_val' => 6,
    'type_value' => 16
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'NEG',
    'masked' => 1,
    'value' => 27,
    'name' => 'NEG_5',
    'masked_val' => 5,
    'type_value' => 16
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'NEG',
    'masked' => 1,
    'value' => 28,
    'name' => 'NEG_4',
    'masked_val' => 4,
    'type_value' => 16
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'NEG',
    'masked' => 1,
    'value' => 29,
    'name' => 'NEG_3',
    'masked_val' => 3,
    'type_value' => 16
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'NEG',
    'masked' => 1,
    'value' => 30,
    'name' => 'NEG_2',
    'masked_val' => 2,
    'type_value' => 16
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'NEG',
    'masked' => 1,
    'comment' => 'small negative integer - value in low 4 bits (k+32)',
    'value' => 31,
    'name' => 'NEG_1',
    'masked_val' => 1,
    'type_value' => 16
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'VARINT',
    'comment' => '<VARINT> - Varint variable length integer',
    'value' => 32,
    'name' => 'VARINT',
    'type_value' => 32
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'ZIGZAG',
    'comment' => '<ZIGZAG-VARINT> - Zigzag variable length integer',
    'value' => 33,
    'name' => 'ZIGZAG',
    'type_value' => 33
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'FLOAT',
    'comment' => '<IEEE-FLOAT>',
    'value' => 34,
    'name' => 'FLOAT',
    'type_value' => 34
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'DOUBLE',
    'comment' => '<IEEE-DOUBLE>',
    'value' => 35,
    'name' => 'DOUBLE',
    'type_value' => 35
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'LONG_DOUBLE',
    'comment' => '<IEEE-LONG-DOUBLE>',
    'value' => 36,
    'name' => 'LONG_DOUBLE',
    'type_value' => 36
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'UNDEF',
    'comment' => 'None - Perl undef var; eg my $var= undef;',
    'value' => 37,
    'name' => 'UNDEF',
    'type_value' => 37
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'BINARY',
    'comment' => '<LEN-VARINT> <BYTES> - binary/(latin1) string',
    'value' => 38,
    'name' => 'BINARY',
    'type_value' => 38
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'STR_UTF8',
    'comment' => '<LEN-VARINT> <UTF8> - utf8 string',
    'value' => 39,
    'name' => 'STR_UTF8',
    'type_value' => 39
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'REFN',
    'comment' => '<ITEM-TAG>    - ref to next item',
    'value' => 40,
    'name' => 'REFN',
    'type_value' => 40
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'REFP',
    'comment' => '<OFFSET-VARINT> - ref to previous item stored at offset',
    'value' => 41,
    'name' => 'REFP',
    'type_value' => 41
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'HASH',
    'comment' => '<COUNT-VARINT> [<KEY-TAG> <ITEM-TAG> ...] - count followed by key/value pairs',
    'value' => 42,
    'name' => 'HASH',
    'type_value' => 42
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'ARRAY',
    'comment' => '<COUNT-VARINT> [<ITEM-TAG> ...] - count followed by items',
    'value' => 43,
    'name' => 'ARRAY',
    'type_value' => 43
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'OBJECT',
    'comment' => '<STR-TAG> <ITEM-TAG> - class, object-item',
    'value' => 44,
    'name' => 'OBJECT',
    'type_value' => 44
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'OBJECTV',
    'comment' => '<OFFSET-VARINT> <ITEM-TAG> - offset of previously used classname tag - object-item',
    'value' => 45,
    'name' => 'OBJECTV',
    'type_value' => 45
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'ALIAS',
    'comment' => '<OFFSET-VARINT> - alias to item defined at offset',
    'value' => 46,
    'name' => 'ALIAS',
    'type_value' => 46
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'COPY',
    'comment' => '<OFFSET-VARINT> - copy of item defined at offset',
    'value' => 47,
    'name' => 'COPY',
    'type_value' => 47
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'WEAKEN',
    'comment' => '<REF-TAG> - Weaken the following reference',
    'value' => 48,
    'name' => 'WEAKEN',
    'type_value' => 48
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'REGEXP',
    'comment' => '<PATTERN-STR-TAG> <MODIFIERS-STR-TAG>',
    'value' => 49,
    'name' => 'REGEXP',
    'type_value' => 49
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'OBJECT_FREEZE',
    'comment' => '<STR-TAG> <ITEM-TAG> - class, object-item. Need to call "THAW" method on class after decoding',
    'value' => 50,
    'name' => 'OBJECT_FREEZE',
    'type_value' => 50
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'OBJECTV_FREEZE',
    'comment' => '<OFFSET-VARINT> <ITEM-TAG> - (OBJECTV_FREEZE is to OBJECT_FREEZE as OBJECTV is to OBJECT)',
    'value' => 51,
    'name' => 'OBJECTV_FREEZE',
    'type_value' => 51
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'RESERVED',
    'masked' => 1,
    'comment' => 'reserved',
    'value' => 52,
    'name' => 'RESERVED_0',
    'masked_val' => 0,
    'type_value' => 52
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'RESERVED',
    'masked' => 1,
    'value' => 53,
    'name' => 'RESERVED_1',
    'masked_val' => 1,
    'type_value' => 52
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'RESERVED',
    'masked' => 1,
    'value' => 54,
    'name' => 'RESERVED_2',
    'masked_val' => 2,
    'type_value' => 52
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'RESERVED',
    'masked' => 1,
    'value' => 55,
    'name' => 'RESERVED_3',
    'masked_val' => 3,
    'type_value' => 52
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'RESERVED',
    'masked' => 1,
    'value' => 56,
    'name' => 'RESERVED_4',
    'masked_val' => 4,
    'type_value' => 52
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'CANONICAL_UNDEF',
    'comment' => 'undef (PL_sv_undef) - "the" Perl undef (see notes)',
    'value' => 57,
    'name' => 'CANONICAL_UNDEF',
    'type_value' => 57
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'FALSE',
    'comment' => 'false (PL_sv_no)',
    'value' => 58,
    'name' => 'FALSE',
    'type_value' => 58
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'TRUE',
    'comment' => 'true  (PL_sv_yes)',
    'value' => 59,
    'name' => 'TRUE',
    'type_value' => 59
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'MANY',
    'comment' => '<LEN-VARINT> <TYPE-BYTE> <TAG-DATA> - repeated tag (not done yet, will be implemented in version 3)',
    'value' => 60,
    'name' => 'MANY',
    'type_value' => 60
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'PACKET_START',
    'comment' => '(first byte of magic string in header)',
    'value' => 61,
    'name' => 'PACKET_START',
    'type_value' => 61
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'EXTEND',
    'comment' => '<BYTE> - for additional tags',
    'value' => 62,
    'name' => 'EXTEND',
    'type_value' => 62
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'PAD',
    'comment' => '(ignored tag, skip to next byte)',
    'value' => 63,
    'name' => 'PAD',
    'type_value' => 63
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'ARRAYREF',
    'masked' => 1,
    'comment' => '[<ITEM-TAG> ...] - count of items in low 4 bits (ARRAY must be refcnt=1)',
    'value' => 64,
    'name' => 'ARRAYREF_0',
    'masked_val' => 0,
    'type_value' => 64
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'ARRAYREF',
    'masked' => 1,
    'value' => 65,
    'name' => 'ARRAYREF_1',
    'masked_val' => 1,
    'type_value' => 64
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'ARRAYREF',
    'masked' => 1,
    'value' => 66,
    'name' => 'ARRAYREF_2',
    'masked_val' => 2,
    'type_value' => 64
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'ARRAYREF',
    'masked' => 1,
    'value' => 67,
    'name' => 'ARRAYREF_3',
    'masked_val' => 3,
    'type_value' => 64
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'ARRAYREF',
    'masked' => 1,
    'value' => 68,
    'name' => 'ARRAYREF_4',
    'masked_val' => 4,
    'type_value' => 64
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'ARRAYREF',
    'masked' => 1,
    'value' => 69,
    'name' => 'ARRAYREF_5',
    'masked_val' => 5,
    'type_value' => 64
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'ARRAYREF',
    'masked' => 1,
    'value' => 70,
    'name' => 'ARRAYREF_6',
    'masked_val' => 6,
    'type_value' => 64
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'ARRAYREF',
    'masked' => 1,
    'value' => 71,
    'name' => 'ARRAYREF_7',
    'masked_val' => 7,
    'type_value' => 64
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'ARRAYREF',
    'masked' => 1,
    'value' => 72,
    'name' => 'ARRAYREF_8',
    'masked_val' => 8,
    'type_value' => 64
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'ARRAYREF',
    'masked' => 1,
    'value' => 73,
    'name' => 'ARRAYREF_9',
    'masked_val' => 9,
    'type_value' => 64
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'ARRAYREF',
    'masked' => 1,
    'value' => 74,
    'name' => 'ARRAYREF_10',
    'masked_val' => 10,
    'type_value' => 64
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'ARRAYREF',
    'masked' => 1,
    'value' => 75,
    'name' => 'ARRAYREF_11',
    'masked_val' => 11,
    'type_value' => 64
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'ARRAYREF',
    'masked' => 1,
    'value' => 76,
    'name' => 'ARRAYREF_12',
    'masked_val' => 12,
    'type_value' => 64
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'ARRAYREF',
    'masked' => 1,
    'value' => 77,
    'name' => 'ARRAYREF_13',
    'masked_val' => 13,
    'type_value' => 64
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'ARRAYREF',
    'masked' => 1,
    'value' => 78,
    'name' => 'ARRAYREF_14',
    'masked_val' => 14,
    'type_value' => 64
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'ARRAYREF',
    'masked' => 1,
    'value' => 79,
    'name' => 'ARRAYREF_15',
    'masked_val' => 15,
    'type_value' => 64
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'HASHREF',
    'masked' => 1,
    'comment' => '[<KEY-TAG> <ITEM-TAG> ...] - count in low 4 bits, key/value pairs (HASH must be refcnt=1)',
    'value' => 80,
    'name' => 'HASHREF_0',
    'masked_val' => 0,
    'type_value' => 80
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'HASHREF',
    'masked' => 1,
    'value' => 81,
    'name' => 'HASHREF_1',
    'masked_val' => 1,
    'type_value' => 80
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'HASHREF',
    'masked' => 1,
    'value' => 82,
    'name' => 'HASHREF_2',
    'masked_val' => 2,
    'type_value' => 80
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'HASHREF',
    'masked' => 1,
    'value' => 83,
    'name' => 'HASHREF_3',
    'masked_val' => 3,
    'type_value' => 80
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'HASHREF',
    'masked' => 1,
    'value' => 84,
    'name' => 'HASHREF_4',
    'masked_val' => 4,
    'type_value' => 80
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'HASHREF',
    'masked' => 1,
    'value' => 85,
    'name' => 'HASHREF_5',
    'masked_val' => 5,
    'type_value' => 80
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'HASHREF',
    'masked' => 1,
    'value' => 86,
    'name' => 'HASHREF_6',
    'masked_val' => 6,
    'type_value' => 80
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'HASHREF',
    'masked' => 1,
    'value' => 87,
    'name' => 'HASHREF_7',
    'masked_val' => 7,
    'type_value' => 80
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'HASHREF',
    'masked' => 1,
    'value' => 88,
    'name' => 'HASHREF_8',
    'masked_val' => 8,
    'type_value' => 80
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'HASHREF',
    'masked' => 1,
    'value' => 89,
    'name' => 'HASHREF_9',
    'masked_val' => 9,
    'type_value' => 80
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'HASHREF',
    'masked' => 1,
    'value' => 90,
    'name' => 'HASHREF_10',
    'masked_val' => 10,
    'type_value' => 80
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'HASHREF',
    'masked' => 1,
    'value' => 91,
    'name' => 'HASHREF_11',
    'masked_val' => 11,
    'type_value' => 80
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'HASHREF',
    'masked' => 1,
    'value' => 92,
    'name' => 'HASHREF_12',
    'masked_val' => 12,
    'type_value' => 80
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'HASHREF',
    'masked' => 1,
    'value' => 93,
    'name' => 'HASHREF_13',
    'masked_val' => 13,
    'type_value' => 80
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'HASHREF',
    'masked' => 1,
    'value' => 94,
    'name' => 'HASHREF_14',
    'masked_val' => 14,
    'type_value' => 80
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'HASHREF',
    'masked' => 1,
    'value' => 95,
    'name' => 'HASHREF_15',
    'masked_val' => 15,
    'type_value' => 80
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'SHORT_BINARY',
    'masked' => 1,
    'comment' => '<BYTES> - binary/latin1 string, length encoded in low 5 bits of tag',
    'value' => 96,
    'name' => 'SHORT_BINARY_0',
    'masked_val' => 0,
    'type_value' => 96
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'SHORT_BINARY',
    'masked' => 1,
    'value' => 97,
    'name' => 'SHORT_BINARY_1',
    'masked_val' => 1,
    'type_value' => 96
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'SHORT_BINARY',
    'masked' => 1,
    'value' => 98,
    'name' => 'SHORT_BINARY_2',
    'masked_val' => 2,
    'type_value' => 96
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'SHORT_BINARY',
    'masked' => 1,
    'value' => 99,
    'name' => 'SHORT_BINARY_3',
    'masked_val' => 3,
    'type_value' => 96
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'SHORT_BINARY',
    'masked' => 1,
    'value' => 100,
    'name' => 'SHORT_BINARY_4',
    'masked_val' => 4,
    'type_value' => 96
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'SHORT_BINARY',
    'masked' => 1,
    'value' => 101,
    'name' => 'SHORT_BINARY_5',
    'masked_val' => 5,
    'type_value' => 96
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'SHORT_BINARY',
    'masked' => 1,
    'value' => 102,
    'name' => 'SHORT_BINARY_6',
    'masked_val' => 6,
    'type_value' => 96
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'SHORT_BINARY',
    'masked' => 1,
    'value' => 103,
    'name' => 'SHORT_BINARY_7',
    'masked_val' => 7,
    'type_value' => 96
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'SHORT_BINARY',
    'masked' => 1,
    'value' => 104,
    'name' => 'SHORT_BINARY_8',
    'masked_val' => 8,
    'type_value' => 96
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'SHORT_BINARY',
    'masked' => 1,
    'value' => 105,
    'name' => 'SHORT_BINARY_9',
    'masked_val' => 9,
    'type_value' => 96
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'SHORT_BINARY',
    'masked' => 1,
    'value' => 106,
    'name' => 'SHORT_BINARY_10',
    'masked_val' => 10,
    'type_value' => 96
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'SHORT_BINARY',
    'masked' => 1,
    'value' => 107,
    'name' => 'SHORT_BINARY_11',
    'masked_val' => 11,
    'type_value' => 96
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'SHORT_BINARY',
    'masked' => 1,
    'value' => 108,
    'name' => 'SHORT_BINARY_12',
    'masked_val' => 12,
    'type_value' => 96
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'SHORT_BINARY',
    'masked' => 1,
    'value' => 109,
    'name' => 'SHORT_BINARY_13',
    'masked_val' => 13,
    'type_value' => 96
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'SHORT_BINARY',
    'masked' => 1,
    'value' => 110,
    'name' => 'SHORT_BINARY_14',
    'masked_val' => 14,
    'type_value' => 96
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'SHORT_BINARY',
    'masked' => 1,
    'value' => 111,
    'name' => 'SHORT_BINARY_15',
    'masked_val' => 15,
    'type_value' => 96
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'SHORT_BINARY',
    'masked' => 1,
    'value' => 112,
    'name' => 'SHORT_BINARY_16',
    'masked_val' => 16,
    'type_value' => 96
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'SHORT_BINARY',
    'masked' => 1,
    'value' => 113,
    'name' => 'SHORT_BINARY_17',
    'masked_val' => 17,
    'type_value' => 96
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'SHORT_BINARY',
    'masked' => 1,
    'value' => 114,
    'name' => 'SHORT_BINARY_18',
    'masked_val' => 18,
    'type_value' => 96
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'SHORT_BINARY',
    'masked' => 1,
    'value' => 115,
    'name' => 'SHORT_BINARY_19',
    'masked_val' => 19,
    'type_value' => 96
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'SHORT_BINARY',
    'masked' => 1,
    'value' => 116,
    'name' => 'SHORT_BINARY_20',
    'masked_val' => 20,
    'type_value' => 96
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'SHORT_BINARY',
    'masked' => 1,
    'value' => 117,
    'name' => 'SHORT_BINARY_21',
    'masked_val' => 21,
    'type_value' => 96
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'SHORT_BINARY',
    'masked' => 1,
    'value' => 118,
    'name' => 'SHORT_BINARY_22',
    'masked_val' => 22,
    'type_value' => 96
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'SHORT_BINARY',
    'masked' => 1,
    'value' => 119,
    'name' => 'SHORT_BINARY_23',
    'masked_val' => 23,
    'type_value' => 96
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'SHORT_BINARY',
    'masked' => 1,
    'value' => 120,
    'name' => 'SHORT_BINARY_24',
    'masked_val' => 24,
    'type_value' => 96
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'SHORT_BINARY',
    'masked' => 1,
    'value' => 121,
    'name' => 'SHORT_BINARY_25',
    'masked_val' => 25,
    'type_value' => 96
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'SHORT_BINARY',
    'masked' => 1,
    'value' => 122,
    'name' => 'SHORT_BINARY_26',
    'masked_val' => 26,
    'type_value' => 96
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'SHORT_BINARY',
    'masked' => 1,
    'value' => 123,
    'name' => 'SHORT_BINARY_27',
    'masked_val' => 27,
    'type_value' => 96
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'SHORT_BINARY',
    'masked' => 1,
    'value' => 124,
    'name' => 'SHORT_BINARY_28',
    'masked_val' => 28,
    'type_value' => 96
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'SHORT_BINARY',
    'masked' => 1,
    'value' => 125,
    'name' => 'SHORT_BINARY_29',
    'masked_val' => 29,
    'type_value' => 96
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'SHORT_BINARY',
    'masked' => 1,
    'value' => 126,
    'name' => 'SHORT_BINARY_30',
    'masked_val' => 30,
    'type_value' => 96
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    'type_name' => 'SHORT_BINARY',
    'masked' => 1,
    'value' => 127,
    'name' => 'SHORT_BINARY_31',
    'masked_val' => 31,
    'type_value' => 96
  }
);
$TAG_INFO_HASH{chr $_}= $TAG_INFO_ARRAY[$_] for 0 .. 127;
push @EXPORT_OK, qw(%TAG_INFO_HASH @TAG_INFO_ARRAY);

# stop autoupdated section - do not modify directly!


our %EXPORT_TAGS=(all => \@EXPORT_OK);
HERE
    close $ofh;
  }
  else {
    warn "Please install ExtUtils::Constant since you appear to be running out of the source repository.\n";
  }
}


1;

