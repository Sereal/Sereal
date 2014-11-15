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
    {
        open my $ifh, "<", "const-xs.inc"
            or die "Can't read const-xs.inc: $!";
        open my $ofh, ">", "const-xs.new"
            or die "Can't write const-xs.new: $!";
        while (<$ifh>) {
            s/(IV\s+)iv;/${1}iv = 0;/g;
            print $ofh $_;
        }
        close $ofh;
        close $ifh;
        rename "const-xs.new","const-xs.inc"
            or die "Can't rename const-xs.new => const-xs.inc: $!";
    }
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
    "comment" => "small positive integer - value in low 4 bits (identity)",
    "masked" => 1,
    "masked_val" => 0,
    "name" => "POS_0",
    "type_name" => "POS",
    "type_value" => 0,
    "value" => 0
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 1,
    "name" => "POS_1",
    "type_name" => "POS",
    "type_value" => 0,
    "value" => 1
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 2,
    "name" => "POS_2",
    "type_name" => "POS",
    "type_value" => 0,
    "value" => 2
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 3,
    "name" => "POS_3",
    "type_name" => "POS",
    "type_value" => 0,
    "value" => 3
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 4,
    "name" => "POS_4",
    "type_name" => "POS",
    "type_value" => 0,
    "value" => 4
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 5,
    "name" => "POS_5",
    "type_name" => "POS",
    "type_value" => 0,
    "value" => 5
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 6,
    "name" => "POS_6",
    "type_name" => "POS",
    "type_value" => 0,
    "value" => 6
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 7,
    "name" => "POS_7",
    "type_name" => "POS",
    "type_value" => 0,
    "value" => 7
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 8,
    "name" => "POS_8",
    "type_name" => "POS",
    "type_value" => 0,
    "value" => 8
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 9,
    "name" => "POS_9",
    "type_name" => "POS",
    "type_value" => 0,
    "value" => 9
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 10,
    "name" => "POS_10",
    "type_name" => "POS",
    "type_value" => 0,
    "value" => 10
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 11,
    "name" => "POS_11",
    "type_name" => "POS",
    "type_value" => 0,
    "value" => 11
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 12,
    "name" => "POS_12",
    "type_name" => "POS",
    "type_value" => 0,
    "value" => 12
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 13,
    "name" => "POS_13",
    "type_name" => "POS",
    "type_value" => 0,
    "value" => 13
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 14,
    "name" => "POS_14",
    "type_name" => "POS",
    "type_value" => 0,
    "value" => 14
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 15,
    "name" => "POS_15",
    "type_name" => "POS",
    "type_value" => 0,
    "value" => 15
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "comment" => "small negative integer - value in low 4 bits (k+32)",
    "masked" => 1,
    "masked_val" => 16,
    "name" => "NEG_16",
    "type_name" => "NEG",
    "type_value" => 16,
    "value" => 16
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 15,
    "name" => "NEG_15",
    "type_name" => "NEG",
    "type_value" => 16,
    "value" => 17
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 14,
    "name" => "NEG_14",
    "type_name" => "NEG",
    "type_value" => 16,
    "value" => 18
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 13,
    "name" => "NEG_13",
    "type_name" => "NEG",
    "type_value" => 16,
    "value" => 19
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 12,
    "name" => "NEG_12",
    "type_name" => "NEG",
    "type_value" => 16,
    "value" => 20
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 11,
    "name" => "NEG_11",
    "type_name" => "NEG",
    "type_value" => 16,
    "value" => 21
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 10,
    "name" => "NEG_10",
    "type_name" => "NEG",
    "type_value" => 16,
    "value" => 22
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 9,
    "name" => "NEG_9",
    "type_name" => "NEG",
    "type_value" => 16,
    "value" => 23
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 8,
    "name" => "NEG_8",
    "type_name" => "NEG",
    "type_value" => 16,
    "value" => 24
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 7,
    "name" => "NEG_7",
    "type_name" => "NEG",
    "type_value" => 16,
    "value" => 25
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 6,
    "name" => "NEG_6",
    "type_name" => "NEG",
    "type_value" => 16,
    "value" => 26
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 5,
    "name" => "NEG_5",
    "type_name" => "NEG",
    "type_value" => 16,
    "value" => 27
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 4,
    "name" => "NEG_4",
    "type_name" => "NEG",
    "type_value" => 16,
    "value" => 28
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 3,
    "name" => "NEG_3",
    "type_name" => "NEG",
    "type_value" => 16,
    "value" => 29
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 2,
    "name" => "NEG_2",
    "type_name" => "NEG",
    "type_value" => 16,
    "value" => 30
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 1,
    "name" => "NEG_1",
    "type_name" => "NEG",
    "type_value" => 16,
    "value" => 31
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "comment" => "<VARINT> - Varint variable length integer",
    "name" => "VARINT",
    "type_name" => "VARINT",
    "type_value" => 32,
    "value" => 32
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "comment" => "<ZIGZAG-VARINT> - Zigzag variable length integer",
    "name" => "ZIGZAG",
    "type_name" => "ZIGZAG",
    "type_value" => 33,
    "value" => 33
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "comment" => "<IEEE-FLOAT>",
    "name" => "FLOAT",
    "type_name" => "FLOAT",
    "type_value" => 34,
    "value" => 34
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "comment" => "<IEEE-DOUBLE>",
    "name" => "DOUBLE",
    "type_name" => "DOUBLE",
    "type_value" => 35,
    "value" => 35
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "comment" => "<IEEE-LONG-DOUBLE>",
    "name" => "LONG_DOUBLE",
    "type_name" => "LONG_DOUBLE",
    "type_value" => 36,
    "value" => 36
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "comment" => "None - Perl undef var; eg my \$var= undef;",
    "name" => "UNDEF",
    "type_name" => "UNDEF",
    "type_value" => 37,
    "value" => 37
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "comment" => "<LEN-VARINT> <BYTES> - binary/(latin1) string",
    "name" => "BINARY",
    "type_name" => "BINARY",
    "type_value" => 38,
    "value" => 38
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "comment" => "<LEN-VARINT> <UTF8> - utf8 string",
    "name" => "STR_UTF8",
    "type_name" => "STR_UTF8",
    "type_value" => 39,
    "value" => 39
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "comment" => "<ITEM-TAG>    - ref to next item",
    "name" => "REFN",
    "type_name" => "REFN",
    "type_value" => 40,
    "value" => 40
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "comment" => "<OFFSET-VARINT> - ref to previous item stored at offset",
    "name" => "REFP",
    "type_name" => "REFP",
    "type_value" => 41,
    "value" => 41
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "comment" => "<COUNT-VARINT> [<KEY-TAG> <ITEM-TAG> ...] - count followed by key/value pairs",
    "name" => "HASH",
    "type_name" => "HASH",
    "type_value" => 42,
    "value" => 42
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "comment" => "<COUNT-VARINT> [<ITEM-TAG> ...] - count followed by items",
    "name" => "ARRAY",
    "type_name" => "ARRAY",
    "type_value" => 43,
    "value" => 43
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "comment" => "<STR-TAG> <ITEM-TAG> - class, object-item",
    "name" => "OBJECT",
    "type_name" => "OBJECT",
    "type_value" => 44,
    "value" => 44
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "comment" => "<OFFSET-VARINT> <ITEM-TAG> - offset of previously used classname tag - object-item",
    "name" => "OBJECTV",
    "type_name" => "OBJECTV",
    "type_value" => 45,
    "value" => 45
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "comment" => "<OFFSET-VARINT> - alias to item defined at offset",
    "name" => "ALIAS",
    "type_name" => "ALIAS",
    "type_value" => 46,
    "value" => 46
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "comment" => "<OFFSET-VARINT> - copy of item defined at offset",
    "name" => "COPY",
    "type_name" => "COPY",
    "type_value" => 47,
    "value" => 47
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "comment" => "<REF-TAG> - Weaken the following reference",
    "name" => "WEAKEN",
    "type_name" => "WEAKEN",
    "type_value" => 48,
    "value" => 48
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "comment" => "<PATTERN-STR-TAG> <MODIFIERS-STR-TAG>",
    "name" => "REGEXP",
    "type_name" => "REGEXP",
    "type_value" => 49,
    "value" => 49
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "comment" => "<STR-TAG> <ITEM-TAG> - class, object-item. Need to call \"THAW\" method on class after decoding",
    "name" => "OBJECT_FREEZE",
    "type_name" => "OBJECT_FREEZE",
    "type_value" => 50,
    "value" => 50
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "comment" => "<OFFSET-VARINT> <ITEM-TAG> - (OBJECTV_FREEZE is to OBJECT_FREEZE as OBJECTV is to OBJECT)",
    "name" => "OBJECTV_FREEZE",
    "type_name" => "OBJECTV_FREEZE",
    "type_value" => 51,
    "value" => 51
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "comment" => "reserved",
    "masked" => 1,
    "masked_val" => 0,
    "name" => "RESERVED_0",
    "type_name" => "RESERVED",
    "type_value" => 52,
    "value" => 52
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 1,
    "name" => "RESERVED_1",
    "type_name" => "RESERVED",
    "type_value" => 52,
    "value" => 53
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 2,
    "name" => "RESERVED_2",
    "type_name" => "RESERVED",
    "type_value" => 52,
    "value" => 54
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 3,
    "name" => "RESERVED_3",
    "type_name" => "RESERVED",
    "type_value" => 52,
    "value" => 55
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 4,
    "name" => "RESERVED_4",
    "type_name" => "RESERVED",
    "type_value" => 52,
    "value" => 56
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "comment" => "undef (PL_sv_undef) - \"the\" Perl undef (see notes)",
    "name" => "CANONICAL_UNDEF",
    "type_name" => "CANONICAL_UNDEF",
    "type_value" => 57,
    "value" => 57
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "comment" => "false (PL_sv_no)",
    "name" => "FALSE",
    "type_name" => "FALSE",
    "type_value" => 58,
    "value" => 58
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "comment" => "true  (PL_sv_yes)",
    "name" => "TRUE",
    "type_name" => "TRUE",
    "type_value" => 59,
    "value" => 59
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "comment" => "<LEN-VARINT> <TYPE-BYTE> <TAG-DATA> - repeated tag (not done yet, will be implemented in version 3)",
    "name" => "MANY",
    "type_name" => "MANY",
    "type_value" => 60,
    "value" => 60
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "comment" => "(first byte of magic string in header)",
    "name" => "PACKET_START",
    "type_name" => "PACKET_START",
    "type_value" => 61,
    "value" => 61
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "comment" => "<BYTE> - for additional tags",
    "name" => "EXTEND",
    "type_name" => "EXTEND",
    "type_value" => 62,
    "value" => 62
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "comment" => "(ignored tag, skip to next byte)",
    "name" => "PAD",
    "type_name" => "PAD",
    "type_value" => 63,
    "value" => 63
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "comment" => "[<ITEM-TAG> ...] - count of items in low 4 bits (ARRAY must be refcnt=1)",
    "masked" => 1,
    "masked_val" => 0,
    "name" => "ARRAYREF_0",
    "type_name" => "ARRAYREF",
    "type_value" => 64,
    "value" => 64
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 1,
    "name" => "ARRAYREF_1",
    "type_name" => "ARRAYREF",
    "type_value" => 64,
    "value" => 65
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 2,
    "name" => "ARRAYREF_2",
    "type_name" => "ARRAYREF",
    "type_value" => 64,
    "value" => 66
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 3,
    "name" => "ARRAYREF_3",
    "type_name" => "ARRAYREF",
    "type_value" => 64,
    "value" => 67
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 4,
    "name" => "ARRAYREF_4",
    "type_name" => "ARRAYREF",
    "type_value" => 64,
    "value" => 68
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 5,
    "name" => "ARRAYREF_5",
    "type_name" => "ARRAYREF",
    "type_value" => 64,
    "value" => 69
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 6,
    "name" => "ARRAYREF_6",
    "type_name" => "ARRAYREF",
    "type_value" => 64,
    "value" => 70
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 7,
    "name" => "ARRAYREF_7",
    "type_name" => "ARRAYREF",
    "type_value" => 64,
    "value" => 71
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 8,
    "name" => "ARRAYREF_8",
    "type_name" => "ARRAYREF",
    "type_value" => 64,
    "value" => 72
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 9,
    "name" => "ARRAYREF_9",
    "type_name" => "ARRAYREF",
    "type_value" => 64,
    "value" => 73
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 10,
    "name" => "ARRAYREF_10",
    "type_name" => "ARRAYREF",
    "type_value" => 64,
    "value" => 74
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 11,
    "name" => "ARRAYREF_11",
    "type_name" => "ARRAYREF",
    "type_value" => 64,
    "value" => 75
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 12,
    "name" => "ARRAYREF_12",
    "type_name" => "ARRAYREF",
    "type_value" => 64,
    "value" => 76
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 13,
    "name" => "ARRAYREF_13",
    "type_name" => "ARRAYREF",
    "type_value" => 64,
    "value" => 77
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 14,
    "name" => "ARRAYREF_14",
    "type_name" => "ARRAYREF",
    "type_value" => 64,
    "value" => 78
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 15,
    "name" => "ARRAYREF_15",
    "type_name" => "ARRAYREF",
    "type_value" => 64,
    "value" => 79
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "comment" => "[<KEY-TAG> <ITEM-TAG> ...] - count in low 4 bits, key/value pairs (HASH must be refcnt=1)",
    "masked" => 1,
    "masked_val" => 0,
    "name" => "HASHREF_0",
    "type_name" => "HASHREF",
    "type_value" => 80,
    "value" => 80
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 1,
    "name" => "HASHREF_1",
    "type_name" => "HASHREF",
    "type_value" => 80,
    "value" => 81
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 2,
    "name" => "HASHREF_2",
    "type_name" => "HASHREF",
    "type_value" => 80,
    "value" => 82
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 3,
    "name" => "HASHREF_3",
    "type_name" => "HASHREF",
    "type_value" => 80,
    "value" => 83
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 4,
    "name" => "HASHREF_4",
    "type_name" => "HASHREF",
    "type_value" => 80,
    "value" => 84
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 5,
    "name" => "HASHREF_5",
    "type_name" => "HASHREF",
    "type_value" => 80,
    "value" => 85
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 6,
    "name" => "HASHREF_6",
    "type_name" => "HASHREF",
    "type_value" => 80,
    "value" => 86
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 7,
    "name" => "HASHREF_7",
    "type_name" => "HASHREF",
    "type_value" => 80,
    "value" => 87
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 8,
    "name" => "HASHREF_8",
    "type_name" => "HASHREF",
    "type_value" => 80,
    "value" => 88
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 9,
    "name" => "HASHREF_9",
    "type_name" => "HASHREF",
    "type_value" => 80,
    "value" => 89
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 10,
    "name" => "HASHREF_10",
    "type_name" => "HASHREF",
    "type_value" => 80,
    "value" => 90
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 11,
    "name" => "HASHREF_11",
    "type_name" => "HASHREF",
    "type_value" => 80,
    "value" => 91
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 12,
    "name" => "HASHREF_12",
    "type_name" => "HASHREF",
    "type_value" => 80,
    "value" => 92
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 13,
    "name" => "HASHREF_13",
    "type_name" => "HASHREF",
    "type_value" => 80,
    "value" => 93
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 14,
    "name" => "HASHREF_14",
    "type_name" => "HASHREF",
    "type_value" => 80,
    "value" => 94
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 15,
    "name" => "HASHREF_15",
    "type_name" => "HASHREF",
    "type_value" => 80,
    "value" => 95
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "comment" => "<BYTES> - binary/latin1 string, length encoded in low 5 bits of tag",
    "masked" => 1,
    "masked_val" => 0,
    "name" => "SHORT_BINARY_0",
    "type_name" => "SHORT_BINARY",
    "type_value" => 96,
    "value" => 96
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 1,
    "name" => "SHORT_BINARY_1",
    "type_name" => "SHORT_BINARY",
    "type_value" => 96,
    "value" => 97
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 2,
    "name" => "SHORT_BINARY_2",
    "type_name" => "SHORT_BINARY",
    "type_value" => 96,
    "value" => 98
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 3,
    "name" => "SHORT_BINARY_3",
    "type_name" => "SHORT_BINARY",
    "type_value" => 96,
    "value" => 99
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 4,
    "name" => "SHORT_BINARY_4",
    "type_name" => "SHORT_BINARY",
    "type_value" => 96,
    "value" => 100
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 5,
    "name" => "SHORT_BINARY_5",
    "type_name" => "SHORT_BINARY",
    "type_value" => 96,
    "value" => 101
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 6,
    "name" => "SHORT_BINARY_6",
    "type_name" => "SHORT_BINARY",
    "type_value" => 96,
    "value" => 102
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 7,
    "name" => "SHORT_BINARY_7",
    "type_name" => "SHORT_BINARY",
    "type_value" => 96,
    "value" => 103
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 8,
    "name" => "SHORT_BINARY_8",
    "type_name" => "SHORT_BINARY",
    "type_value" => 96,
    "value" => 104
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 9,
    "name" => "SHORT_BINARY_9",
    "type_name" => "SHORT_BINARY",
    "type_value" => 96,
    "value" => 105
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 10,
    "name" => "SHORT_BINARY_10",
    "type_name" => "SHORT_BINARY",
    "type_value" => 96,
    "value" => 106
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 11,
    "name" => "SHORT_BINARY_11",
    "type_name" => "SHORT_BINARY",
    "type_value" => 96,
    "value" => 107
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 12,
    "name" => "SHORT_BINARY_12",
    "type_name" => "SHORT_BINARY",
    "type_value" => 96,
    "value" => 108
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 13,
    "name" => "SHORT_BINARY_13",
    "type_name" => "SHORT_BINARY",
    "type_value" => 96,
    "value" => 109
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 14,
    "name" => "SHORT_BINARY_14",
    "type_name" => "SHORT_BINARY",
    "type_value" => 96,
    "value" => 110
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 15,
    "name" => "SHORT_BINARY_15",
    "type_name" => "SHORT_BINARY",
    "type_value" => 96,
    "value" => 111
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 16,
    "name" => "SHORT_BINARY_16",
    "type_name" => "SHORT_BINARY",
    "type_value" => 96,
    "value" => 112
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 17,
    "name" => "SHORT_BINARY_17",
    "type_name" => "SHORT_BINARY",
    "type_value" => 96,
    "value" => 113
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 18,
    "name" => "SHORT_BINARY_18",
    "type_name" => "SHORT_BINARY",
    "type_value" => 96,
    "value" => 114
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 19,
    "name" => "SHORT_BINARY_19",
    "type_name" => "SHORT_BINARY",
    "type_value" => 96,
    "value" => 115
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 20,
    "name" => "SHORT_BINARY_20",
    "type_name" => "SHORT_BINARY",
    "type_value" => 96,
    "value" => 116
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 21,
    "name" => "SHORT_BINARY_21",
    "type_name" => "SHORT_BINARY",
    "type_value" => 96,
    "value" => 117
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 22,
    "name" => "SHORT_BINARY_22",
    "type_name" => "SHORT_BINARY",
    "type_value" => 96,
    "value" => 118
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 23,
    "name" => "SHORT_BINARY_23",
    "type_name" => "SHORT_BINARY",
    "type_value" => 96,
    "value" => 119
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 24,
    "name" => "SHORT_BINARY_24",
    "type_name" => "SHORT_BINARY",
    "type_value" => 96,
    "value" => 120
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 25,
    "name" => "SHORT_BINARY_25",
    "type_name" => "SHORT_BINARY",
    "type_value" => 96,
    "value" => 121
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 26,
    "name" => "SHORT_BINARY_26",
    "type_name" => "SHORT_BINARY",
    "type_value" => 96,
    "value" => 122
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 27,
    "name" => "SHORT_BINARY_27",
    "type_name" => "SHORT_BINARY",
    "type_value" => 96,
    "value" => 123
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 28,
    "name" => "SHORT_BINARY_28",
    "type_name" => "SHORT_BINARY",
    "type_value" => 96,
    "value" => 124
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 29,
    "name" => "SHORT_BINARY_29",
    "type_name" => "SHORT_BINARY",
    "type_value" => 96,
    "value" => 125
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 30,
    "name" => "SHORT_BINARY_30",
    "type_name" => "SHORT_BINARY",
    "type_value" => 96,
    "value" => 126
  },
  # autoupdated by author_tools/update_from_header.pl do not modify directly!
  {
    "masked" => 1,
    "masked_val" => 31,
    "name" => "SHORT_BINARY_31",
    "type_name" => "SHORT_BINARY",
    "type_value" => 96,
    "value" => 127
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

# Prefer external csnappy and miniz libraries over the bundled ones.
sub check_external_libraries {
  my ($libs, $defines, $objects) = @_;
  require Devel::CheckLib;

  if (
    !$ENV{SEREAL_USE_BUNDLED_LIBS} &&
    !$ENV{SEREAL_USE_BUNDLED_CSNAPPY} &&
    Devel::CheckLib::check_lib(
      lib      => 'csnappy',
      header   => 'csnappy.h'
  )) {
    print "Using installed csnappy library\n";
    $$libs .= ' -lcsnappy';
    $$defines .= ' -DHAVE_CSNAPPY';
  } else {
    print "Using bundled csnappy code\n";
  }

  if (
    !$ENV{SEREAL_USE_BUNDLED_LIBS} &&
    !$ENV{SEREAL_USE_BUNDLED_MINIZ} &&
    Devel::CheckLib::check_lib(
      lib      => 'miniz',
      header   => 'miniz.h'
  )) {
    print "Using installed miniz library\n";
    $$libs .= ' -lminiz';
    $$defines .= ' -DHAVE_MINIZ';
  } else {
    print "Using bundled miniz code\n";
    $$objects .= ' miniz$(OBJ_EXT)';
  }
}

1;

