#!perl
use strict;
use warnings;
use Data::Dumper;
my (%n2v, %v2n, %v2c);
my $len= 0;
sub fill_ranges {
    my $pfx= shift;
    $pfx=~s/_LOW//;
    defined(my $ofs= $n2v{$pfx})
        or die "unknown $pfx";
    for my $i ( $n2v{$pfx . "_LOW"} .. $n2v{$pfx . "_HIGH"}) {
        my $n= $pfx=~/NEG/ ? abs($i - 32) : $i - $ofs;
        $n2v{ $pfx . "_" . $n } ||= $i;
        $v2n{ $i } = $pfx . "_". $n;
        $v2c{ $i } ||= '';
    }
    $v2c{ $n2v{$pfx . "_HIGH"} } = $v2c{ $ofs };
}
sub read_protocol {
    open my $fh,"<", "Perl/shared/srl_protocol.h"
        or die "Perl/shared/srl_protocol.h: $!";
    my @fill;
    while (<$fh>) {
        if(m!^#define\s+SRL_HDR_(\S+)\s+\(\(char\)(\d+)\)\s*(?:/\*\s*(.*?)\s*\*/)?\s*\z!i) {
            $n2v{$1}= $2;
            $v2n{$2} ||= $1;
            $v2c{$2} ||= $3;
            push @fill, $1 if substr($1,-4) eq '_LOW';
        }
    }
    close $fh;
    fill_ranges($_) for @fill;
    foreach my $pfx (keys %n2v) {
        $len= length($pfx) if $len < length($pfx);
    }
}
sub open_swap {
    my $file= shift;
    open my $fh,"<", $file
        or die "error opening $file for read:$!";
    rename $file,"$file.bak"
        or die "error renaming $file to $file.bak: $!";
    open my $out,">", $file
        or die "error opening $file for write (old version in $file.bak):$!";
    return ($fh,$out);
}

sub replace_block {
    my ($file,$blob)= @_;
    my ($in,$out)= open_swap($file);
    while (<$in>) {
        print $out $_;
        last if /^=for autoupdater start/;
    }
    $blob=~s/\s+$//mg;
    print $out "\n$blob\n\n";
    while (<$in>) {
        if (/^=for autoupdater stop/) {
            print $out $_;
            last;
        }
    }
    while (<$in>) {
        print $out $_;
    }
    close $out;
    close $in;
}
sub update_srl_decoder_h {
    replace_block("Perl/Decoder/srl_decoder.h",
        join("\n",
            "* NOTE this section is autoupdated by $0",
            "*/",
            "static const char * const tag_name[] = {",
            ( map {
                my $str= Data::Dumper::qquote(chr($_));
                if ($str=~/^"\\[0-9]+"\z/) { $str="";}
                sprintf qq(\t%-*s /* %-4s %3d 0x%02x 0b%08b */),
                    $len+3, qq("$v2n{$_}") . ($_==127 ? " " : ","), $str, $_, $_, $_
            } 0 .. 127 ),
            "};",
            "/*",
            "* NOTE the above section is auto-updated by $0",
        )
    )
}

sub update_table {
    replace_block($_[0],
        join("\n",
            "",
            sprintf(qq(    %*s | %-4s | %3s | %4s | %10s | %s),
                $len,qw(Tag       Char Dec Hex  Binary     Follow  )),
            sprintf(qq(    %*s-+-%-4s-+-%3s-+-%4s-+-%10s |-%s),
                $len,"-" x $len, "-" x 4, "-" x 3, "-" x 4, "-" x 10, "-" x 40),
            ( map {
                my $str= Data::Dumper::qquote(chr($_));
                if ($str=~/^"\\[0-9]+"\z/) { $str="";}
                sprintf qq(    %-*s | %-4s | %3d | 0x%02x | 0b%08b | %s),
                    $len, $v2n{$_}, $str, $_, $_, $_, $v2c{$_} || ""
            } 0 .. 127 ),
            "",
            "",
        )
    )
}

my $git_dir = `git rev-parse --git-dir`
    or die; # we will get a message from rev-parse iirc
chomp($git_dir);
chdir "$git_dir/.."
    or die "Failed to chdir to root of repo '$git_dir/..': $!";
read_protocol();
update_srl_decoder_h();
update_table("sereal_spec.pod");
update_table("Perl/shared/srl_protocol.h");


