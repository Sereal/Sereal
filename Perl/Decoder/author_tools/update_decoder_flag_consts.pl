use strict;
use warnings;
use Data::Dumper;

my %sets;
my %flag_consts;
my %flag_names;
my (@flags, @static, @volatile);
my $file= "srl_decoder.h";
{
    open my $fh, "<", $file
        or die "Failed to open '$file' for read: $!";

    while (<$fh>) {
        if ( m/#define (SRL_F_(\w+))\s+(.*)/ || /(\w+(VOLATILE_FLAGS))\s+(.*)/ ) {
            #print;
            my $full_name= $1;
            my $name= $2;
            my $value= $3;
            $name=~s/_?DECODER_?//g;
            $flag_names{$full_name}= $name;
            if ($value=~s/UL\z//) {
                $flag_consts{$full_name}= 0+eval $value;
            } else {
                $value =~ s/(SRL_F_\w+)/\$flag_consts{$1}/g;
                $sets{"SRL_F_DECODER_" . $name}= 0+eval $value;
            }
        }
    }
    foreach my $key ( sort { $flag_consts{$a} <=> $flag_consts{$b} } keys %flag_consts ) {
        my $is_volatile= $flag_consts{$key} & $sets{SRL_F_DECODER_VOLATILE_FLAGS};
        push @static,   $is_volatile ? undef : $flag_names{$key};
        push @volatile, $is_volatile ? $flag_names{$key} : undef;
        push @flags, $flag_names{$key};
    }
}

my %consts= (%sets, %flag_consts);
$consts{_FLAG_NAME}= \@flags;
$consts{_FLAG_NAME_VOLATILE}= \@volatile;
$consts{_FLAG_NAME_STATIC}= \@static;

my $infile= "lib/Sereal/Decoder.pm";
my $outfile= "$infile.new";
open my $fh,"<", $infile
    or die "Failed to read '$infile': $!";
open my $ofh, ">", $outfile
    or die "Failed to write to '$outfile': $!";
while (<$fh>) {
    if (/#begin generated/) {
        print $ofh $_;
        my $s= Data::Dumper->new([\%consts])->Sortkeys(1)->Terse(1)->Dump();
        chop($s);
        $s.="; #end generated\n";
        print $ofh $s;
    }
    unless(/#begin generated/ ... /#end generated/) {
        print $ofh $_;
    }
}
close $ofh;
close $fh;
rename $infile, "$infile.bak" or die "Failed to rename '$infile' to '$infile.bak': $!";
rename $outfile, $infile or die "Failed to rename '$outfile' to '$infile': $!";

