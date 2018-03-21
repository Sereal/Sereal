#!perl
use strict;
use warnings;
use File::Spec;
use lib File::Spec->catdir(qw(t lib));
BEGIN {
    lib->import('lib')
        if !-d 't';
}
use Sereal::TestSet qw(:all);
use Sereal qw(write_sereal_file read_sereal_file);

use Test::More tests => 4;
use File::Temp;
my $dir= File::Temp->newdir;
my $source= {foo=>1};
my $file= "$dir/test.srl";
{
    write_sereal_file("$dir/test.srl",$source);
    ok(-e $file, "file exists");
}
{
    my $copy= read_sereal_file("$dir/test.srl");
    is_deeply($source,$copy,"simple read works");
}
{
    read_sereal_file("$dir/test.srl",{},my $copy);
    is_deeply($source,$copy,"read to root works");
}
{
    read_sereal_file("$dir/test.srl",undef,my $copy);
    is_deeply($source,$copy,"read to root works (undef opts)");
}




