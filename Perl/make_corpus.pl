use strict;
use Sereal::Encoder qw(encode_sereal);
use Array::RefElem qw(hv_store);

=pod

Run with:

rm ../sereal_corpus.txt; ^Cr v in $(git tag | grep Sereal-Encoder | grep -v _ | grep -v '\-0.'); do git reset --hard $v >/dev/null 2>&1; git clean -dfx; perl Makefile.PL; make; perl -Mblib ../make_corpus.pl | tee -a ../sereal_corpus.txt; done

=cut


sub alias_array { \@_ }
my $array= [ "this", \"is", qr/a test/ ];
my $x= "alias";
my $y= "why oh why?";
my @snums= ("0001", "1.00010", "   1  ", "  0  ", "1E5");
#my $sum; $sum += $_ for @snums; # causes issues with 3.01 - 3.05
#when we didnt handle leading or trailing zeros properly.
my $struct= {
    ar1 => $array,
    ar2 => $array,
    alias_array => alias_array($x,$x,$x),
    h2 => { h3 => "furble" },
    re1 => qr/this is a regexp/x,
    re2 => qr/this is a case insensitive regexp/i,
    nums => [ -42, 42, 0.42, @snums ],
    srrrrrefs => \\\\\$y,
    sref=> \$y,
    rconst => \98,
};
$struct->{r1}= \$struct->{r2};
$struct->{r2}= \$struct->{r1};
my ($p,$q);
$p=\$q;
$q=\$p;
$struct->{cross}= [ $p, $q ];
$struct->{alias_cross}= alias_array($p,$q);
hv_store(%$struct,"alias",$x);

my $encoded= encode_sereal($struct);
print "'$Sereal::Encoder::VERSION' => <<'EOF',\n",pack("u*", $encoded), "EOF\n";
