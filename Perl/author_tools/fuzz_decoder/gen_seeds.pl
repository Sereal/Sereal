use strict; use warnings;
use Sereal::Encoder qw(encode_sereal);
my $dir = shift or die "usage: gen_seeds.pl <outdir>\n";
mkdir $dir unless -d $dir;
my @payloads = (
  42, -7, 3.14, "hello", "", "unicode: \x{263a}\x{1f600}",
  [1,2,3], {a=>1,b=>[2,3],c=>{d=>"e"}},
  [ ("x") x 50 ], { map { $_ => $_*$_ } 1..40 },
  [ { nested => [ {deep=>[1,[2,[3,[4]]]]} ] } ],
  \"refscalar", [\1, \2, \1],        # dedup / refp
  ("A".."Z"), [ ("dupkey") x 20 ],   # copy tags for repeated strings
  bless({x=>1}, "Some::Class"),       # object / classname
);
my $n = 0;
for my $v (1..6) {
  for my $compress (0,1,2,3) {        # 0 none, 1 snappy, 2 zlib, 3 zstd
    for my $p (@payloads) {
      my %o = (protocol_version => $v);
      $o{compress} = $compress if $compress;
      my $enc = eval { Sereal::Encoder->new(\%o) } or next;
      my $bytes = eval { $enc->encode($p) };
      next unless defined $bytes;
      open my $fh, ">:raw", sprintf("%s/seed_v%d_c%d_%04d.srl",$dir,$v,$compress,$n++) or die $!;
      print $fh $bytes; close $fh;
    }
  }
}
# v6 keyed-checksum seeds (key matches the one the harness verifies with)
for my $mode (qw(v1 v2 v3)) {
  my $enc = eval { Sereal::Encoder->new({protocol_version=>6, checksum_mode=>$mode, checksum_key=>("k" x 16)}) };
  next unless $enc;
  for my $p (@payloads) {
    my $bytes = eval { $enc->encode($p) } or next;
    open my $fh, ">:raw", sprintf("%s/seed_csum_%s_%04d.srl",$dir,$mode,$n++) or die $!;
    print $fh $bytes; close $fh;
  }
}
print "generated $n seeds in $dir\n";
