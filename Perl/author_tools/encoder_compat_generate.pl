use strict;
use warnings;
use Config;
use File::Path qw(make_path);
use File::Spec;
use Sereal::Encoder ();
require $ARGV[0];
my ($module_path, $out, $tag, $tag_commit, $source_commit) = @ARGV[1..5];
my $loaded = $INC{'Sereal/Encoder.pm'} || '';
die "wrong Encoder loaded: $loaded\n" unless $loaded =~ /\Q$module_path\E/;
my $version = $Sereal::Encoder::VERSION;
die "unexpected Encoder version '$version'\n" unless defined $version;
make_path($out);
my $cases = inputs($version);
for my $name (sort keys %$cases) {
    my %opts;
    if ($name eq 'compressed') {
        if ($Sereal::Encoder::VERSION < 4) {
            %opts = ( snappy => 1, snappy_threshold => 0 );
        }
        else {
            %opts = ( compress => Sereal::Encoder::SRL_SNAPPY(), compress_threshold => 0 );
        }
    }
    my $packet = Sereal::Encoder->new(\%opts)->encode($cases->{$name});
    if ($name eq 'compressed') {
        my $protocol = ord substr($packet, 4, 1);
        die "compressed case lacks compressed protocol header (byte $protocol)\n"
            unless (($protocol >> 4) & 0x07);
    }
    open my $fh, '>:raw', File::Spec->catfile($out, "$name.srl") or die $!;
    print {$fh} $packet or die $!;
    close $fh or die $!;
}
open my $meta, '>', File::Spec->catfile($out, 'provenance.txt') or die $!;
print {$meta} "tag=$tag\ntag_commit=$tag_commit\nsource_commit=$source_commit\nencoder_version=$version\nencoder_path=$loaded\nperl=$^V\narch=$Config{archname}\nprotocol_profile=Encoder defaults; compression forced for compressed fixture\nfeature_gates=regexp requires Encoder 4.025 or later; compression uses Snappy supported by each Encoder generation\n";
close $meta or die $!;
