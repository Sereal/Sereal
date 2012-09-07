use strict;
use warnings;
use blib;
use Benchmark qw(cmpthese :hireswallclock);
use Sereal::Decoder qw(decode_sereal);
use Sereal::Encoder qw(encode_sereal);
use JSON::XS qw(decode_json encode_json);
use Storable qw(nfreeze thaw);
use Data::Undump qw(undump);
use Data::Dumper qw(Dumper);
use Data::Dumper::Limited qw(DumpLimited);
use Data::MessagePack;
use Getopt::Long qw(GetOptions);

my (
    $duration,
    $encoder,
    $decoder,
    $dump,
    $tiny_data,
    $small_data,
    $medium_data,
    $large_data,
    $very_large_data,
    $nobless,
    $diagrams,
    $diagram_output_dir,
);
BEGIN {
    my $sereal_only = 0;
    GetOptions(
        'duration=f' => \($duration=-3),
        'encoder'   => \$encoder,
        'decoder'   => \$decoder,
        'dump|d'    => \$dump,
        'tiny'      => \$tiny_data,
        'small'     => \$small_data,
        'medium'    => \$medium_data,
        'large'     => \$large_data,
        'very_large|very-large|verylarge' => \$very_large_data,
        'no_bless|no-bless|nobless'    => \$nobless,
        'sereal_only|sereal-only|serealonly' => \$sereal_only,
        'diagrams'  => \$diagrams,
        'diagram_output=s' => \$diagram_output_dir,
    );
    eval "sub SEREAL_ONLY () { $sereal_only }";
}

my $fail = do {no warnings; $tiny_data + $small_data + $medium_data + $very_large_data + $large_data - 1};
if ($fail and $fail > 0) {
    die "Only one of --tiny, --small, --medium, --large, --very-large allowed!";
}
$encoder = 1 if not $encoder and not $decoder;

our %opt = @ARGV;

our $mpo = Data::MessagePack->new();

my $data_set_name;
srand(0);
my $chars = join("", "a".."z", "A".."Z") x 2;
my @str;
push @str, substr($chars, int(rand(int(length($chars)/2+1))), 10) for 1..1000;
my @rand = map rand, 1..1000;
our %data;

$data{$_}= make_data() for qw(sereal sereal_func dd1 dd2 ddl mp json_xs storable sereal_snappy);

our $enc = Sereal::Encoder->new(\%opt);
our $enc_snappy = Sereal::Encoder->new({%opt, snappy => 1});
our $dec = Sereal::Decoder->new(\%opt);

our ($json_xs, $dd1, $dd2, $ddl, $sereal, $storable, $mp, $sereal_snappy);
# do this first before any of the other dumpers "contaminate" the iv/pv issue
$sereal   = $enc->encode($data{sereal});
$sereal_snappy   = $enc_snappy->encode($data{sereal_snappy});
if (!SEREAL_ONLY) {
    $json_xs  = encode_json($data{json_xs}) if !$medium_data or $nobless;
    $dd1      = Data::Dumper->new([$data{dd1}])->Indent(0)->Dump();
    $dd2      = Dumper($data{dd2});
    $ddl      = DumpLimited($data{ddl}) if !$medium_data or $nobless;
    $mp       = $mpo->pack($data{mp}) if !$medium_data or $nobless;
    $storable = nfreeze($data{storable}); # must be last
}
print($sereal), exit if $dump;

my $sereal_len= bytes::length($sereal);
require bytes;
my @size_datasets;
if (!SEREAL_ONLY) {
    @size_datasets = (
        (($medium_data && !$nobless) ? () : (
            ["JSON::XS",  bytes::length($json_xs)],
            ["Data::Dumper::Limited", bytes::length($ddl)],
            ["Data::MessagePack", bytes::length($mp)],
        )),
        ["Data::Dumper (1)", bytes::length($dd1)],
        ["Data::Dumper (2)", bytes::length($dd2)],
        ["Storable", bytes::length($storable)],
        ["Sereal::Encoder",  bytes::length($sereal)],
        ["Sereal::Encoder, Snappy",  bytes::length($sereal_snappy)],
    );
    for my $tuple (@size_datasets) {
        my ($name, $size) = @$tuple;
        printf "%-40s %12d bytes %.2f%% of sereal\n", $name, $size, $size/$sereal_len *100;
    }
}

our $x;
my ($encoder_result, $decoder_result);
if ($encoder) {
    $encoder_result = cmpthese(
        $duration,
        {
            (!SEREAL_ONLY
                ? (
                    ($medium_data && !$nobless ? () : (
                        json_xs => '$::x = encode_json($::data{json_xs});',
                        ddl => '$::x = DumpLimited($::data{ddl});',
                        msgpack => '$::x = $::mpo->pack($::data{mp});',
                    )),
                    dd_noindent => '$::x = Data::Dumper->new([$::data{dd1}])->Indent(0)->Dump();',
                    dd => '$::x = Dumper($::data{dd2});',
                    storable => '$::x = nfreeze($::data{storable});',
                ) : ()),
            sereal_func => '$::x = encode_sereal($::data{sereal_func}, \%::opt);',
            sereal => '$::x = $::enc->encode($::data{sereal});',
            sereal_snappy => '$::x = $::enc_snappy->encode($::data{sereal_snappy});',
        }
    );
}

if ($decoder) {
    $decoder_result = cmpthese(
        $duration,
        {
            (!SEREAL_ONLY
                ? (
                    ($medium_data && !$nobless ? () : (
                        json_xs => '$::x = decode_json($::json_xs);',
                        undump_ddl => '$::x = Data::Undump::undump($::ddl);',
                        msgpack => '$::x = $::mpo->unpack($::mp);',
                    )),
                    eval_dd => '$::x = eval $::dd1;',
                    storable => '$::x = thaw($::storable);',
                ) : ()),
            sereal_func => '$::x = decode_sereal($::sereal, \%::opt);',
            sereal => '$::x = $::dec->decode($::sereal);',
            sereal_snappy => '$::x = $::dec->decode($::sereal_snappy);',
        }
    );
}

sub make_data {
    if ($tiny_data) {
        $data_set_name = "empty hash";
        return {};
    }
    elsif ($small_data) {
        $data_set_name = "small hash";
        return { foo=> 1, bar => [100,101,102], str => "this is a \x{df} string which has to be serialized" };
    }
    elsif ($medium_data) {
        my @obj = (
            { foo => 1, bar => [100,101,102], str => "this is a \x{df} string which has to be serialized" },
            { foo => 2, bar => [103,103,106,999], str2 => "this is a \x{df} aaaaaastring which has to be serialized" },
            { foozle => 3, bar => [100], str3 => "this is a \x{df} string which haaaaadsadas to be serialized" },
            { foozle => 3, bar => [], st4r => "this is a \x{df} string which has to be sdassdaerialized" },
            { foo => 1, bar => [100,101,102], s5tr => "this is a \x{df} string which has to be serialized" },
            { foo => 2, bar => [103,103,106,999], str => "this is a \x{df} aaaaaastring which has to be serialized" },
            { foozle => 3, bar => [100], str => "this is a \x{df} string which haaaaadsadas to be serialized" },
            { foozle => 3, bar => [], str2 => "this is a \x{df} string which has to be sdassdaerialized" },
            { foo2 => -99999, bar => [100,101,102], str2 => "this is a \x{df} string which has to be serialized" },
            { foo2 => 213, bar => [103,103,106,999], str => "this is a \x{df} aaaaaastring which has to be serialized" },
            { foozle2 => undef, bar => [100], str => "this is a \x{df} string which haaaaadsadas to be serialized" },
            { foozle2 => undef, bar => [1..20], str => "this is a \x{df} string which has to be sdassdaerialized" },
        );
        my @classes = qw(Baz Baz Baz3 Baz2 Baz Baz Baz3 Baz2 Baz Baz Baz3 Baz2);
        if (!$nobless) {
            bless($obj[$_], $classes[$_]) for 0..$#obj;
            $data_set_name = "array of small objects with relations";
        }
        else {
            $data_set_name = "array of small hashes with relations";
        }
        foreach my $i (1..$#obj) {
            $obj[$i]->{parent} = $obj[$i-1];
        }
        return \@obj;
    }
    elsif ($very_large_data) { # "large data"
        $data_set_name = "really rather large data structure";
        my @refs = (
            [1..10000], {@str}, {@str}, [1..10000],
            {@str}, [@rand], {@str}, {@str},
        );
        return [
            \@refs, \@refs, [map {[reverse 1..100]} (0..1000)], [map {+{foo => "bar", baz => "buz"}} 1..2000]
        ]
    }
    else { # "large data"
        $data_set_name = "large data structure";
        return [
            [1..10000], {@str}, {@str}, [1..10000],
            {@str}, [@rand], {@str}, {@str},
        ]
    }
}

if ($diagrams) {
    require SOOT;
    SOOT::Init(0);
    SOOT->import(":all");

    my ($enc_data, $dec_data);
    $enc_data = cmpthese_to_sanity($encoder_result) if $encoder_result;
    $dec_data = cmpthese_to_sanity($decoder_result) if $decoder_result;

    foreach my $dia (["Encoder performance [1/s]", $enc_data],
                     ["Decoder performance [1/s]", $dec_data],)
    {
        my ($title, $d) = @$dia;
        next if not $d;
        $_->[0] =~ s/_/ /g, $_->[0] =~ s/sereal /sereal, / for @$d;
        make_bar_chart(
            substr($title, 0, 3),
            $d,
            {
                title => $title,
                filename => do {
                    my $x = $title;
                    $x =~ s/\[1\/s\]/per second/;
                    $data_set_name . " - " . $x
                },
            }
        );
    }

    my %names = (
        "JSON::XS" => 'json xs',
        "Data::Dumper::Limited" => 'ddl',
        "Data::MessagePack" => "msgpack",
        "Data::Dumper (1)" => "dd noindent",
        "Data::Dumper (2)" => "dd",
        "Storable" => 'storable',
        "Sereal::Encoder" => 'sereal',
        "Sereal::Encoder, Snappy" => 'sereal, snappy',
    );

    make_bar_chart(
        "size",
        [
            sort {$b->[1] <=> $a->[1]} map [ $names{$_->[0]}||die, $_->[1] ], @size_datasets
        ],
        {
            title => "Encoded output sizes [bytes]",
            color => kRed(),
            filename => $data_set_name . " - Encoded output sizes in bytes",
        },
    );
    SOOT->Run if not $diagram_output_dir;
}

sub make_bar_chart {
    my ($name, $data, $opts) = @_;
    my $h = TH1D->new($name, ($opts->{title}||$name), scalar(@$data), -0.5, scalar(@$data)-0.5);
    $h->keep;
    $h->SetFillColor($opts->{color} || kBlue());
    $h->SetBarOffset(0.12);
    $h->SetBarWidth(0.74);
    $h->SetStats(0);
    $h->GetXaxis()->SetLabelSize(0.06);
    $h->GetXaxis()->SetLabelOffset(0.009);
    $h->GetYaxis()->SetTitle($opts->{title}) if defined $opts->{title};
    $h->GetYaxis()->SetTitleSize(0.045);
    for my $i (1..@$data) {
        my ($label, $rate) = @{ $data->[$i-1] };
        $h->GetXaxis()->SetBinLabel($i, $label);
        $h->SetBinContent($i, 0+$rate);
    }
    my $c = TCanvas->new->keep;
    $c->GetPad(0)->SetBottomMargin(0.175);
    $c->GetPad(0)->SetLeftMargin(0.15);
    $c->GetPad(0)->SetRightMargin(0.115);
    $c->GetPad(0)->SetGrid();
    $h->Draw("bar2");
    if ($diagram_output_dir) {
        require File::Path;
        File::Path::mkpath($diagram_output_dir);
        my $file = $opts->{filename} || do {my $f = $opts->{title}; $f =~ s/[^a-zA-Z0-9_\ ]/_/g; $f};
        $c->SaveAs("$diagram_output_dir/$file.png");
    }
}

sub cmpthese_to_sanity {
    my $res = shift;
    my @rows = map {
        my $rate = $_->[1];
        if (not $rate =~ s/\s*\/\s*s$//) {
            $rate = 1/$rate;
        }
        [$_->[0], $rate]
    } grep {defined $_->[0] and $_->[0] =~ /\S/} @$res;
    return \@rows;
}

