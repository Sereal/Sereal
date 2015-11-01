use strict;
use warnings;
use blib;
use Benchmark qw(cmpthese :hireswallclock);
use Sereal::Decoder qw(decode_sereal sereal_decode_with_object);
use Sereal::Encoder qw(encode_sereal sereal_encode_with_object);
use Storable qw(nfreeze thaw);
use Data::Dumper qw(Dumper);


use Getopt::Long qw(GetOptions);
require bytes;

GetOptions(
    'secs|duration=f'                    => \( my $duration           = -5 ),
    'encoder'                            => \( my $encoder            = 0 ),
    'decoder'                            => \( my $decoder            = 0 ),
    'dump|d'                             => \( my $dump               = 0 ),
    'only=s@'                            => \( my $only               = undef ),
    'exclude=s@'                         => \( my $exclude            = undef ),
    'tiny'                               => \( my $tiny_data          = 0 ),
    'small'                              => \( my $small_data         = 0 ),
    'medium'                             => \( my $medium_data        = 0 ),
    'large'                              => \( my $large_data         = 0 ),
    'very_large|very-large|verylarge'    => \( my $very_large_data    = 0 ),
    'no_bless|no-bless|nobless'          => \( my $nobless            = 0 ),
    'sereal_only|sereal-only|serealonly' => \( my $sereal_only        = 0 ),
    'diagrams'                           => \( my $diagrams           = 0 ),
    'diagram_output=s'                   => \( my $diagram_output_dir = "" ),
) or die "Bad option";

my $fail =
  $tiny_data + $small_data + $medium_data + $very_large_data + $large_data - 1;
if ( $fail and $fail > 0 ) {
    die "Only one of --tiny, --small, --medium, --large, --very-large allowed!";
}
$encoder = 1 if not $encoder and not $decoder;

#our %opt = @ARGV;
our %opt;

my $data_set_name;
srand(0);
my $chars = join( "", "a" .. "z", "A" .. "Z" ) x 2;
my @str;
push @str, substr( $chars, int( rand( int( length($chars) / 2 + 1 ) ) ), 10 )
  for 1 .. 1000;
my @rand = map rand, 1 .. 1000;

our (
    $enc, $dec,
    $enc_snappy,        $dec_snappy,
    $enc_zlib_fast,     $dec_zlib_fast,
    $enc_zlib_small,    $dec_zlib_small,
    $jsonxs, $msgpack, $dd_noindent, $dd_indent, $cbor
);
my $storable_tag= "strbl";
my $sereal_tag= "srl";
my %meta = (
    jxs => {
        enc  => '$::jsonxs->encode($data);',
        dec  => '$::jsonxs->decode($encoded);',
        name => 'JSON::XS OO',
        init => sub {
            $jsonxs = JSON::XS->new()->allow_nonref();
        },
        use => 'use JSON::XS qw(decode_json encode_json);',
    },
    ddl => {
        enc  => 'DumpLimited($data);',
        dec  => 'Data::Undump::undump($encoded);',
        name => 'Data::Dump::Limited',
        use  => [
                    'use Data::Undump qw(undump);',
                    'use Data::Dumper::Limited qw(DumpLimited);',
                ],
    },
    mp => {
        enc  => '$::msgpack->pack($data);',
        dec  => '$::msgpack->unpack($encoded);',
        name => 'Data::MsgPack',
        use  => 'use Data::MessagePack;',
        init => sub {
            $msgpack = Data::MessagePack->new();
        },
    },
    cbor => {
        enc  => '$::cbor->encode($data);',
        dec  => '$::cbor->decode($encoded);',
        name => 'CBOR::XS',
        use => 'use CBOR::XS qw(encode_cbor decode_cbor);',
        init => sub {
            $cbor= CBOR::XS->new();
        },
    },
    dd_noind => {
        enc  => 'Data::Dumper->new([$data])->Indent(0)->Dump();',
        dec  => 'eval $encoded;',
        name => 'Data::Dumper no-indent',
    },
    dd => {
        enc  => 'Dumper($data);',
        dec  => 'eval $encoded;',
        name => 'Data::Dumper indented',
    },
    $storable_tag => {
        enc  => 'nfreeze($data);',
        dec  => 'thaw($encoded);',
        name => 'Storable',
    },
    srl_func => {
        enc  => 'encode_sereal($data, $opt);',
        dec  => 'decode_sereal($encoded, $opt);',
        name => 'Sereal functional',
    },
    srl_fwo => {
        enc  => 'sereal_encode_with_object($::enc,$data);',
        dec  => 'sereal_decode_with_object($::dec,$encoded);',
        name => 'Sereal functional with object',
    },
    $sereal_tag => {
        enc  => '$::enc->encode($data);',
        dec  => '$::dec->decode($encoded);',
        name => 'Sereal OO',
        init => sub {
            $enc = Sereal::Encoder->new( %opt ? \%opt : () );
            $dec = Sereal::Decoder->new( \%opt ? \%opt : () );
        },
    },
    srl_snpy => {
        enc  => '$::enc_snappy->encode($data);',
        dec  => '$::dec_snappy->decode($encoded);',
        name => 'Sereal OO snappy',
        init => sub {
            $enc_snappy = Sereal::Encoder->new(
                {
                    %opt,
                    compress => Sereal::Encoder::SRL_SNAPPY
                }
            );
            $dec_snappy = Sereal::Decoder->new( %opt ? \%opt : () );
        },
    },
    srl_zfast => {
        enc  => '$::enc_zlib_fast->encode($data);',
        dec  => '$::dec_zlib_fast->decode($encoded);',
        name => 'Sereal OO zlib fast',
        init => sub {
            $enc_zlib_fast = Sereal::Encoder->new(
                {
                    %opt,
                    compress           => Sereal::Encoder::SRL_ZLIB,
                    compress_level     => 1,
                    compress_threshold => 0,
                }
            );
            $dec_zlib_fast = Sereal::Decoder->new( %opt ? \%opt : () );
        },
    },
    srl_zbest => {
        enc  => '$::enc_zlib_small->encode($data);',
        dec  => '$::dec_zlib_small->decode($encoded);',
        name => 'Sereal OO zib best',
        init => sub {
            $enc_zlib_small = Sereal::Encoder->new(
                {
                    %opt,
                    compress           => Sereal::Encoder::SRL_ZLIB,
                    compress_level     => 10,
                    compress_threshold => 0,
                }
            );
            $dec_zlib_small = Sereal::Decoder->new( %opt ? \%opt : () );
        },
    },
);
if ($only) {
    my @pat= map { split /\s*,\s*/, $_ } @$only;
    $only = {};
    foreach my $key (keys %meta) {
        $key=~/$_/ and $only->{$key}= 1
            for @pat;
    }
    die "Only [@pat] produced no matches!" unless keys %$only;
}
if ($exclude) {
    my @pat= map { split /\s*,\s*/, $_ } @$exclude;
    $exclude = {};
    foreach my $key (keys %meta) {
        $key=~/$_/ and $exclude->{$key}= 1
            for @pat;
    }
    die "Exclude [@pat] produced no matches!" unless keys %$exclude;
}

our %data;
our %encoded;
our %decoded;
our %enc_bench;
our %dec_bench;
foreach my $key ( sort keys %meta ) {
    my $info = $meta{$key};
    $info->{tag}= $key;
    next if $only    and not $only->{$key}    and $key ne $storable_tag;
    next if $exclude and     $exclude->{$key} and $key ne $storable_tag;
    if (my $use= $info->{use}) {
        $use= [$use] unless ref $use;
        $use= join ";\n", @$use, 1;
        unless (eval $use) {
            warn "Can't load dependencies for $info->{name}, skipping\n";
            next;
        }
    }
    $info->{enc}=~s/\$data/\$::data{$key}/g;
    $info->{dec}=~s/\$encoded/\$::encoded{$key}/g;
    $info->{enc}=~s/\$opt/%opt ? "\\%::opt" : ""/ge;
    $info->{dec}=~s/\$opt/%opt ? "\\%::opt" : ""/ge;

    $data{$key}    = make_data();
    $info->{init}->() if $info->{init};
    $encoded{$key} = eval $info->{enc}
      or die "Failed to eval $info->{enc}: $@";
    $decoded{$key} = eval '$::x = ' . $info->{dec} . '; 1'
      or die "Failed to eval $info->{dec}: $@\n$encoded{$key}\n";
    $info->{size}    = bytes::length( $encoded{$key} );
    next if $only    and not $only->{$key};
    next if $exclude and     $exclude->{$key};
    $enc_bench{$key} = '$::x_' . $key . ' = ' . $info->{enc};
    $dec_bench{$key} = '$::x_' . $key . ' = ' . $info->{dec};
}

my $sereal = $encoded{$sereal_tag};
print($sereal), exit if $dump;

my $storable_len = bytes::length($encoded{$storable_tag});
foreach my $info (
    sort { $a->{size} <=> $b->{size} || $a->{name} cmp $b->{name} }
    grep { defined $_->{size} }
    values %meta
) {
    next unless $info->{size};
    if ($info->{tag} eq $storable_tag) {
        printf "%-40s %12d bytes\n",
            $info->{name} . " ($info->{tag})", $info->{size};
    } else {
        printf "%-40s %12d bytes %6.2f%% of $storable_tag\n",
            $info->{name} . " ($info->{tag})", $info->{size},
            $info->{size} / $storable_len * 100;
    }
}

our $x;
my ( $encoder_result, $decoder_result );
if ($encoder) {
    print "\n* Timing encoders\n";
    $encoder_result = cmpthese( $duration, \%enc_bench );
}

if ($decoder) {
    print "\n* Timing decoders\n";
    $decoder_result = cmpthese( $duration, \%dec_bench );
}

sub make_data {
    if ($tiny_data) {
        $data_set_name = "empty hash";
        return {};
    }
    elsif ($small_data) {
        $data_set_name = "small hash";
        return {
            foo => 1,
            bar => [ 100, 101, 102 ],
            str => "this is a \x{df} string which has to be serialized"
        };
    }
    elsif ($medium_data) {
        my @obj = (
            {
                foo => 1,
                bar => [ 100, 101, 102 ],
                str => "this is a \x{df} string which has to be serialized"
            },
            {
                foo => 2,
                bar => [ 103, 103, 106, 999 ],
                str2 =>
                  "this is a \x{df} aaaaaastring which has to be serialized"
            },
            {
                foozle => 3,
                bar    => [100],
                str3 =>
                  "this is a \x{df} string which haaaaadsadas to be serialized"
            },
            {
                foozle => 3,
                bar    => [],
                st4r =>
                  "this is a \x{df} string which has to be sdassdaerialized"
            },
            {
                foo  => 1,
                bar  => [ 100, 101, 102 ],
                s5tr => "this is a \x{df} string which has to be serialized"
            },
            {
                foo => 2,
                bar => [ 103, 103, 106, 999 ],
                str =>
                  "this is a \x{df} aaaaaastring which has to be serialized"
            },
            {
                foozle => 3,
                bar    => [100],
                str =>
                  "this is a \x{df} string which haaaaadsadas to be serialized"
            },
            {
                foozle => 3,
                bar    => [],
                str2 =>
                  "this is a \x{df} string which has to be sdassdaerialized"
            },
            {
                foo2 => -99999,
                bar  => [ 100, 101, 102 ],
                str2 => "this is a \x{df} string which has to be serialized"
            },
            {
                foo2 => 213,
                bar  => [ 103, 103, 106, 999 ],
                str =>
                  "this is a \x{df} aaaaaastring which has to be serialized"
            },
            {
                foozle2 => undef,
                bar     => [100],
                str =>
                  "this is a \x{df} string which haaaaadsadas to be serialized"
            },
            {
                foozle2 => undef,
                bar     => [ 1 .. 20 ],
                str =>
                  "this is a \x{df} string which has to be sdassdaerialized"
            },
        );
        my @classes = qw(Baz Baz Baz3 Baz2 Baz Baz Baz3 Baz2 Baz Baz Baz3 Baz2);
        if ( $nobless ) {
            $data_set_name = "array of small hashes with relations";
        }
        else {
            bless( $obj[$_], $classes[$_] ) for 0 .. $#obj;
            $data_set_name = "array of small objects with relations";
        }
        foreach my $i ( 1 .. $#obj ) {
            $obj[$i]->{parent} = $obj[ $i - 1 ];
        }
        return \@obj;
    }
    elsif ($very_large_data) {    # "large data"
        $data_set_name = "really rather large data structure";
        my @refs = (
            [ 1 .. 10000 ],
            {@str}, {@str}, [ 1 .. 10000 ],
            {@str}, [@rand], {@str}, {@str},
        );
        return [
            \@refs, \@refs,
            [ map { [ reverse 1 .. 100 ] } ( 0 .. 1000 ) ],
            [ map { +{ foo => "bar", baz => "buz" } } 1 .. 2000 ]
        ];
    }
    else {    # "large data"
        $data_set_name = "large data structure";
        return [
            [ map { my $y= "$_"; $_ } 1 .. 10000 ], {@str}, {@str}, [ map { my $y= "$_"; $_ } 1 .. 10000 ],
            {@str}, [@rand], {@str}, {@str},
        ];
    }
}

if ($diagrams) {
    require SOOT;
    SOOT::Init(0);
    SOOT->import(":all");

    my ( $enc_data, $dec_data );
    $enc_data = cmpthese_to_sanity($encoder_result) if $encoder_result;
    $dec_data = cmpthese_to_sanity($decoder_result) if $decoder_result;

    foreach my $dia (
        [ "Encoder performance [1/s]", $enc_data ],
        [ "Decoder performance [1/s]", $dec_data ],
      )
    {
        my ( $title, $d ) = @$dia;
        next if not $d;
        $_->[0] =~ s/_/ /g, $_->[0] =~ s/sereal /sereal, / for @$d;
        make_bar_chart(
            substr( $title, 0, 3 ),
            $d,
            {
                title    => $title,
                filename => do {
                    my $x = $title;
                    $x =~ s/\[1\/s\]/per second/;
                    $data_set_name . " - " . $x;
                },
            }
        );
    }

    my %names = (
        "JSON::XS"                => 'json xs',
        "Data::Dumper::Limited"   => 'ddl',
        "Data::MessagePack"       => "msgpack",
        "Data::Dumper (1)"        => "dd noindent",
        "Data::Dumper (2)"        => "dd",
        "Storable"                => 'storable',
        "Sereal::Encoder"         => 'sereal',
        "Sereal::Encoder, Snappy" => 'sereal, snappy',
    );

    make_bar_chart(
        "size",
        [
            sort { $b->[1] <=> $a->[1] }
            map { $_->{size} ? [ $_->{name}, $_->{size} ] : () } values %meta
        ],
        {
            title    => "Encoded output sizes [bytes]",
            color    => kRed(),
            filename => $data_set_name . " - Encoded output sizes in bytes",
        },
    );
    SOOT->Run if not $diagram_output_dir;
}

sub make_bar_chart {
    my ( $name, $data, $opts ) = @_;
    my $h = TH1D->new( $name, ( $opts->{title} || $name ),
        scalar(@$data), -0.5, scalar(@$data) - 0.5 );
    $h->keep;
    $h->SetFillColor( $opts->{color} || kBlue() );
    $h->SetBarOffset(0.12);
    $h->SetBarWidth(0.74);
    $h->SetStats(0);
    $h->GetXaxis()->SetLabelSize(0.06);
    $h->GetXaxis()->SetLabelOffset(0.009);
    $h->GetYaxis()->SetTitle( $opts->{title} ) if defined $opts->{title};
    $h->GetYaxis()->SetTitleSize(0.045);

    for my $i ( 1 .. @$data ) {
        my ( $label, $rate ) = @{ $data->[ $i - 1 ] };
        $h->GetXaxis()->SetBinLabel( $i, $label );
        $h->SetBinContent( $i, 0 + $rate );
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
        my $file = $opts->{filename}
          || do { my $f = $opts->{title}; $f =~ s/[^a-zA-Z0-9_\ ]/_/g; $f };
        $c->SaveAs("$diagram_output_dir/$file.png");
    }
}

sub cmpthese_to_sanity {
    my $res  = shift;
    my @rows = map {
        my $rate = $_->[1];
        if ( not $rate =~ s/\s*\/\s*s$// ) {
            $rate = 1 / $rate;
        }
        [ $_->[0], $rate ]
    } grep { defined $_->[0] and $_->[0] =~ /\S/ } @$res;
    return \@rows;
}
print "\n";
