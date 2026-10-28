use strict;
use warnings;
use Exporter 'import';
our @EXPORT_OK = qw(inputs expected);

sub inputs {
    my ($encoder_version) = @_;
    my $shared = [ 'shared', 17 ];
    my $cycle = {};
    $cycle->{self} = $cycle;
    my $reference = 'reference';
    my $cases = {
        scalars => [ undef, 0, 1, -1, 255, 256, 65536, 1.5, -0.25 ],
        integer_boundaries => [ -2147483649, -2147483648, -2147483647, -32769,
            -32768, -129, -128, -127, -1, 0, 1, 127, 128, 129, 255, 256,
            32767, 32768, 65535, 65536, 2147483647, 2147483648, 4294967295,
            4294967296, (~0 >> 1) ],
        string_length_boundaries => [ map { 'x' x $_ } (0, 1, 31, 32, 255, 256, 65535) ],
        strings => [ "bytes\x{00}\x{ff}", "snowman \x{2603}" ],
        containers => { alpha => [ 'a', 'b' ], omega => { n => 42 } },
        references => [ \$reference ],
        blessed => bless( { kind => 'compatibility fixture', values => [ 1, 2 ] },
            'Sereal::Encoder::Compat::Fixture' ),
        aliases => [ $shared, $shared ],
        cycle => $cycle,
        compressed => 'compressible payload ' x 1000,
    };
    $cases->{regexp} = qr/foo/i if $encoder_version >= 4.025;
    return $cases;
}

sub expected {
    my ($encoder_version) = @_;
    my $shared = [ 'shared', 17 ];
    my $cycle = {};
    $cycle->{self} = $cycle;
    my $reference = 'reference';
    my $cases = {
        scalars => [ undef, 0, 1, -1, 255, 256, 65536, 1.5, -0.25 ],
        integer_boundaries => [ -2147483649, -2147483648, -2147483647, -32769,
            -32768, -129, -128, -127, -1, 0, 1, 127, 128, 129, 255, 256,
            32767, 32768, 65535, 65536, 2147483647, 2147483648, 4294967295,
            4294967296, (~0 >> 1) ],
        string_length_boundaries => [ map { 'x' x $_ } (0, 1, 31, 32, 255, 256, 65535) ],
        strings => [ "bytes\x{00}\x{ff}", "snowman \x{2603}" ],
        containers => { alpha => [ 'a', 'b' ], omega => { n => 42 } },
        references => [ \$reference ],
        blessed => bless( { kind => 'compatibility fixture', values => [ 1, 2 ] },
            'Sereal::Encoder::Compat::Fixture' ),
        aliases => [ $shared, $shared ],
        cycle => $cycle,
        compressed => 'compressible payload ' x 1000,
    };
    $cases->{regexp} = qr/foo/i if $encoder_version >= 4.025;
    return $cases;
}

1;
