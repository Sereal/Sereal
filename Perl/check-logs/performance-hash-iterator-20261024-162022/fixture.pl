use strict;
use warnings;
my ($tiny_data, $small_data, $medium_data, $large_data, $very_large_data) = (0,0,1,0,0);
my $nobless = $ENV{SEREAL_FIXTURE_PLAIN};
my ($data_set_name, @str, @rand);
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
        my @obj = ( {
                foo => 1,
                bar => [ 100, 101, 102 ],
                str => "this is a \x{df} string which has to be serialized"
            },
            {
                foo  => 2,
                bar  => [ 103, 103, 106, 999 ],
                str2 => "this is a \x{df} aaaaaastring which has to be serialized"
            },
            {
                foozle => 3,
                bar    => [100],
                str3   => "this is a \x{df} string which haaaaadsadas to be serialized"
            },
            {
                foozle => 3,
                bar    => [],
                st4r   => "this is a \x{df} string which has to be sdassdaerialized"
            },
            {
                foo  => 1,
                bar  => [ 100, 101, 102 ],
                s5tr => "this is a \x{df} string which has to be serialized"
            },
            {
                foo => 2,
                bar => [ 103, 103, 106, 999 ],
                str => "this is a \x{df} aaaaaastring which has to be serialized"
            },
            {
                foozle => 3,
                bar    => [100],
                str    => "this is a \x{df} string which haaaaadsadas to be serialized"
            },
            {
                foozle => 3,
                bar    => [],
                str2   => "this is a \x{df} string which has to be sdassdaerialized"
            },
            {
                foo2 => -99999,
                bar  => [ 100, 101, 102 ],
                str2 => "this is a \x{df} string which has to be serialized"
            },
            {
                foo2 => 213,
                bar  => [ 103, 103, 106, 999 ],
                str  => "this is a \x{df} aaaaaastring which has to be serialized"
            },
            {
                foozle2 => undef,
                bar     => [100],
                str     => "this is a \x{df} string which haaaaadsadas to be serialized"
            },
            {
                foozle2 => undef,
                bar     => [ 1 .. 20 ],
                str     => "this is a \x{df} string which has to be sdassdaerialized"
            },
        );
        my @classes = qw(Baz Baz Baz3 Baz2 Baz Baz Baz3 Baz2 Baz Baz Baz3 Baz2);
        if ($nobless) {
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
            [ map { +{ foo => "bar", baz => "buz" } } 1 .. 2000 ] ];
    }
    else {                        # "large data"
        $data_set_name = "large data structure";
        return [
            [ map { my $y = "$_"; $_ } 1 .. 10000 ], {@str}, {@str},
            [ map { my $y = "$_"; $_ } 1 .. 10000 ],
            {@str}, [@rand], {@str}, {@str},
        ];
    }
}

make_data();
