#!/usr/bin/env perl
use warnings;
use strict;
use File::Spec;
use lib File::Spec->catdir(qw(t lib));
BEGIN {
    lib->import('lib')
      if !-d 't';
}
use Test::More;
use Sereal::TestSet qw(:all);
use Sereal::Decoder qw(decode_sereal);
use Data::Dumper qw(Dumper);

if ( !have_encoder_and_decoder() ) {
    plan skip_all => 'Did not find right version of encoder';
}

sub dd {
    local $Data::Dumper::Indent = 0;
    local $Data::Dumper::Terse = 1;
    local $Data::Dumper::Sortkeys = 1;
    return Data::Dumper::Dumper($_[0]);
}

my @steps;
{
    package FooInside;

    sub new {
        my ( $class, $attrs, @inside ) = @_;
        $attrs ||= {};
        return bless {
            attrs  => { %$attrs },
            inside => {
                map { $_ => $inside[$_] } 0..$#inside
            },
        }, $class;
    }

    sub FREEZE {
        my ( $self, $serializer ) = @_;

        push @steps, 'FooInside::FREEZE(' . ::dd($self) . ')';
        return {
            attrs  => $self->{attrs},
            inside => [
                map { $self->{inside}{$_} } sort { $a <=> $b } keys %{$self->{inside}},
            ],
        };
    }

    sub THAW {
        my ( $class, $serializer, @data ) = @_;

        push @steps, 'FooInside::THAW(' . ::dd($data[0]) . ')';
        return $class->new( $data[0]->{attrs}, @{ $data[0]->{inside} || [] } );
    }
}

{
    package FooOutside;

    sub new {
        my ( $class, $attrs, @inside ) = @_;
        $attrs ||= {};
        return bless {
            attrs  => { %$attrs },
            inside => {
                map { $_ => $inside[$_] } 0..$#inside
            },
        }, $class;
    }


    sub FREEZE {
        my ( $self, $serializer ) = @_;

        push @steps, 'FooOutside::FREEZE(' . ::dd($self) . ')';
        return {
            attrs  => $self->{attrs},
            inside => [
                map { $self->{inside}{$_} } sort { $a <=> $b } keys %{$self->{inside}},
            ],
        };
    }

    sub THAW {
        my ( $class, $serializer, @data ) = @_;

        push @steps, 'FooOutside::THAW(' . ::dd($data[0]) . ')';
        return $class->new( $data[0]->{attrs}, @{ $data[0]->{inside} || [] } );
    }
}

my $struct = 
    FooOutside->new(
        # Attrs
        {
            attr1 => 'foobar',
            bar => FooInside->new({}, 'should_be_first'),
        },
        FooInside->new({}, 'second/not_frozen', 'third/not_frozen'),
        FooOutside->new({}, FooInside->new({}, 'fourth/inner')),
    );

my $srl = Sereal::Encoder::encode_sereal($struct, { freeze_callbacks => 1, canonical => 1 });
my $dec = decode_sereal($srl);


is_deeply(\@steps, [
          "FooOutside::FREEZE(bless( {'attrs' => {'attr1' => 'foobar','bar' => bless( {'attrs' => {},'inside' => {'0' => 'should_be_first'}}, 'FooInside' )},'inside' => {'0' => bless( {'attrs' => {},'inside' => {'0' => 'second/not_frozen','1' => 'third/not_frozen'}}, 'FooInside' ),'1' => bless( {'attrs' => {},'inside' => {'0' => bless( {'attrs' => {},'inside' => {'0' => 'fourth/inner'}}, 'FooInside' )}}, 'FooOutside' )}}, 'FooOutside' ))",
          "FooInside::FREEZE(bless( {'attrs' => {},'inside' => {'0' => 'should_be_first'}}, 'FooInside' ))",
          "FooInside::FREEZE(bless( {'attrs' => {},'inside' => {'0' => 'second/not_frozen','1' => 'third/not_frozen'}}, 'FooInside' ))",
          "FooOutside::FREEZE(bless( {'attrs' => {},'inside' => {'0' => bless( {'attrs' => {},'inside' => {'0' => 'fourth/inner'}}, 'FooInside' )}}, 'FooOutside' ))",
          "FooInside::FREEZE(bless( {'attrs' => {},'inside' => {'0' => 'fourth/inner'}}, 'FooInside' ))",
          "FooInside::THAW({'attrs' => {},'inside' => ['fourth/inner']})",
          "FooOutside::THAW({'attrs' => {},'inside' => [bless( {'attrs' => {},'inside' => {'0' => 'fourth/inner'}}, 'FooInside' )]})",
          "FooInside::THAW({'attrs' => {},'inside' => ['second/not_frozen','third/not_frozen']})",
          "FooInside::THAW({'attrs' => {},'inside' => ['should_be_first']})",
          "FooOutside::THAW({'attrs' => {'attr1' => 'foobar','bar' => bless( {'attrs' => {},'inside' => {'0' => 'should_be_first'}}, 'FooInside' )},'inside' => [bless( {'attrs' => {},'inside' => {'0' => 'second/not_frozen','1' => 'third/not_frozen'}}, 'FooInside' ),bless( {'attrs' => {},'inside' => {'0' => bless( {'attrs' => {},'inside' => {'0' => 'fourth/inner'}}, 'FooInside' )}}, 'FooOutside' )]})"
        ], "freeze/thaw sequence for nested objects was as expected") or do {
    if ($ENV{DUMP_STEPS}) {
        local $Data::Dumper::Useqq = 1;
        print Dumper(\@steps),"\n";
    }
};

is_deeply( $dec, $struct, "And the final structures match according to is_deeply()");
is( dd($dec), dd($struct), "And the final structures match according to Dumper");

done_testing;
