#!/usr/bin/env perl
use strict;
use warnings;

use File::Spec;
use lib File::Spec->catdir(qw(t lib));

BEGIN {
    lib->import('lib')
      if !-d 't';
}
use Test::More;
use Sereal::TestSet qw(:all);
use Sereal::Decoder qw(decode_sereal);

if ( !have_encoder_and_decoder() ) {
    plan skip_all => 'Did not find right version of encoder';
}

my $encoder_freeze = Sereal::Encoder->new( { freeze_callbacks => 1 } );

my @order;
{
    package FooInside;

    sub new {
        my ( $class, $attrs, @inside ) = @_;
        $attrs //= {};
        return bless {
            attrs  => { %$attrs },
            inside => {
                map { $_ => $inside[$_] } 0..$#inside
            },
        }, $class;
    }

    sub FREEZE {
        my ( $self, $serializer ) = @_;

        push @order, 'FooInside::FREEZE';
        return {
            attrs  => $self->{attrs},
            inside => [
                map { $self->{inside}{$_} } sort { $a <=> $b } keys %{$self->{inside}},
            ],
        };
    }

    sub THAW {
        my ( $class, $serializer, @data ) = @_;

        push @order, 'FooInside::THAW';
        return $class->new( $data[0]->{attrs}, @{ $data[0]->{inside} // [] } );
    }
}

{
    package FooOutside;

    sub new {
        my ( $class, $attrs, @inside ) = @_;
        $attrs //= {};
        return bless {
            attrs  => { %$attrs },
            inside => {
                map { $_ => $inside[$_] } 0..$#inside
            },
        }, $class;
    }

    sub FREEZE {
        my ( $self, $serializer ) = @_;

        push @order, 'FooOutside::FREEZE';
        return {
            attrs  => $self->{attrs},
            inside => [
                map { $self->{inside}{$_} } sort { $a <=> $b } keys %{$self->{inside}},
            ],
        };
    }

    sub THAW {
        my ( $class, $serializer, @data ) = @_;

        push @order, 'FooOutside::THAW';
        return $class->new( $data[0]->{attrs}, @{ $data[0]->{inside} // [] } );
    }
}

# Serialize, then deserialize, and check order of freeze/thaw.
my $encoded = $encoder_freeze->encode(FooOutside->new({ bar => FooInside->new({}, 'bar') }, FooInside->new({}, 'baz')), { freeze_callbacks => 1 });
my $decoded = decode_sereal($encoded);

my @should_be = (
    'FooOutside::FREEZE',
    'FooInside::FREEZE',
    'FooInside::FREEZE',
    'FooInside::THAW',
    'FooInside::THAW',
    'FooOutside::THAW',
);
is_deeply( \@order, \@should_be, "Correct inside-out freeze/thaw order")
    or diag "Got:  @order\nWant: @should_be";

done_testing;
