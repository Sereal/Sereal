package Sereal::Path;

use 5.008;
use strict qw(vars refs);

our $AUTHORITY = 'cpan:TOBYINK';
our $VERSION   = '0.1';

use Carp;
use Scalar::Util qw[blessed];
use Sereal::Path::Iterator;

sub new {
    defined $_[1] or "first argument must be defined";

    my $iter;
    if (blessed($_[1]) && $_[1]->isa(Sereal::Path::Iterator)) {
        $iter = $_[1];
    } else {
        $iter = Sereal::Path::Iterator->new($_[1]);
    }

    bless {
        iter       => $iter,
        obj        => undef,
        resultType => 'VALUE',
        result     => [],
    }, $_[0];
}

sub _normalize {
    my ($self, $x) = @_;
    $x =~ s/[\['](\??\(.*?\))[\]']/_callback_01($self,$1)/eg;
    $x =~ s/'?\.'?|\['?/;/g;
    $x =~ s/;;;|;;/;..;/g;
    $x =~ s/;\$|'?\]|'$//g;
    $x =~ s/#([0-9]+)/_callback_02($self,$1)/eg;
    $self->{'result'} = [];   # result array was temporarily used as a buffer
    return $x;
}

sub _store {
    my ($self, $path, $value) = @_;
    push @{ $self->{'result'} }, ( $self->{'resultType'} eq "PATH"
                                   ? $self->asPath($path)
                                   : $value ) if $path;
    return !!$path;
}

sub traverse {
    my ($self, $expr) = @_;

    my $norm = $self->_normalize($expr);
    $norm =~ s/^\$;//;

    $self->{iter}->reset;
    return $self->_trace_next_object($norm, '$');
}

sub _trace_next_object {
    my ($self, $expr, $path) = @_;
    my $iter = $self->{iter};

    warn("_trace_next_object: expr=$expr path=$path");

    return if $iter->eof;
    return $self->_store($path, $iter->decode) if "$expr" eq '';
    
    my ($loc, $x);
    {
        my @x = split /\;/, $expr;
        $loc  = shift @x;
        $x    = join ';', @x;
    }

    my ($type, $cnt) = $iter->info;
    warn("_trace_next_object: type=$type cnt=$cnt loc=$loc");

    if ($type eq 'ARRAY' && $loc =~ /^[0-9]+$/ && $loc < $cnt) {
        warn("_trace_next_object: ARRAY loc=$loc");
        # /^\-?[0-9]+$/ # TODO add support of negative $loc
        $iter->step_in;
        $iter->next foreach (1..$loc); # TODO $iter->array_goto
        return $self->_trace_next_object($x, sprintf('%s;%s', $path, $loc));
    }

    if ($type eq 'HASH') {
        my $depth = $iter->stack_depth;
        warn("_trace_next_object: HASH");

        $iter->step_in;
        if ($iter->hash_exists($loc)) {
            warn("_trace_next_object: HASH key found=$loc");
            return $self->_trace_next_object($x, sprintf('%s;%s', $path, $loc))
        }

        warn("_trace_next_object: HASH key not found=$loc");
        $iter->srl_next_at_depth($depth);
    }

    if ($loc eq '*') {
        if ($type eq 'ARRAY') {
            warn("_trace_next_object: WALK ARRAY");

            $iter->step_in;
            my $depth = $iter->stack_depth;

            foreach (1..$cnt) {
                warn("_trace_next_object: WALK ARRAY offset=" . $iter->offset . " iter=" . $_);
                $self->_trace_next_object($x, $path);
                if ($iter->stack_depth > $depth) {
                    warn("_trace_next_object: WALK ARRAY srl_next_at_depth($depth)");
                    $iter->srl_next_at_depth($depth)
                }
            }
        } elsif ($type eq 'HASH') {
            die("!!!");
        }

        #return $self->walk($loc, $x, $path, \&_callback_03);
    }

    warn("_trace_next_object: end of function");
}

sub walk {
    my ($self, $loc, $expr, $path, $f) = @_;
    my $iter = $self->{iter};
    my ($type, $cnt) = $iter->info;
    
    if ($type eq 'ARRAY') {
        $iter->step_in;
        my $depth = $iter->stack_depth;

        for (my $i = 0; $i < $cnt; $i++) {
            $f->($self, $i, $loc, $expr, $path);

            my ($self, $m, $loc, $expr, $path) = @_;
            $self->trace($m . ";" . $expr, $path);

            if ($iter->stack_depth > $depth) {
                $iter->continue_until_depth($depth)
            }
        }
    } elsif ($type eq 'HASH') {
        $iter->step_in;
        my $depth = $iter->depth;
        warn("------- HASH $depth ");

        for (my $i = 0; $i < $cnt; $i++) {
            $iter->next;
            $f->($self, $i, $loc, $expr, $path);
            $iter->continue_until_depth($depth)
                if $iter->stack_depth > $depth;
        }
    } else {
        croak('walk called on non hashref/arrayref value, died');
    }
}

sub _trace_object {
}

sub _trace {
    my ($self, $expr, $path) = @_;
    my $iter = $self->{iter};

    return if $iter->eof;
    return $self->store($path, $iter->decode) if "$expr" eq '';
    
    my ($loc, $x);
    {
        my @x = split /\;/, $expr;
        $loc  = shift @x;
        $x    = join ';', @x;
    }

    my ($type, $cnt) = $iter->info;
    if ($type eq 'ARRAY' && $loc =~ /^[0-9]+$/ && $loc < $cnt) {
        # /^\-?[0-9]+$/ # TODO add support of negative $loc
        $iter->step_in;
        $iter->next foreach (1..$loc);
        return $self->trace($x, sprintf('%s;%s', $path, $loc));
    }

    if ($type eq 'HASH') {
        $iter->step_in;
        return $self->trace($x, sprintf('%s;%s', $path, $loc))
            if $iter->hash_exists($loc);
        $iter->step_out;
    }

    if ($loc eq '*') {
        return $self->walk($loc, $x, $path, \&_callback_03);
    }
}

sub _callback_01 {
    my ($self, $m1) = @_;
    push @{ $self->{'result'} }, $m1;
    my $last_index = scalar @{ $self->{'result'} } - 1;
    return "[#${last_index}]";
}

sub _callback_02 {
    my ($self, $m1) = @_;
    return $self->{'result'}->[$m1];
}

1;

__END__

=head1 AUTHOR

Ivan Kruglov <ivan.kruglov@yahoo.com>

This module is pretty much a straight line-by-line port of the 
JSON::Path module by Toby Inkster which is a port of the PHP JSONPath
implementation (version 0.8.x) by Stefan Goessner. See
L<http://code.google.com/p/jsonpath/>.

=head1 COPYRIGHT AND LICENCE

Copyright 2007 Stefan Goessner.

Copyright 2010-2013 Toby Inkster.

Copyright 2014 Ivan Kruglov.

This module is tri-licensed. It is available under the X11 (a.k.a. MIT)
licence; you can also redistribute it and/or modify it under the same
terms as Perl itself.

=head2 a.k.a. "The MIT Licence"

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
