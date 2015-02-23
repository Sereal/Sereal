package Sereal::Path;

use 5.008;
use strict qw(vars refs);
no warnings;

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

sub normalize {
    my ($self, $x) = @_;
    $x =~ s/[\['](\??\(.*?\))[\]']/_callback_01($self,$1)/eg;
    $x =~ s/'?\.'?|\['?/;/g;
    $x =~ s/;;;|;;/;..;/g;
    $x =~ s/;\$|'?\]|'$//g;
    $x =~ s/#([0-9]+)/_callback_02($self,$1)/eg;
    $self->{'result'} = [];   # result array was temporarily used as a buffer
    return $x;
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

sub asPath {
    my ($self, $path) = @_;
    my @x = split /\;/, $path;
    my $p = '$';
    my $n = scalar(@x);
    for (my $i=1; $i<$n; $i++) {
        $p .= /^[0-9]+$/ ? ("[".$x[$i]."]") : ("['".$x[$i]."']");
    }

    return $p;
}

sub store {
    my ($self, $path, $value) = @_;
    push @{ $self->{'result'} }, ( $self->{'resultType'} eq "PATH"
                                   ? $self->asPath($path)
                                   : $value ) if $path;
    return !!$path;
}

sub trace {
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

    my $iter_type = $iter->type;
    if ($iter_type eq 'ARRAY' && $loc =~ /^[0-9]+$/) { # /^\-?[0-9]+$/
        my $iter_count = $iter->count;
        if ($loc < $iter_count) { # TODO add support of negative $loc
            $iter->step or die "failed to do step";
            for (my $i = 0; $i < $loc; $i++) {
                $iter->next or die "failed to do next";
            }

            return $self->trace($x, sprintf('%s;%s', $path, $loc));
        }
    }

    if ($iter_type eq 'HASH') {
        $iter->step or die "failed to do step";
        if ($iter->find_key($loc)) {
            return $self->trace($x, sprintf('%s;%s', $path, $loc));
        }

        $iter->parent or die "failed to goto parent";
    }

    if ($loc eq '*') {
        return $self->walk($loc, $x, $path, \&_callback_03);
    }

    #elsif ($loc eq '..')
    #{
    #    $self->trace($x, $val, $path);
    #    $self->walk($loc, $x, $val, $path, \&_callback_04);
    #}

    if ($loc =~ /\,/) { # [name1,name2,...]
        $self->trace($_ . ';' . $x, $path) foreach split /\,/, $loc;
        return;
    }

    #elsif ($loc =~ /^\(.*?\)$/) # [(expr)]
    #{
    #    my $evalx = $self->evalx($loc, $val, substr($path, rindex($path,";")+1));
    #    $self->trace($evalx.';'.$x, $val, $path);
    #}
    #elsif ($loc =~ /^\?\(.*?\)$/) # [?(expr)]
    #{
    #    # my $evalx = $self->evalx($loc, $val, substr($path, rindex($path,";")+1));
    #    $self->walk($loc, $x, $val, $path, \&_callback_05);
    #}
    #elsif ($loc =~ /^(-?[0-9]*):(-?[0-9]*):?(-?[0-9]*)$/) # [start:end:step]  python slice syntax
    #{
    #    $self->slice($loc, $x, $iter, $path);
    #}
}

sub _callback_03 {
    my ($self, $m, $loc, $expr, $path) = @_;
    $self->trace($m . ";" . $expr, $path);
}

#sub _callback_04
#{
#    my ($self, $m, $l, $x, $v, $p) = @_;
#    
#    if (isArray($v)
#    and isArray($v->[$m]) || isObject($v->[$m]))
#    {
#        $self->trace("..;".$x, $v->[$m], $p.";".$m);
#    }
#    elsif (isObject($v)
#    and isArray($v->{$m}) || isObject($v->{$m}))
#    {
#        $self->trace("..;".$x, $v->{$m}, $p.";".$m);
#    }
#}
#
#sub _callback_05
#{
#    my ($self, $m, $l, $x, $v, $p) = @_;
#    
#    $l =~ s/^\?\((.*?)\)$/$1/g;
#    
#    my $evalx;
#    if (isArray($v))
#    {
#        $evalx = $self->evalx($l, $v->[$m]);
#    }
#    elsif (isObject($v))
#    {
#        $evalx = $self->evalx($l, $v->{$m});
#    }
#    
#    $self->trace($m.";".$x, $v, $p)
#        if $evalx;
#}

sub walk {
    my ($self, $loc, $expr, $path, $f) = @_;
    my $iter = $self->{iter};
    my $iter_type = $iter->type;
    
    if ($iter_type eq 'ARRAY') {
        my $cnt = $iter->count;
        for (my $i = 0; $i < $cnt; $i++) {
            $f->($self, $i, $loc, $expr, $path);
        }
    } elsif ($iter_type eq 'HASH') {
        my $cnt = $iter->count;
        for (my $i = 0; $i < $cnt; $i++) {
            $iter->next or die "failed to do next";
            $f->($self, $i, $loc, $expr, $path);
        }
    } else {
        croak('walk called on non hashref/arrayref value, died');
    }
}

#sub slice {
#    my ($self, $loc, $expr, $v, $path) = @_;
#    
#    $loc =~ s/^(-?[0-9]*):(-?[0-9]*):?(-?[0-9]*)$/$1:$2:$3/;
#    my @s   = split /\:/, $loc;
#    my $len = scalar @$v;
#    
#    my $start = $s[0]+0 ? $s[0]+0 : 0;
#    my $end   = $s[1]+0 ? $s[1]+0 : $len;
#    my $step  = $s[2]+0 ? $s[2]+0 : 1;
#    
#    $start = ($start < 0) ? max(0,$start+$len) : min($len,$start);
#    $end   = ($end < 0)   ? max(0,$end+$len)   : min($len,$end);
#    
#    for (my $i=$start; $i<$end; $i+=$step)
#    {
#        $self->trace($i.";".$expr, $v, $path);
#    }
#}
#
#sub max { return $_[0] > $_[1] ? $_[0] : $_[1] }
#sub min { return $_[0] < $_[1] ? $_[0] : $_[1] }
#
#sub evalx
#{
#    my ($self, $x, $v, $vname) = @_;
#    
#    croak('non-safe evaluation, died') if $JSON::Path::Safe;
#        
#    my $expr = $x;
#    $expr =~ s/\$root/\$self->{'obj'}/g;
#    $expr =~ s/\$_/\$v/g;
#    
#    local $@ = undef;
#    my $res = eval $expr;
#    
#    if ($@)
#    {
#        croak("eval failed: `$expr`, died");
#    }
#    
#    return $res;
#}

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
