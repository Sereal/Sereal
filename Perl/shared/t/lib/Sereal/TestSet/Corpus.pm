package Sereal::TestSet::Corpus;

use strict;
use warnings;
use Exporter qw(import);
use File::Path qw(mkpath remove_tree);
use File::Spec;

our @EXPORT_OK = qw(
    write_packet_set read_packet has_source packet_names remove_sources
    prune_unreferenced pack_corpus unpack_corpus
);

my %PACK_CACHE;

sub _hash {
    my ($bytes) = @_;
    my $function = Sereal::Decoder->can('siphash36_128')
        || Sereal::Encoder->can('siphash36_128')
        or die 'Neither Sereal::Decoder nor Sereal::Encoder provides siphash36_128';
    return unpack 'H*', $function->($bytes);
}

sub _read_file {
    my ($path) = @_;
    open my $fh, '<:raw', $path or die "Failed to read '$path': $!";
    local $/;
    my $data = <$fh>;
    close $fh or die "Failed to close '$path': $!";
    return $data;
}

sub _write_file {
    my ( $path, $data ) = @_;
    my $tmp = "$path.$$";
    open my $fh, '>:raw', $tmp or die "Failed to write '$tmp': $!";
    print {$fh} $data or die "Failed to write '$tmp': $!";
    close $fh or die "Failed to close '$tmp': $!";
    rename $tmp, $path or die "Failed to rename '$tmp' to '$path': $!";
}

sub _pack_path { return File::Spec->catfile( $_[0], 'corpus.pack' ) }

sub _load_pack_index {
    my ($root) = @_;
    my $path = _pack_path($root);
    my @stat = stat $path;
    die "Failed to stat '$path': $!" unless @stat;
    my $cache_key = "$path:$stat[7]:$stat[9]";
    return $PACK_CACHE{$cache_key} if $PACK_CACHE{$cache_key};

    open my $fh, '<:raw', $path or die "Failed to read '$path': $!";
    read( $fh, my $header, 16 ) == 16 or die "Truncated corpus header in '$path'";
    die "Invalid corpus magic in '$path'" unless substr( $header, 0, 8 ) eq "SRLPC02\n";
    my $index_offset = unpack 'Q<', substr( $header, 8, 8 );
    seek( $fh, $index_offset, 0 ) or die "Failed to seek to corpus index in '$path': $!";
    local $/;
    my $text = <$fh>;
    close $fh or die "Failed to close '$path': $!";

    my %sources;
    for my $line ( split /\n/, $text ) {
        next if $line eq '' || $line =~ /^#/;
        my @field = split /\t/, $line, -1;
        my $kind = shift @field;
        if ( $kind eq 'S' && @field == 2 ) {
            my ( $id, $version ) = @field;
            $sources{$id} = [ $version, {} ];
        }
        elsif ( $kind eq 'M' && @field == 4 ) {
            my ( $id, $name, $offset, $length ) = @field;
            die "invalid packed corpus manifest row '$line'"
                unless exists $sources{$id} && $offset =~ /^\d+$/ && $length =~ /^\d+$/;
            $sources{$id}[1]{$name} = [ $offset, $length ];
        }
        else {
            die "invalid packed corpus index row '$line'";
        }
    }
    my $index = {
        path      => $path,
        sources   => \%sources,
    };
    $PACK_CACHE{$cache_key} = $index;
    return $index;
}

sub _read_packed_packet {
    my ( $index, $offset, $length ) = @_;
    open my $fh, '<:raw', $index->{path} or die "Failed to read '$index->{path}': $!";
    seek( $fh, $offset, 0 ) or die "Failed to seek to packet at offset $offset: $!";
    my $packet = '';
    my $read = read( $fh, $packet, $length );
    close $fh or die "Failed to close '$index->{path}': $!";
    die "Truncated packed packet at offset $offset" unless defined($read) && $read == $length;
    return $packet;
}

sub write_packet_set {
    my ( $root, $source, $encoder_version, $packets ) = @_;
    die 'source must be a non-empty name without tabs or newlines'
        if !defined($source) || $source eq '' || $source =~ /[\t\r\n]/;
    die 'encoder version must be a non-empty value without tabs or newlines'
        if !defined($encoder_version) || $encoder_version eq '' || $encoder_version =~ /[\t\r\n]/;
    die 'packets must be a hash reference' if ref($packets) ne 'HASH';

    my $packets_dir   = File::Spec->catdir( $root, 'packets' );
    my $manifests_dir = File::Spec->catdir( $root, 'manifests' );
    mkpath( [$packets_dir, $manifests_dir] );

    my $manifest = "# sereal-packet-manifest-v1\n";
    for my $name ( sort keys %$packets ) {
        die "invalid packet name '$name'" if $name eq '' || $name =~ /[\t\r\n]/;
        my $packet = $packets->{$name};
        die "packet '$name' is undefined" if !defined $packet;
        my $hash = _hash($packet);
        my $path = File::Spec->catfile( $packets_dir, "$hash.srl" );
        if ( -e $path ) {
            die "hash collision or corrupted packet '$path'" if _read_file($path) ne $packet;
        }
        else {
            _write_file( $path, $packet );
        }
        $manifest .= "$name\t$hash\n";
    }

    my $manifest_hash = _hash($manifest);
    my $manifest_path = File::Spec->catfile( $manifests_dir, "$manifest_hash.tsv" );
    if ( -e $manifest_path ) {
        die "hash collision or corrupted manifest '$manifest_path'"
            if _read_file($manifest_path) ne $manifest;
    }
    else {
        _write_file( $manifest_path, $manifest );
    }

    my $sources_path = File::Spec->catfile( $root, 'sources.tsv' );
    my %sources;
    if ( -e $sources_path ) {
        for my $line ( split /\n/, _read_file($sources_path) ) {
            next if $line eq '' || $line =~ /^#/;
            my ( $id, $version, $hash ) = split /\t/, $line, 3;
            die "invalid source index line '$line'" unless defined $hash;
            $sources{$id} = [ $version, $hash ];
        }
    }
    $sources{$source} = [ $encoder_version, $manifest_hash ];
    my $source_index = "# source\tencoder_version\tmanifest_hash\n";
    $source_index .= join "\n",
        map { join "\t", $_, @{ $sources{$_} } } sort keys %sources;
    $source_index .= "\n";
    _write_file( $sources_path, $source_index );

    return $manifest_hash;
}

sub read_packet {
    my ( $root, $source, $name ) = @_;
    if ( -f _pack_path($root) ) {
        my $index = _load_pack_index($root);
        my $record = $index->{sources}{$source}
            or die "unknown packet corpus source '$source'";
        my $entry = $record->[1]{$name}
            or die "unknown packet '$name' in corpus source '$source'";
        return _read_packed_packet( $index, @$entry );
    }
    my $sources_path = File::Spec->catfile( $root, 'sources.tsv' );
    my %sources;
    for my $line ( split /\n/, _read_file($sources_path) ) {
        next if $line eq '' || $line =~ /^#/;
        my ( $id, $version, $hash ) = split /\t/, $line, 3;
        die "invalid source index line '$line'" unless defined $hash;
        $sources{$id} = [ $version, $hash ];
    }
    my $source_record = $sources{$source}
        or die "unknown packet corpus source '$source'";
    my $manifest_hash = $source_record->[1];
    my $manifest_path = File::Spec->catfile( $root, 'manifests', "$manifest_hash.tsv" );
    my $manifest = _read_file($manifest_path);
    die "manifest hash mismatch for '$manifest_path'" if _hash($manifest) ne $manifest_hash;

    my %packets;
    for my $line ( split /\n/, $manifest ) {
        next if $line eq '' || $line =~ /^#/;
        my ( $packet_name, $hash ) = split /\t/, $line, 2;
        die "invalid manifest line '$line'" unless defined $hash && $hash =~ /^[0-9a-f]{32}$/;
        $packets{$packet_name} = $hash;
    }
    my $packet_hash = $packets{$name}
        or die "unknown packet '$name' in corpus source '$source'";
    my $packet_path = File::Spec->catfile( $root, 'packets', "$packet_hash.srl" );
    my $packet = _read_file($packet_path);
    die "packet hash mismatch for '$packet_path'" if _hash($packet) ne $packet_hash;
    return $packet;
}

sub has_source {
    my ( $root, $source ) = @_;
    if ( -f _pack_path($root) ) {
        my $index = _load_pack_index($root);
        return exists $index->{sources}{$source};
    }
    my $sources_path = File::Spec->catfile( $root, 'sources.tsv' );
    return 0 unless -f $sources_path;
    for my $line ( split /\n/, _read_file($sources_path) ) {
        next if $line eq '' || $line =~ /^#/;
        my ($id) = split /\t/, $line, 2;
        return 1 if defined($id) && $id eq $source;
    }
    return 0;
}

sub packet_names {
    my ( $root, $source ) = @_;
    if ( -f _pack_path($root) ) {
        my $index = _load_pack_index($root);
        my $record = $index->{sources}{$source}
            or die "unknown packet corpus source '$source'";
        return sort keys %{ $record->[1] };
    }
    my $sources_path = File::Spec->catfile( $root, 'sources.tsv' );
    my $manifest_hash;
    for my $line ( split /\n/, _read_file($sources_path) ) {
        next if $line eq '' || $line =~ /^#/;
        my ( $id, $version, $hash ) = split /\t/, $line, 3;
        $manifest_hash = $hash if defined($id) && $id eq $source;
    }
    die "unknown packet corpus source '$source'" unless defined $manifest_hash;
    my $path = File::Spec->catfile( $root, 'manifests', "$manifest_hash.tsv" );
    my $manifest = _read_file($path);
    die "manifest hash mismatch for '$path'" if _hash($manifest) ne $manifest_hash;
    my @names;
    for my $line ( split /\n/, $manifest ) {
        next if $line eq '' || $line =~ /^#/;
        my ( $name, $hash ) = split /\t/, $line, 2;
        die "invalid manifest line '$line'" unless defined $hash;
        push @names, $name;
    }
    return @names;
}

sub pack_corpus {
    my ($root) = @_;
    my $sources_path = File::Spec->catfile( $root, 'sources.tsv' );
    my ( %sources, %manifests, %packets );
    for my $line ( split /\n/, _read_file($sources_path) ) {
        next if $line eq '' || $line =~ /^#/;
        my ( $id, $version, $hash ) = split /\t/, $line, 3;
        die "invalid source index line '$line'" unless defined $hash && $hash =~ /^[0-9a-f]{32}$/;
        $sources{$id} = [ $version, $hash ];
    }
    for my $hash ( map { $_->[1] } values %sources ) {
        next if $manifests{$hash};
        my $path = File::Spec->catfile( $root, 'manifests', "$hash.tsv" );
        my $text = _read_file($path);
        die "manifest hash mismatch for '$path'" if _hash($text) ne $hash;
        my %rows;
        for my $line ( split /\n/, $text ) {
            next if $line eq '' || $line =~ /^#/;
            my ( $name, $packet_hash ) = split /\t/, $line, 2;
            die "invalid manifest line '$line'"
                unless defined $packet_hash && $packet_hash =~ /^[0-9a-f]{32}$/;
            $rows{$name} = $packet_hash;
            $packets{$packet_hash} = 1;
        }
        $manifests{$hash} = \%rows;
    }

    my $path = _pack_path($root);
    my $tmp  = "$path.$$";
    open my $out, '>:raw', $tmp or die "Failed to write '$tmp': $!";
    print {$out} "SRLPC02\n", pack( 'Q<', 0 ) or die "Failed to write corpus header: $!";
    my %packet_offsets;
    for my $hash ( sort keys %packets ) {
        my $packet_path = File::Spec->catfile( $root, 'packets', "$hash.srl" );
        my $packet = _read_file($packet_path);
        die "packet hash mismatch for '$packet_path'" if _hash($packet) ne $hash;
        my $offset = tell($out);
        print {$out} $packet or die "Failed to write packet '$hash': $!";
        $packet_offsets{$hash} = [ $offset, length($packet) ];
    }
    my $index_offset = tell($out);
    my $index = "# sereal-packet-corpus-v2\n";
    for my $id ( sort keys %sources ) {
        $index .= join( "\t", 'S', $id, $sources{$id}[0] ) . "\n";
        my $manifest_hash = $sources{$id}[1];
        for my $name ( sort keys %{ $manifests{$manifest_hash} } ) {
            my $hash = $manifests{$manifest_hash}{$name};
            $index .= join( "\t", 'M', $id, $name, @{ $packet_offsets{$hash} } ) . "\n";
        }
    }
    print {$out} $index or die "Failed to write corpus index: $!";
    seek( $out, 8, 0 ) or die "Failed to seek to corpus header: $!";
    print {$out} pack( 'Q<', $index_offset ) or die "Failed to update corpus index offset: $!";
    close $out or die "Failed to close '$tmp': $!";
    rename $tmp, $path or die "Failed to rename '$tmp' to '$path': $!";

    unlink $sources_path or die "Failed to remove '$sources_path': $!";
    for my $dir ( qw(manifests packets) ) {
        my $dir_path = File::Spec->catdir( $root, $dir );
        remove_tree($dir_path);
    }
    %PACK_CACHE = ();
    return $path;
}

sub unpack_corpus {
    my ($root) = @_;
    return 0 unless -f _pack_path($root);
    my $index = _load_pack_index($root);
    my $packets_dir   = File::Spec->catdir( $root, 'packets' );
    my $manifests_dir = File::Spec->catdir( $root, 'manifests' );
    mkpath( [$packets_dir, $manifests_dir] );
    my $sources = "# source\tencoder_version\tmanifest_hash\n";
    for my $id ( sort keys %{ $index->{sources} } ) {
        my ( $version, $rows ) = @{ $index->{sources}{$id} };
        my $manifest = "# sereal-packet-manifest-v1\n";
        for my $name ( sort keys %$rows ) {
            my ( $offset, $length ) = @{ $rows->{$name} };
            my $packet = _read_packed_packet( $index, $offset, $length );
            my $hash = _hash($packet);
            my $packet_path = File::Spec->catfile( $packets_dir, "$hash.srl" );
            _write_file( $packet_path, $packet ) unless -e $packet_path;
            $manifest .= "$name\t$hash\n";
        }
        my $manifest_hash = _hash($manifest);
        _write_file( File::Spec->catfile( $manifests_dir, "$manifest_hash.tsv" ), $manifest );
        $sources .= "$id\t$version\t$manifest_hash\n";
    }
    $sources .= "\n";
    _write_file( File::Spec->catfile( $root, 'sources.tsv' ), $sources );
    return 1;
}

sub remove_sources {
    my ( $root, @remove ) = @_;
    my %remove = map { $_ => 1 } @remove;
    my $path = File::Spec->catfile( $root, 'sources.tsv' );
    my %sources;
    for my $line ( split /\n/, _read_file($path) ) {
        next if $line eq '' || $line =~ /^#/;
        my ( $id, $version, $hash ) = split /\t/, $line, 3;
        die "invalid source index line '$line'" unless defined $hash;
        $sources{$id} = [ $version, $hash ] unless $remove{$id};
    }
    my $index = "# source\tencoder_version\tmanifest_hash\n";
    $index .= join "\n", map { join "\t", $_, @{ $sources{$_} } } sort keys %sources;
    $index .= "\n";
    _write_file( $path, $index );
}

sub prune_unreferenced {
    my ($root) = @_;
    my $sources_path = File::Spec->catfile( $root, 'sources.tsv' );
    my ( %manifests, %packets );
    for my $line ( split /\n/, _read_file($sources_path) ) {
        next if $line eq '' || $line =~ /^#/;
        my ( $id, $version, $hash ) = split /\t/, $line, 3;
        die "invalid source index line '$line'" unless defined $hash && $hash =~ /^[0-9a-f]{32}$/;
        $manifests{$hash} = 1;
    }
    for my $hash ( keys %manifests ) {
        my $path = File::Spec->catfile( $root, 'manifests', "$hash.tsv" );
        for my $line ( split /\n/, _read_file($path) ) {
            next if $line eq '' || $line =~ /^#/;
            my ( $name, $packet_hash ) = split /\t/, $line, 2;
            die "invalid manifest line '$line'"
                unless defined $packet_hash && $packet_hash =~ /^[0-9a-f]{32}$/;
            $packets{$packet_hash} = 1;
        }
    }

    for my $dir ( [ 'manifests', \%manifests, qr/\A([0-9a-f]{32})\.tsv\z/ ],
                  [ 'packets',   \%packets,   qr/\A([0-9a-f]{32})\.srl\z/ ] )
    {
        my ( $subdir, $referenced, $pattern ) = @$dir;
        my $path = File::Spec->catdir( $root, $subdir );
        opendir my $dh, $path or die "Failed to open '$path': $!";
        for my $file ( readdir $dh ) {
            next if $file eq '.' || $file eq '..';
            next unless my ($hash) = $file =~ $pattern;
            next if $referenced->{$hash};
            my $stale = File::Spec->catfile( $path, $file );
            unlink $stale or die "Failed to remove stale corpus file '$stale': $!";
        }
        closedir $dh or die "Failed to close '$path': $!";
    }
}

1;
