$ErrorActionPreference = 'Stop'

function Run-Command {
    param([string]$Command, [string[]]$Arguments)
    & $Command @Arguments
    if ($LASTEXITCODE -ne 0) {
        throw "$Command $($Arguments -join ' ') failed with exit code $LASTEXITCODE"
    }
}

function In-Directory {
    param([string]$Path, [scriptblock]$Body)
    Push-Location $Path
    try {
        & $Body
    }
    finally {
        Pop-Location
    }
}

Run-Command 'perl' @('-V')
Run-Command 'cpanm' @('--notest', 'Test::Deep', 'Test::Differences',
    'Test::Exception', 'Test::LongString', 'Test::Warn')

$make = (& perl -MConfig -e 'print $Config{make}').Trim()
if ($LASTEXITCODE -ne 0 -or !$make) {
    throw 'Could not determine the make command for this Perl'
}

In-Directory 'Perl/Decoder' {
    Run-Command 'perl' @('Makefile.PL')
    Run-Command $make @()
}
In-Directory 'Perl/Encoder' {
    Run-Command 'perl' @('Makefile.PL')
    Run-Command $make @()
}

$blib = @('Perl/Decoder/blib/lib', 'Perl/Decoder/blib/arch',
    'Perl/Encoder/blib/lib', 'Perl/Encoder/blib/arch') |
    ForEach-Object { (Resolve-Path $_).Path }
if ($env:PERL5LIB) {
    $blib += $env:PERL5LIB
}
$env:PERL5LIB = $blib -join [IO.Path]::PathSeparator

In-Directory 'Perl/Decoder' { Run-Command $make @('test') }
In-Directory 'Perl/Encoder' { Run-Command $make @('test') }
In-Directory 'Perl/Sereal' {
    Run-Command 'perl' @('Makefile.PL')
    Run-Command $make @('test')
}
