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

# GitHub's Windows images can put Git's Unix utilities on PATH, masking
# dependencies absent from ordinary Strawberry Perl installations.
if ($env:SEREAL_CI_REJECT_UNIX_RM -eq '1') {
    $guardDir = Join-Path ([IO.Path]::GetTempPath()) ('sereal-no-rm-' + [guid]::NewGuid())
    New-Item -ItemType Directory -Path $guardDir | Out-Null
    $guardSource = Join-Path $guardDir 'rm.c'
    $guardExe = Join-Path $guardDir 'rm.exe'
    @'
#include <stdio.h>
int main(void) {
    fputs("Sereal must not require a Unix rm executable\n", stderr);
    return 97;
}
'@ | Set-Content -Encoding ascii $guardSource
    Run-Command 'gcc' @($guardSource, '-o', $guardExe)
    $env:PATH = $guardDir + [IO.Path]::PathSeparator + $env:PATH
    # Get-Command can return several applications with the same name.
    $firstRm = Get-Command rm.exe -CommandType Application | Select-Object -First 1
    if ($firstRm.Source -ne $guardExe) {
        throw 'The Unix rm guard is not first on PATH'
    }
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
