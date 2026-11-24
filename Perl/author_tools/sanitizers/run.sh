#!/usr/bin/env bash
# Build once and report address and leak checks independently.
set -euo pipefail
if [[ $# != 1 ]]; then
    echo 'Usage: PERL=/path/to/perl run.sh new-work-directory' >&2
    exit 1
fi
here=$(cd -- "$(dirname -- "$0")" && pwd)
"$here/build.sh" "$1"
status=0
"$here/test.sh" "$1" asan || status=1
"$here/test.sh" "$1" lsan || status=1
exit "$status"
