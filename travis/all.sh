#!/bin/sh

for lang in perl go ruby; do
    travis/$lang.sh || failed=1
done

[ -z "$failed" ] || exit 1
