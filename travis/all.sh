#!/bin/sh

for lang in perl go; do
    travis/$lang.sh || failed=1
done

[ -z "$failed" ] || exit 1
