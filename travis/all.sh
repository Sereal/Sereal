#!/bin/sh

for lang in ruby perl go; do
    travis/$lang.sh || failed=1
done

[ -z "$failed" ] || exit 1
