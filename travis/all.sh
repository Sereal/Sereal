#!/bin/sh

for lang in go perl; do
    travis/$lang.sh || failed=1
done

[ -z "$failed" ] || exit 1
