#!/bin/sh

failed=""

for lang in perl go ruby; do
    if ! travis/$lang.sh ; then
        failed="$failed $lang"
    fi
done

if [ -n "$failed" ]; then
    echo "Failed languages: $failed"
    exit 1
fi

exit 0
