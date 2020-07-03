#!/bin/sh

failed=""

for lang in perl go ruby all; do
    echo "Testing $lang"
    if ! travis/$lang.sh ; then
        echo "Failed $lang"
        failed="$failed $lang"
    fi
done

if [ -n "$failed" ]; then
    echo "Failed languages: $failed"
    exit 1
fi

exit 0
