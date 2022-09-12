#!/usr/bin/env bash

set -euo pipefail

for dict in /dicts/*.txt; do
    echo "Adding $dict"
    cat "$dict" >> org/languagetool/resource/en/hunspell/spelling.txt
done

ARGS="$*"

if [ -z "${ARGS}" ]; then
    echo "Missing schema file"
    exit 1
fi

## LT is Web server, start it in the background.
echo "Starting LanguageTool server..."
bash /LanguageTool/start.sh >/dev/null &

# health-check the server
attempts=0
limit=10
while ! curl --fail --data "language=en-US&text=a simple test" http://localhost:8010/v2/check >/dev/null 2>&1; do
    attempts=$(( attempts + 1 ))
    if [ $attempts -gt $limit ]; then
        echo "Failed to start LanguageTool in $limit seconds"
        exit 1
    fi
    sleep 1
done

echo "Checking $ARGS..."

emqx_schema_validate "$ARGS"
