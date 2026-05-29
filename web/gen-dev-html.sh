#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/public"
GIT_SHA=$(git -C "$(dirname "$0")" rev-parse --short HEAD 2>/dev/null || echo "dev")
sed "s/main\.__HASH__\.js/main.js/g; s/__HASH__/dev/g; s/__GIT_SHA__/${GIT_SHA}/g" index.html.template > index.html
sed 's/main\.__HASH__\.js/main.js/g; s/__HASH__/dev/g' sw.js.template > sw.js
