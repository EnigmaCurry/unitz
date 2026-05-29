#!/usr/bin/env bash
set -euo pipefail

# Post-build cache-busting: hash main.js and stamp index.html + sw.js
cd "$(dirname "$0")/public"

HASH=$(sha256sum js/main.js | cut -c1-10)

# Rename the JS bundle (remove previous hashed bundles first)
for f in js/main.*.js; do
  [ -f "$f" ] && rm -f "$f"
done
mv js/main.js "js/main.${HASH}.js"

# Generate index.html and sw.js from templates with the hash stamped in
sed "s/__HASH__/${HASH}/g" index.html.template > index.html
sed "s/__HASH__/${HASH}/g" sw.js.template > sw.js

# Generate calc.html: a single self-contained file with JS inlined
awk -v js_file="js/main.${HASH}.js" -v hash="${HASH}" '
  # Replace __HASH__ placeholders (for any non-script uses)
  { gsub(/__HASH__/, hash) }
  # Inline the JS bundle
  /<script src="js\/main\./ {
    print "<script>"
    while ((getline line < js_file) > 0) print line
    close(js_file)
    print "</script>"
    next
  }
  # Skip service worker registration block (not useful for standalone file)
  /serviceWorker/ { next }
  { print }
' index.html.template > calc.html
echo "Cache-busted with hash: ${HASH}"
echo "Generated calc.html ($(du -h calc.html | cut -f1) standalone)"
