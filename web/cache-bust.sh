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

# Get git SHA for build metadata
GIT_SHA=$(git -C "$(dirname "$0")" rev-parse --short HEAD 2>/dev/null || echo "unknown")

# Generate index.html and sw.js from templates with the hash stamped in
sed "s/__HASH__/${HASH}/g; s/__GIT_SHA__/${GIT_SHA}/g" index.html.template > index.html
sed "s/__HASH__/${HASH}/g" sw.js.template > sw.js

# Generate calc.html: a single self-contained file with JS inlined
awk -v js_file="js/main.${HASH}.js" -v hash="${HASH}" -v git_sha="${GIT_SHA}" '
  # Replace placeholders
  { gsub(/__HASH__/, hash); gsub(/__GIT_SHA__/, git_sha) }
  # Inline the JS bundle
  /<script src="js\/main\./ {
    print "<script>"
    while ((getline line < js_file) > 0) print line
    close(js_file)
    print "</script>"
    next
  }
  # Inject snapshot meta tag (detected by app to show snapshot intro)
  /<meta charset/ { print; print "  <meta name=\"calc-snapshot\" content=\"" git_sha "\">"; next }
  # Strip PWA-related tags not useful for standalone file
  /rel="manifest"/ { next }
  /apple-touch-icon/ { next }
  /apple-mobile-web-app/ { next }
  /name="theme-color"/ { next }
  # Replace SW registration block with unregistration (cleans up stale workers)
  /serviceWorker\.register/ {
    print "      navigator.serviceWorker.getRegistrations().then(function(regs) {"
    print "        regs.forEach(function(r) { r.unregister(); });"
    print "      });"
    next
  }
  { print }
' index.html.template > calc.html
echo "Cache-busted with hash: ${HASH}"
echo "Generated calc.html ($(du -h calc.html | cut -f1) standalone)"
