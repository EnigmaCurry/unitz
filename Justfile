set shell := ["bash", "-eu", "-o", "pipefail", "-c"]

# Run a command via nix develop if available, otherwise directly
_nix *CMD:
    @if command -v nix &>/dev/null; then nix develop --command bash -c "{{CMD}}"; else bash -c "{{CMD}}"; fi

# Show available commands
help:
    just --list

# Enter `nix develop` sub-shell
dev:
    nix develop

# Fast tests under Babashka
_test-bb:
    bb test

# JVM Clojure tests
_test-clj:
    clojure -M:test

# Run tests in both Babashka and Clojure JVM
test:
    just _nix "bb test && clojure -M:test"

# Build the static ClojureScript web app (output: web/public/)
web-build:
    just _nix "cd web && npm ci && npx shadow-cljs release app && bash cache-bust.sh"

# Run the web app dev server with hot reload (http://localhost:8080)
web-dev:
    just _nix "cd web && npm ci && GIT_SHA=$(git rev-parse --short HEAD 2>/dev/null || echo dev) && sed \"s/main\.__HASH__\.js/main.js/g; s/__HASH__/dev/g; s/__GIT_SHA__/\$GIT_SHA/g\" public/index.html.template > public/index.html && sed 's/main\.__HASH__\.js/main.js/g; s/__HASH__/dev/g' public/sw.js.template > public/sw.js && npx shadow-cljs watch app"

# Remove build artifacts
clean:
    rm -rf web/node_modules web/.shadow-cljs web/public/js web/public/index.html web/public/sw.js web/public/calc.html

# Run a conversion (e.g., just calc 5 miles to km)
calc *ARGS:
    #!/usr/bin/env bash
    set -euo pipefail
    ARGS='{{ARGS}}'
    if command -v nix &>/dev/null; then
        nix develop --command bb calc $ARGS
    else
        bb calc $ARGS
    fi
