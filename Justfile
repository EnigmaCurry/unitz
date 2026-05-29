set shell := ["bash", "-eu", "-o", "pipefail", "-c"]

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
    nix develop --command bash -c "bb test && clojure -M:test"

# Build the static ClojureScript web app (output: web/public/)
web-build:
    cd web && npm ci && npx shadow-cljs release app

# Run the web app dev server with hot reload (http://localhost:8080)
web-dev:
    cd web && npm ci && npx shadow-cljs watch app

# Run a conversion (e.g., just calc 5 miles to km)
calc *ARGS:
    @nix develop --command bb calc {{ARGS}}

