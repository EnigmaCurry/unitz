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

