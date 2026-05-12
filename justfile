default:
    just --list

# Run a test example
test-example name:
    cabal test golden --test-options='-p {{ name }}'

# Run a test example with the --accept flag
test-example-accept name:
    cabal test golden --test-options='-p {{ name }} --accept'
