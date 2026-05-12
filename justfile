default:
    just --list

# Run tests filtered by tasty pattern
test pattern:
    cabal test --test-options='-p "{{ pattern }}"'

# Run tests accepting new golden output
test-accept pattern:
    cabal test --test-options='-p "{{ pattern }}" --accept'
