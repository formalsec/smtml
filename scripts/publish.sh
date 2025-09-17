#!/usr/bin/env bash
set -euo pipefail

# Fail fast if opam-publish isn't installed
if ! command -v opam-publish >/dev/null; then
  echo "opam-publish is not installed. Run: opam install opam-publish"
  exit 1
fi

TAG="$(git describe --tags --exact-match 2>/dev/null || true)"

if [ -z "$TAG" ]; then
  echo "No tag on the current commit. Nothing to release."
  exit 0
fi

echo "Found tag $TAG on HEAD. Releasing…"

# Remove pin depends which are not allowed in opam-repository
sed -i '/^pin-depends:/,$d' smtml.opam

opam publish \
  --tag="$TAG" \
  --no-confirmation \
  --no-browser
