#!/usr/bin/env bash
set -euo pipefail

TAG="$(git describe --tags --exact-match 2>/dev/null || true)"

if [ -z "$TAG" ]; then
  echo "No tag on the current commit. Nothing to release."
  exit 0
fi

echo "Found tag $TAG on HEAD. Releasing…"

# Verify that the GitHub token exists
if [[ -z "${OPAM_PUBLISH_GH_TOKEN:-}" ]]; then
  echo "Error: OPAM_PUBLISH_GH_TOKEN is not set."
  exit 1
fi

# Create PR change log
PREV_TAG=$(git describe --tags --abbrev=0 HEAD^)
cat <<EOF > shortlog.txt
#### Git shortlog

\`\`\`
$(git shortlog $PREV_TAG..$TAG)
\`\`\`
EOF

# Remove pin depends which are not allowed in opam-repository
sed -i '/^pin-depends:/,$d' smtml.opam

opam exec -- opam-publish \
  --tag="$TAG" \
  --msg-file=./shortlog.txt \
  --no-confirmation \
  --no-browser
