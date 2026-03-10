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

OUTPUT_FILE=$(mktemp)

opam exec -- opam-publish \
  --tag="$TAG" \
  --msg-file=./shortlog.txt \
  --no-confirmation \
  --no-browser | tee "$OUTPUT_FILE"

PR_LINK=$(tail -n 5 "$OUTPUT_FILE" | grep -oE "https://github.com/ocaml/opam-repository/pull/[0-9]+")

if [ -z "$PR_LINK" ]; then
  echo "Error: could not find the PR link in the output."
else
  echo "Succesfully captured PR: $PR_LINK"
  if [[ -n "${SLACK_WEBHOOK_URL}" ]]; then
    echo "Sending notification to Slack ..."

    echo "🚀 *smtml* version *$TAG* has been submitted to opam-repository!\n*PR:* $PR_LINK" | sigh notify "$SLACK_WEBHOOK_URL"
    echo "Slack notification sent."
  else
    echo "Skipping Slack notification: SLACK_WEBHOOK_URL is not defined."
  fi
fi

rm "$OUTPUT_FILE"
