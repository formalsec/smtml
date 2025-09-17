#!/usr/bin/env bash
set -euo pipefail

TAG="$1"
BRANCH="release/v$TAG"

if ! command -v git-cliff >/dev/null; then
  echo "git-cliff is not installed. Run: cargo install git-cliff"
  exit 1
fi

# Fail fast if opam-publish isn't installed
if ! command -v gh >/dev/null; then
  echo "github-clie is not installed."
  exit 1
fi

git switch -c $BRANCH
git-cliff -c cliff.toml -t $TAG -o CHANGES.md
git add CHANGES.md
git commit -m "Release $TAG"
git push -u origin $BRANCH

gh pr create \
  --base "main" \
  --head "$BRANCH" \
  --title "Release $TAG" \
  --body "Automated Release $TAG"
