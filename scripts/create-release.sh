#!/usr/bin/env bash
set -euo pipefail

if ! command -v git-cliff >/dev/null; then
  echo "git-cliff is not installed. Run: cargo install git-cliff"
  exit 1
fi

# Fail fast if opam-publish isn't installed
if ! command -v gh >/dev/null; then
  echo "github-cli is not installed."
  exit 1
fi

if [ $# -ge 1 ] && [ -n "$1" ]; then
  TAG="$1"
else
  git fetch --tags

  LAST_TAG=$(git describe --tags --abbrev=0 || echo "v0.0.0")

  IFS='.' read -r MAJOR MINOR PATCH <<< "${LAST_TAG#v}"

  MINOR=$((MINOR + 1))

  TAG="$MAJOR.$MINOR.$PATCH"
fi

BRANCH="release/v$TAG"
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
