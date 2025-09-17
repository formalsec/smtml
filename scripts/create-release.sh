#!/bin/bash

TAG=$1

if [ -e "$TAG" ]; then
  echo "usage: ./$0 <TAG>"
  exit 1
fi

git-cliff -c cliff.toml -t "$TAG" -o CHANGES.md
