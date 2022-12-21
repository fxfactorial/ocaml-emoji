#!/bin/sh
set -e
REMOTE="git@github.com:fxfactorial/ocaml-emoji.git"

SRC="$(pwd)"
TMP="$(mktemp -d)"

cp docs/style.css api.docdir/

echo "Source root is $SRC"
echo "Using tmp dir $TMP"
cd "$TMP"

git init
git checkout --orphan gh-pages
git remote add origin "$REMOTE"
git pull origin gh-pages || true

cp -R "$SRC/api.docdir/"* "$TMP/"

(git add . && \
git commit -m "Updated $(date)!" && \
git push origin gh-pages) || true

rm -rf "$TMP"
