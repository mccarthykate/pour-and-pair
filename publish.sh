#!/bin/sh
# Rebuild the site with Shinylive and publish it to GitHub Pages (the gh-pages branch).
# Run from anywhere: sh publish.sh
set -e
cd "$(dirname "$0")"
SITE="$(mktemp -d)/site"
Rscript -e "shinylive::export('.', '$SITE')"
touch "$SITE/.nojekyll"   # serve files as-is, without Jekyll processing
REPO_URL="$(git remote get-url origin)"
cd "$SITE"
git init -q -b gh-pages
git add -A
git commit -q -m "Publish site"
git push -q -f "$REPO_URL" gh-pages
echo "Published. It can take a minute or two to update at https://mccarthykate.github.io/pour-and-pair/"
