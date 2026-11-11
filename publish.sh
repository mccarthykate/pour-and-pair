#!/bin/sh
# Rebuild the site with Shinylive and publish it to GitHub Pages (the gh-pages branch).
# Run from anywhere: sh publish.sh
set -e
cd "$(dirname "$0")"
WORK="$(mktemp -d)"
# copy only what the app needs, so working files never end up in the site
mkdir -p "$WORK/app/data"
cp -R app.R R www "$WORK/app/"
cp data/*.csv "$WORK/app/data/"
[ -f data/last_checked.txt ] && cp data/last_checked.txt "$WORK/app/data/"
Rscript -e "shinylive::export('$WORK/app', '$WORK/site')"
touch "$WORK/site/.nojekyll"   # serve files as-is, without Jekyll processing
REPO_URL="${REPO_URL:-$(git remote get-url origin)}"
cd "$WORK/site"
git init -q -b gh-pages
git add -A
git commit -q -m "Publish site"
git push -q -f "$REPO_URL" gh-pages
echo "Published. It can take a minute or two to update at https://mccarthykate.github.io/pour-and-pair/"
