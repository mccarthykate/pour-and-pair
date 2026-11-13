# Pour & Pair

**Live site:** https://mccarthykate.github.io/pour-and-pair/ (the first visit takes 10 to 30 seconds while R loads in your browser)

A Shiny app that teaches wine and cheese pairing using products sold at
Trader Joe's. Set a guest count, then pick as many wines and cheeses as you like,
starting with either. For each pick the app ranks its best matches and suggests a
second wine (or cheese): one that rounds out the lineup and one that goes with the
same partners. A summary shows the best of your wines for each cheese, and flags
any cheese none of them suit. Then choose accompaniments with advice for your
pairings. An illustrated mini cheese board floats on the right and fills in as you go.

## Run it

1. Open `WinePairing.Rproj` in RStudio.
2. Install the packages once: `install.packages(c("shiny", "bslib"))`
3. Open `app.R` and click **Run App**, or run `shiny::runApp()` in the console.

## Publish updates

The live site is a Shinylive export, so R runs in the visitor's browser and
nobody needs R installed. After editing the app:

1. Commit and push your code changes as usual.
2. Run `sh publish.sh`. It rebuilds the site and pushes it to the `gh-pages`
   branch, which GitHub Pages serves. Changes appear within a couple of minutes.

You need the `shinylive` R package (`install.packages("shinylive")`) and the
GitHub CLI signed in (`gh auth login`).

## Files

- `app.R`: UI and server
- `R/data.R`: wine and cheese types, products, prices, and pairings
- `R/data_addons.R`: crackers, fruit, nuts, spreads, meats, pickles, chocolate, plus drawing specs and roles
- `R/companions.R`: second-wine and second-cheese suggestions, and pairing lookups
- `R/a_catalog.R`: reads the product lists in `data/`
- `R/bites.R`: the three perfect bites for each cheese and wine
- `R/guidance.R`: why-it-works logic, product facts, bread advice, guest planner
- `R/icons.R`: the illustrated mini cheese board
- `R/learn.R`: the Learn tab, an in-depth guide with a link to the source for every claim
- `www/styles.css`: styling
- `data/`: the product lists, and the last-checked date
- `scripts/`: the Trader Joe's refresh (`tj_page.js`, `search_terms.json`, `update_catalog.R`)
- `publish.sh`: rebuilds the site and publishes it to GitHub Pages

## About the product data

Products and prices come from traderjoes.com and are refreshed by hand from
time to time (last on 2026-11-13). Trader Joe's blocks automated access, so
there is no scheduled job: a refresh is done in a normal browser session.

To refresh:

1. Open traderjoes.com in a browser and run `scripts/tj_page.js` in the page,
   with the search words from `scripts/search_terms.json`.
2. Save the result as `data/tj_snapshot.json`.
3. Run `Rscript scripts/update_catalog.R`. It refreshes prices, hides items the
   site no longer lists, adds new wines, cheeses and board items that fit the
   app, and writes a summary to `data/changes.md`. It changes nothing if the
   snapshot looks incomplete. New items show a "new" badge for 30 days.
4. Review the changes, commit, and run `sh publish.sh`.

The product lists are `data/wines.csv`, `data/cheeses.csv` and
`data/addons.csv`; edit them directly to add or remove a product. Stock and
prices vary by store and season, and some products are not on the website.
The website listed no Port, dessert wine or Ripasso, so those styles are not
included. Belhara Estate Malbec is included as "ask in store" because a 2024
review lists it as a Trader Joe's exclusive, but it was not on the website.

## Sources

- Gary Farrell Winery, Pinot Noir and Cheese: http://www.garyfarrellwinery.com/pinot-noir-and-cheese-the-complete-pairing-guide/
- Culture Cheese Magazine: https://culturecheesemag.com/cheese-pairings/how-to-pair-wine-and-cheese/
- Tasting Table, Trader Joe's pairings: https://tastingtable.com/1134375/best-trader-joes-wine-and-cheese-pairings
- Robert Parker Wine Advocate, Trader Joe's pairings: https://content.robertparker.com/perfect-wine-and-cheese-pairings-from-trader-joe-s

Pairings without a named source come from general pairing principles.
