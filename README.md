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
- `R/guidance.R`: why-it-works logic, product facts, bread/jam/meat advice, guest planner
- `R/icons.R`: the illustrated mini cheese board
- `R/learn.R`: the Learn tab, an in-depth guide with a link to the source for every claim
- `www/styles.css`: styling
- `publish.sh`: rebuilds the site and publishes it to GitHub Pages

## About the product data

Products and prices were read from traderjoes.com on 2026-10-29. Stock and
prices vary by store and season, and some products are not on the website.
The website listed no Port, dessert wine or Ripasso, so those styles are not
included. The only Malbec on the website was a Cabernet/Sangiovese/Malbec blend;
Belhara Estate Malbec is included as "ask in store" because a 2024 review lists
it as a Trader Joe's exclusive, but it was not on the website. To add or remove a product, edit the lists at the top of
`R/data.R` and `R/data_addons.R`.

## Sources

- Gary Farrell Winery, Pinot Noir and Cheese: http://www.garyfarrellwinery.com/pinot-noir-and-cheese-the-complete-pairing-guide/
- Culture Cheese Magazine: https://culturecheesemag.com/cheese-pairings/how-to-pair-wine-and-cheese/
- Tasting Table, Trader Joe's pairings: https://tastingtable.com/1134375/best-trader-joes-wine-and-cheese-pairings
- Robert Parker Wine Advocate, Trader Joe's pairings: https://content.robertparker.com/perfect-wine-and-cheese-pairings-from-trader-joe-s

Pairings without a named source come from general pairing principles.
