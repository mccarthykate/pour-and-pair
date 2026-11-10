# Pairing data for the Pour & Pair app.
# Products and prices were read from traderjoes.com on 2026-10-29.
# Trader Joe's stock varies by store and season, and not every product is on
# the website, so check the shelf. Edit this file to add or remove products.

RULES <- data.frame(
  title = c("Acidity cuts richness", "Fat softens tannin", "Salt loves sugar",
            "Match the weight", "Young cheese, gentle wine"),
  text = c(
    "Crisp wines and bubbles refresh your mouth after creamy cheese.",
    "Fat and protein smooth the dry grip of tannin in red wine.",
    "Salty cheese makes fruity or sweet wine taste even better.",
    "Delicate with delicate. Bold wine needs a cheese that holds up.",
    "Fresh cheese makes tannic reds taste chalky. Go light and bright."),
  stringsAsFactors = FALSE
)

# ---- Wine styles --------------------------------------------------------
WINE_STYLES <- data.frame(
  id = c("cremant", "prosecco", "sauv", "lightwhite", "chard", "aromatic", "rose", "pinot", "malbec", "cab", "redblend"),
  name = c("Brut sparkling & Crémant", "Prosecco", "Sauvignon Blanc", "Pinot Grigio & light whites",
           "Chardonnay", "Dry Riesling & aromatic whites", "Rosé (still & sparkling)", "Pinot Noir",
           "Malbec", "Cabernet, Zinfandel & Syrah", "Red blends & Italian reds"),
  color = c("#e8d27a", "#efe3a0", "#d9dd8a", "#ece8b0", "#e3c75c", "#e6e08e", "#e89aa6", "#8a2b43", "#4b1838", "#4a1530", "#6e1d33"),
  about = c(
    "Dry, crisp bubbles, often with a toasty brioche note. Made like Champagne.",
    "Light, fruity and lightly sweet with green apple and pear. Easygoing and refreshing.",
    "Zesty and grassy with citrus and high acidity. Marlborough bottles are the classic style.",
    "Neutral, crisp and easy: Pinot Grigio, Vinho Verde and Vermentino. Best with mild cheese.",
    "Medium to full body. Oaked versions are creamy with butter and baked apple; Monterey styles lean fruit and citrus.",
    "Bright acidity with peach and citrus. Viognier is rounder and more floral.",
    "Dry and fresh with strawberry and citrus. Light enough for delicate cheeses.",
    "Lower tannin, higher acidity, red cherry with earthy, mushroom notes. The most cheese-friendly red.",
    "Argentina's signature red: full-bodied, with dark fruit such as black cherry and plum and hints of cocoa. Murray's Cheese describes it as similar in boldness to Cabernet but with a gentler structure.",
    "Full-bodied reds with dark fruit and firm tannin. They need rich, fatty or aged cheese.",
    "Medium to full reds from Italy, Paso Robles and Bordeaux. Savory, with moderate tannin."),
  stringsAsFactors = FALSE
)

# name|price|note
.wine <- list(
  cremant = c(
    "Opaline Brut Sparkling Wine|9.99|", "Reserve North Coast Brut Sparkling Wine|9.99|",
    "La Burgondie Crémant de Bourgogne|11.99|Tasting Table suggests it with triple crème brie.",
    "Platinum Reserve Brut Sparkling Wine Sonoma County|14.99|",
    "Platinum Reserve Blanc de Blancs Napa Valley Sparkling Wine|14.99|",
    "L'Eclat Blanc de Blancs|15.99|",
    "Henri Laffard Grand Reserve Brut Champagne|26.99|True Champagne from France."),
  prosecco = c(
    "Prosecco|8.99|", "Villa Antica Asolo Prosecco DOCG Superiore|8.99|",
    "Cecilia Beretta Prosecco|10.99|", "Incanto Prosecco|14.99|"),
  sauv = c(
    "Tazmin Breeze New Zealand Sauvignon Blanc|4.99|", "Vignobles Lacheteau Touraine Sauvignon Blanc|6.99|",
    "Picton Bay Sauvignon Blanc|7.99|", "Tuatea Marlborough New Zealand Sauvignon Blanc|7.99|",
    "Le Beau Sud Vermentino-Sauvignon Blanc|8.99|", "Charles Shaw Sauvignon Blanc|3.49|"),
  lightwhite = c(
    "Charles Shaw Pinot Grigio|3.49|", "Organic Charles Shaw Pinot Grigio|3.99|",
    "Espiral Vinho Verde|5.49|", "Villa Borghetti Otto Pino Grigio|7.99|",
    "Casone Toscana Vermentino 2023|7.99|", "Sauvigio Sauvignon Blanc & Pinot Grigio|6.99|"),
  chard = c(
    "Charles Shaw Chardonnay|3.49|", "Maison Barboulot Chardonnay-Viognier 2024|5.99|",
    "Big Churn Chardonnay California|6.99|", "Rebuttel Chardonnay|6.99|", "Copilot Chardonnay 2023|7.99|",
    "Compass Bridge Sonoma County Chardonnay 2023|8.99|", "VINTJS Sonoma County Chardonnay 2021|8.99|",
    "Reserve Chardonnay Yountville Napa Valley 2023|9.99|",
    "Reserve Chardonnay Arroyo Seco Monterey County 2023 Lot #262|9.99|Monterey style: fruit and citrus.",
    "Grand Reserve Chardonnay Carneros 2023|12.99|"),
  aromatic = c(
    "Emma Reichart Dry Riesling|4.99|Dry, not sweet.", "Honey Moon Viognier|5.99|"),
  rose = c(
    "Emma Reichart Rosé Pinot Noir|4.99|", "Cinsault Rosé Maison Barboulot|5.99|",
    "La Ferme Julien Rosé|6.99|", "Giardino Pinot Grigio Rosé Vivace|6.99|",
    "French Fizz Le Rosé 2021|7.99|", "Opaline Pinot Noir Brut Rosé|9.99|",
    "Reserve Brut Rosé North Coast|9.99|", "Cecilia Beretta Prosecco Rosé|10.99|",
    "La Burgondie Cremant De Bourgogne Rose Brut|13.99|", "L'Eclat Brut Rosé|14.99|"),
  pinot = c(
    "Loudenotte Pinot Noir 2022|6.99|", "Moon X Pinot Noir|6.99|", "Copilot Pinot Noir 2023|7.99|",
    "Caretaker Pinot Noir 2022|9.99|", "Reserve Pinot Noir Santa Barbara County 2022|9.99|",
    "Compass Bridge Sonoma County Pinot Noir|9.99|", "Grand Reserve Pinot Noir Carneros Lot 23|12.99|",
    "Grand Reserve Pinot Noir Santa Lucia Highlands 2021 Lot #118|12.99|",
    "Sunshine Mountain Vineyard Pinot Noir 2023|14.99|", "Platinum Reserve Pinot Noir Carneros 2022|14.99|"),
  malbec = c(
    "Super Happy Cabernet Sauvignon Sangiovese & Malbec|9.99|A blend: Malbec is one of three grapes, so expect it a little firmer than a straight Malbec.",
    "Belhara Estate Malbec|8.99|Not listed on traderjoes.com right now. Reverse Wine Snob reviewed the 2021 vintage (single vineyard, Uco Valley, Mendoza) as a Trader Joe's exclusive in August 2024. Ask in store; price is from that review."),
  cab = c(
    "Charles Shaw Cabernet Sauvignon|3.49|", "Charles Shaw Shiraz|3.49|",
    "Motif Cabernet Sauvignon Red Hills Lake County|5.49|", "Moon X Cabernet Sauvignon|6.99|",
    "Corvelia Cabernet Sauvignon|9.99|", "Reserve Zinfandel Paso Robles Adelaida District 2023 Lot #265|9.99|",
    "Compass Bridge Sonoma County Cabernet Sauvignon 2023|9.99|",
    "Reserve Cabernet Sauvignon Wahluke Slope 2022|9.99|",
    "Grand Reserve Zinfandel Dry Creek Valley Sonoma 2021|12.99|",
    "Grand Reserve Syrah Sonoma County 2022|12.99|",
    "Grand Reserve Cabernet Sauvignon Alexander Valley 2020|12.99|",
    "Platinum Reserve Cabernet Sauvignon Napa Valley 2020|14.99|"),
  redblend = c(
    "Charles Shaw Red Blend|3.49|", "Maison Barboulot Cabernet Syrah|5.99|",
    "Villa Cerrina Montepulciano D'Abruzzo|6.99|", "Vin De Bordeaux B by Fonbadet Bordeaux 2019|7.99|",
    "Petit Reserve Cabernet Franc Paso Robles 2021|7.99|", "Casone Toscana Governo All'uso Toscano 2023|8.99|",
    "Langhe Nebbiolo Rosa dell Olmo 2021|8.99|", "Reserve Meritage Paso Robles 2023 Red Wine|9.99|",
    "Susumaniello Ruggero di Bardo|9.99|",
    "Grand Reserve Meritage Paso Robles 2023|12.99|")
)

# ---- Cheese styles ------------------------------------------------------
CHEESE_STYLES <- data.frame(
  id = c("brie", "goat", "fresh", "feta", "sheep", "alpine", "toscano", "cheddar", "gouda", "parm", "blue", "stilton"),
  name = c("Brie & soft-ripened", "Goat cheese (chèvre)", "Fresh mozzarella & burrata", "Feta",
           "Sheep's milk (Manchego & more)", "Gruyère & Alpine-style", "Toscano & Italian semi-firm",
           "Cheddar & English hard cheeses", "Gouda & caramel cheeses", "Parmigiano & hard Italian",
           "Blue-veined", "White Stilton with fruit"),
  color = c("#f2e6c2", "#f6f1e7", "#fbf8f0", "#efe9d9", "#e4c56e", "#dcae4c", "#e8cf8a", "#e19a2a", "#c97f26", "#e9c875", "#a9b3b0", "#f0d9d6"),
  about = c(
    "Very soft, buttery and mild with a bloomy rind. Triple crème is the richest.",
    "Fresh, tangy and lemony, from plain logs to honey and herb versions.",
    "Mild, milky and soft. Very delicate, so keep the wine light.",
    "Briny, salty and tangy with a crumbly texture.",
    "Firm, nutty and gently sweet. Manchego is the Spanish classic.",
    "Nutty, slightly sweet and savory, with a dense, smooth texture.",
    "Mild, nutty and semi-firm. Rinds are rubbed or soaked in pepper, wine or herbs.",
    "Sharp, salty and crumbly. Flavored versions add truffle, whiskey, onion or fruit.",
    "Caramel and butterscotch notes, firm with crunchy crystals when aged.",
    "Hard, salty and nutty with deep umami and a granular bite.",
    "Pungent, salty and creamy with sharp mineral veins. The boldest cheese on the board.",
    "Crumbly, mild and tangy, studded with dried fruit."),
  stringsAsFactors = FALSE
)

# name|size|price|note
.cheese <- list(
  brie = c(
    "Double Cream Brie|8Oz|4.49|", "Double Cream Brie|16Oz|9.99|", "Triple Creme Brie with Calvados Apple Brandy|1Lb|12.99|",
    "Saint André Triple Crème Brie|1Lb|14.99|Very rich. Bubbles are the classic partner.",
    "Double Crème Brie with Truffles|8.5Oz|10.99|", "Dorothy's Comeback Cow Soft Ripened Cheese|7Oz|6.99|"),
  goat = c(
    "Chevre Goat Cheese|5Oz|2.99|", "Chevre with Honey|5Oz|3.29|", "Chevre with Fine Herbs|5Oz|3.29|",
    "Silver Goat® Chevre Goat Cheese Log|8.5Oz|4.79|", "Cranberry Chèvre Goat Cheese|8Oz|5.99|",
    "Hot Honey Goat Milk Cheese|4Oz|2.99|", "Caramelized Onion Goat's Milk Cheese|4Oz|3.29|",
    "Wild Blueberry Vanilla Chevre|8Oz|5.99|", 
    "The \"OG\" (Old Goat) Aged Goat Cheese|1Lb|12.99|Aged and firm, bolder than fresh chèvre."),
  fresh = c(
    "All Natural Fresh Mozzarella Cheese|8Oz|3.99|", "Burrata|8Oz|4.99|",
    "Truffle Burrata|8Oz|5.99|", "Marinated Fresh Mozzarella Cheese|12Oz|5.99|"),
  feta = c("Feta Cheese|8Oz|3.79|", "Israeli Feta|17.6Oz|8.99|"),
  sheep = c(
    "Manchego Cheese|1Lb|14.99|", "Spanish Sheep Milk Cheese with Quince Jam|6Oz|5.99|",
    "Aged Sheep's Milk Cheese Marinated in EVOO|6Oz|5.99|", "The Sheepish Tomato|6Oz|5.99|",
    "The Sheepish Pesto|5.3Oz|6.49|"),
  alpine = c(
    "Raw Milk Cave Aged Le Gruyère|1Lb|19.99|Nutty and sweet. A dependable Pinot Noir cheese.",
    "Cheddar & Gruyere Mèlange Cheese|1Lb|7.99|", 
    "Jasper Hill Vault No 5 Cave Aged Cheese|1Lb|11.99|", "Fontina|1Lb|6.99|",
    "Pink Peppercorn Fontina Cheese|7Oz|6.99|", 
    "French Heritage Saint Paulin Cheese|5.3Oz|3.99|"),
  toscano = c(
    "Toscano Cheese with Black Pepper|1Lb|11.99|Cracked pepper adds gentle spice. Good with fruity reds.",
    "Toscano Cheese Crunchy Chili Onion|1Lb|11.99|Savory crunch with a little heat.",
    "Toscano Cheese Soaked in Chardonnay Wine|1Lb|10.99|The Chardonnay-soaked rind suits a Chardonnay.",
    "Creamy Toscano Cheese Soaked in Syrah|1Lb|11.99|The Syrah-soaked rind echoes a bold red.",
    "Creamy Toscano Cheese Dusted with Cinnamon|1Lb|11.99|Sweet spice. Best with softer, fruity wines.",
    "Ubriaco al Cabernet|1Lb|11.99|Rind is soaked in Cabernet. Pair with a bold red.",
    "Italian Bigonda Cheese with Herbs|1Lb|11.99|", "Italian Lagorai Semi-Soft Cheese|1Lb|10.49|"),
  cheddar = c(
    "Unexpected Cheddar Cheese|5Oz|3.99|", "New Zealand Sharp Cheddar Cheese|1Lb|7.99|",
    "Orkney White Cheddar Cheese|7Oz|4.99|", "McLelland Vintage Scottish Cheddar|1Each|5.49|",
    "Coastal Cheddar Cheese with Truffle|5.29Oz|4.99|Truffle cheddar is a sommelier pick for Pinot Noir.",
    "Cheddar Cheese with Caramelized Onions|1Lb|11.99|", "Irish Porter Cheddar Cheese|1Lb|12.49|",
    "English Cheddar Cheese with Champagne|1Lb|12.99|", "Figgy Cheddar|1Lb|10.99|", "Sticky Toffee Cheddar|1Lb|11.99|",
    "Smoked Cheddar Cheese with Paprika|7Oz|4.99|", "Rutland Red Leicester Cheese|1Lb|12.99|",
    "Cotswold Double Gloucester Cheese|1Lb|11.99|"),
  gouda = c(
    "Double Cream Gouda Cheese|1Lb|9.99|", "Walnut Gouda Cheese|6Oz|5.49|", "Caramel Washed Gouda Cheese|1Lb|11.99|",
    "Family Nijland Clover Gouda Cheese|1Lb|11.99|", "Jersey Gouda|1Lb|12.99|",
    "1000 Day Gouda Cheese|1Lb|13.99|Aged, with crunchy crystals and deep caramel.",
    "Mimolette Cheese|7.05Oz|7.49|"),
  parm = c(
    "Parmesan Cheese|1Lb|8.99|", "Parmigiano Reggiano Aged Over 55 Months|1Lb|19.99|",
    "Parmigiano Reggiano Stravecchio|1Lb|21.99|", "Truffle Pecorino|1Lb|12.99|", "Canestrato Pepato|1Lb|11.99|"),
  blue = c(
    "Dolce Creamy Gorgonzola Style Cheese|8Oz|4.99|", "Carr Valley Cheese Glacier Wildfire Blue|5Oz|3.99|",
    "Cambozola® Triple Cream Cheese|1Lb|14.99|Mild and creamy. The easiest blue to start with.",
    "Shropshire Blue English Cheese|1Lb|14.99|"),
  stilton = c(
    "White Stilton Cheese with Mango & Ginger|1Lb|12.99|", "White Stilton with Cranberries|1Lb|13.99|",
    "White Stilton Cheese with Apple & Pear|1Lb|13.99|", "Strawberry White Stilton Cheese|6.34Oz|5.49|")
)

.expand <- function(lst, cols) {
  do.call(rbind, lapply(names(lst), function(s) {
    parts <- strsplit(lst[[s]], "|", fixed = TRUE)
    do.call(rbind, lapply(parts, function(p) {
      p <- c(p, rep("", length(cols) - length(p)))
      setNames(data.frame(s, t(p), stringsAsFactors = FALSE), c("style", cols))
    }))
  }))
}
WINE_PRODUCTS <- .expand(.wine, c("name", "price", "note"))
WINE_PRODUCTS$price <- as.numeric(WINE_PRODUCTS$price)
WINE_PRODUCTS$key <- paste0("w", seq_len(nrow(WINE_PRODUCTS)))
CHEESE_PRODUCTS <- .expand(.cheese, c("name", "size", "price", "note"))
CHEESE_PRODUCTS$price <- as.numeric(CHEESE_PRODUCTS$price)
CHEESE_PRODUCTS$key <- paste0("c", seq_len(nrow(CHEESE_PRODUCTS)))
rm(.wine, .cheese)

# ---- Pairings -----------------------------------------------------------
# rating: 3 = classic, 2 = good, 0 = skip it
.p <- function(wine, cheese, rating, reason) {
  data.frame(wine = wine, cheese = cheese, rating = rating, reason = reason, stringsAsFactors = FALSE)
}
PAIRINGS <- rbind(
  .p("cremant", "brie", 3, "Bubbles and acid cut the cream. The wine's toasty brioche notes echo the cheese."),
  .p("cremant", "parm", 2, "Salty, nutty cheese against crisp bubbles is a clean, refreshing contrast."),
  .p("cremant", "sheep", 2, "Nutty sheep's milk cheese suits the toasty character of the wine."),
  .p("cremant", "goat", 2, "Matching high acid with high acid keeps both bright."),
  .p("cremant", "blue", 0, "Pungent blue overwhelms delicate bubbles."),

  .p("prosecco", "fresh", 3, "Light and milky meets light and fruity. Nothing overpowers anything."),
  .p("prosecco", "brie", 2, "A little sweetness and fizz lift a rich, creamy cheese."),
  .p("prosecco", "parm", 2, "Fizz scrubs the palate between bites of salty, granular cheese."),
  .p("prosecco", "feta", 2, "Mild sweetness softens feta's brine."),
  .p("prosecco", "stilton", 2, "Fruit in the cheese meets fruit and fizz in the glass."),
  .p("prosecco", "cheddar", 0, "Sharp aged cheddar makes a light wine taste thin."),

  .p("sauv", "goat", 3, "The classic. Grassy citrus wine against tangy chèvre. Contrast in flavor, match in acidity."),
  .p("sauv", "feta", 3, "Bright acid echoes the tang while the wine freshens the salt."),
  .p("sauv", "brie", 2, "Zippy acid contrasts a soft, creamy cheese."),
  .p("sauv", "fresh", 2, "Both are light and fresh. Add tomato and basil for the full effect."),
  .p("sauv", "gouda", 0, "Sweet caramel cheese makes the wine taste sour."),

  .p("lightwhite", "fresh", 3, "A neutral, crisp wine lets mild cheese shine."),
  .p("lightwhite", "feta", 2, "Crisp and clean against salty, briny cheese."),
  .p("lightwhite", "goat", 2, "Light acid matches fresh chèvre."),
  .p("lightwhite", "toscano", 2, "A mild, nutty cheese needs a mild wine."),
  .p("lightwhite", "blue", 0, "Blue overwhelms a delicate white."),

  .p("chard", "sheep", 3, "Fruit and citrus balance the savory, nutty cheese."),
  .p("chard", "alpine", 3, "Creamy oaked styles mirror the cheese's nutty richness."),
  .p("chard", "toscano", 2, "Rich, nutty and easygoing. The Chardonnay-soaked Toscano is a natural fit."),
  .p("chard", "brie", 2, "Buttery texture on both sides. Pick an unoaked style for more contrast."),
  .p("chard", "cheddar", 2, "Works with a milder cheddar. Very sharp ones will dominate."),
  .p("chard", "blue", 0, "Blue overwhelms the wine and makes it taste flat."),

  .p("aromatic", "stilton", 2, "Bright acid and stone fruit suit a crumbly cheese with dried fruit."),
  .p("aromatic", "feta", 2, "Acid keeps salty, briny cheese lively."),
  .p("aromatic", "gouda", 2, "Peachy fruit lifts the caramel notes in gouda."),
  .p("aromatic", "alpine", 2, "Acid and fruit refresh a nutty, savory cheese."),
  .p("aromatic", "goat", 2, "Tangy cheese, tangy wine."),

  .p("rose", "feta", 3, "Dry, fresh and fruity, a good partner for salty, tangy cheese."),
  .p("rose", "goat", 3, "Strawberry and citrus complement fresh chèvre."),
  .p("rose", "fresh", 2, "Light enough that the cheese isn't drowned out."),
  .p("rose", "sheep", 2, "Red fruit brings out the cheese's gentle sweetness."),
  .p("rose", "stilton", 2, "Berry fruit echoes the fruit in the cheese."),
  .p("rose", "blue", 0, "Delicate rosé disappears next to pungent blue."),

  .p("pinot", "brie", 3, "Soft, earthy rind echoes the wine's mushroom notes. Cream balances acidity."),
  .p("pinot", "alpine", 3, "Nutty and slightly sweet. Complements the fruit without competing. A dependable choice."),
  .p("pinot", "gouda", 3, "Caramel notes suit the darker fruit. Firm texture gives acidity something to work against."),
  .p("pinot", "toscano", 3, "Mild, nutty cheese with a little pepper lets Pinot's cherry fruit show."),
  .p("pinot", "cheddar", 2, "Sharp saltiness brings out fruit in a lighter Pinot. Taste a small piece first with bolder bottles."),
  .p("pinot", "goat", 2, "Earthy wine with tangy, creamy cheese. Works best with a lighter Pinot."),
  .p("pinot", "blue", 0, "Pungent blue overwhelms Pinot's subtlety and makes it taste thin."),

  .p("malbec", "cheddar", 3, "Aged cheddar stands up to Malbec's full body. Murray's Cheese suggests a fruity aged cheddar so the fruit in the cheese and wine echo each other."),
  .p("malbec", "gouda", 3, "Wisconsin Cheese calls gouda a classic Malbec match. Aged gouda's caramel notes suit the wine's dark fruit and cocoa."),
  .p("malbec", "toscano", 2, "Semi-firm Italian-style cheeses (Wisconsin Cheese names Asiago and Provolone) work with Malbec. The black pepper or Syrah-soaked Toscano echoes the wine's spice and fruit."),
  .p("malbec", "parm", 2, "Aged, firm, salty cheese has the concentrated fat that softens a bold red's tannin."),
  .p("malbec", "blue", 2, "Wisconsin Cheese says Malbec's fruit brings out milder blues. Pick the creamy Gorgonzola or Cambozola over a sharp, salty blue."),
  .p("malbec", "sheep", 2, "Sources disagree. Some pair Manchego's nutty caramel with Malbec's fruit; Wisconsin Cheese advises against sheep's milk cheese. Taste a piece first."),
  .p("malbec", "brie", 0, "Wisconsin Cheese advises against creamy cheeses with Malbec. The dark fruit and structure overwhelm them."),
  .p("malbec", "goat", 0, "Wisconsin Cheese advises against goat cheese with Malbec; the dark fruit clashes with the tang."),
  .p("malbec", "fresh", 0, "Too delicate for a full-bodied red; tannin makes fresh cheese taste chalky."),

  .p("cab", "cheddar", 3, "Tannin grips the milkfat in a dense, aged cheddar. A textbook match."),
  .p("cab", "gouda", 3, "Aged gouda's caramel and crunch hold up to bold dark fruit."),
  .p("cab", "parm", 2, "Salty, umami-rich cheese tames the tannin."),
  .p("cab", "sheep", 2, "Firm, nutty and rich enough to stand next to a big wine."),
  .p("cab", "toscano", 2, "The wine-soaked rinds (Syrah, Cabernet) echo the red in your glass."),
  .p("cab", "blue", 2, "Only with care: choose a ripe, jammy Zinfandel and a mild, creamy blue such as Cambozola. Salt exaggerates tannin, so a firm Cabernet with a sharp blue can taste harsh."),
  .p("cab", "fresh", 0, "Fresh cheese makes tannin taste chalky."),
  .p("cab", "goat", 0, "Young, tangy cheese clashes with heavy tannin."),

  .p("redblend", "parm", 3, "Savory red with deep umami cheese. Hard to beat."),
  .p("redblend", "toscano", 3, "Italian reds and Italian semi-firm cheese are a natural fit."),
  .p("redblend", "gouda", 2, "Caramel and dried fruit are a natural fit."),
  .p("redblend", "cheddar", 2, "Sharp, aged cheese matches the wine's weight."),
  .p("redblend", "sheep", 2, "Nutty sheep's milk cheese suits a savory red."),
  .p("redblend", "fresh", 0, "Too mild for such a savory wine.")
)
rm(.p)

TIPS <- c(
  "Serve sparkling and light whites ice cold, fuller whites fridge cold and reds slightly cool (see the Learn tab). Take cheese out about an hour ahead so the flavor opens up.",
  "Aim for one soft, one firm and one bold cheese. Variety in texture matters as much as variety in flavor.",
  "Taste in order from mild to strong, and sip water between pairings. Strong blue cheese first will flatten everything after it.",
  "Bloomy rinds on brie are edible and add earthy notes that suit Pinot Noir."
)

SOURCES <- data.frame(
  label = c("Gary Farrell Winery: Pinot Noir and Cheese, the Complete Pairing Guide",
            "Culture Cheese Magazine: An Expert's Guide to Pairing Wine and Cheese",
            "Tasting Table: Best Trader Joe's Wine and Cheese Pairings",
            "Robert Parker Wine Advocate: Perfect Wine and Cheese Pairings from Trader Joe's",
            "Trader Joe's cheese search", "Trader Joe's wine search"),
  url = c("http://www.garyfarrellwinery.com/pinot-noir-and-cheese-the-complete-pairing-guide/",
          "https://culturecheesemag.com/cheese-pairings/how-to-pair-wine-and-cheese/",
          "https://tastingtable.com/1134375/best-trader-joes-wine-and-cheese-pairings",
          "https://content.robertparker.com/perfect-wine-and-cheese-pairings-from-trader-joe-s",
          "https://www.traderjoes.com/home/search?q=cheese&global=yes",
          "https://www.traderjoes.com/home/search?q=wine&global=yes"),
  stringsAsFactors = FALSE
)
