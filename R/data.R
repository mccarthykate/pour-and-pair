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

# ---- Products ---------------------------------------------------------------
# Wines and cheeses live in data/wines.csv and data/cheeses.csv so the nightly
# Trader Joe's check (scripts/update_catalog.R) can add items, refresh prices and
# retire discontinued ones. Only active rows are shown.
WINE_PRODUCTS <- read_catalog("wines.csv")
WINE_PRODUCTS <- WINE_PRODUCTS[WINE_PRODUCTS$style %in% WINE_STYLES$id, ]
WINE_PRODUCTS$key <- paste0("w", seq_len(nrow(WINE_PRODUCTS)))
CHEESE_PRODUCTS <- read_catalog("cheeses.csv")
CHEESE_PRODUCTS <- CHEESE_PRODUCTS[CHEESE_PRODUCTS$style %in% CHEESE_STYLES$id, ]
CHEESE_PRODUCTS$key <- paste0("c", seq_len(nrow(CHEESE_PRODUCTS)))
LAST_CHECKED <- if (file.exists(file.path("data", "last_checked.txt"))) readLines(file.path("data", "last_checked.txt"), warn = FALSE)[1] else NA_character_

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
