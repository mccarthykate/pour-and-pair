# Merge a Trader Joe's snapshot (data/tj_snapshot.json, collected with tj_page.js) into
# data/wines.csv, data/cheeses.csv and data/addons.csv.
#  - refreshes prices for items still sold
#  - retires items no longer listed (items noted "Not listed..." are kept, they are in-store only)
#  - brings back retired items that return
#  - adds new wines, cheeses and board items that fit the app's types and categories
# Writes data/last_checked.txt and data/changes.md, and appends to data/changelog.md.
# Exits with status 2 without changing anything if the snapshot looks incomplete
# (for example, Trader Joe's blocked the check), so nothing is retired by mistake.
# Usage: Rscript scripts/update_catalog.R

suppressMessages(library(jsonlite))
setwd(file.path(dirname(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1])), ".."))

snap <- fromJSON("data/tj_snapshot.json", simplifyVector = TRUE)
items <- snap$items
items <- items[!is.na(items$name) & nzchar(items$name), ]
items$price <- as.numeric(items$price)
items$size_str <- ifelse(is.na(items$uom), "", paste0(format(items$size, trim = TRUE, drop0trailing = TRUE), items$uom))
norm <- function(x) tolower(gsub("\\s+", " ", trimws(x)))
items$key <- norm(items$name)
today <- format(as.POSIXct(Sys.time(), tz = "America/Denver"), "%Y-%m-%d", tz = "America/Denver")

read_csv <- function(f) utils::read.csv(file.path("data", f), stringsAsFactors = FALSE, colClasses = "character",
                                       na.strings = character(), fileEncoding = "UTF-8")
write_csv <- function(df, f) utils::write.csv(df, file.path("data", f), row.names = FALSE, fileEncoding = "UTF-8")
fmt_price <- function(p) sprintf("%.2f", p)

catalogs <- list(wines = read_csv("wines.csv"), cheeses = read_csv("cheeses.csv"), addons = read_csv("addons.csv"))
for (n in names(catalogs)) if (!"added" %in% names(catalogs[[n]])) catalogs[[n]]$added <- ""

# ---- sanity check: did we really see the catalog? ---------------------------------
watch <- unlist(lapply(catalogs, function(d) {
  in_store_only <- if ("note" %in% names(d)) grepl("^Not listed", d$note) else FALSE
  d$name[d$active == "TRUE" & !in_store_only]
}))
seen <- norm(watch) %in% items$key
if (nrow(items) < 300 || mean(seen) < .6) {
  message(sprintf("Snapshot looks incomplete (%d products, %.0f%% of catalog seen). No changes made.", nrow(items), 100 * mean(seen)))
  quit(status = 2)
}

# ---- 1. prices, retirements and returns ---------------------------------------
changes <- list(added = character(), price = character(), removed = character(), back = character())
match_item <- function(name, size = "") {
  hit <- items[items$key == norm(name), , drop = FALSE]
  if (nrow(hit) > 1 && nzchar(size)) { same <- hit[hit$size_str == size, , drop = FALSE]; if (nrow(same)) hit <- same }
  if (nrow(hit)) hit[1, ] else NULL
}
for (n in names(catalogs)) {
  d <- catalogs[[n]]
  for (i in seq_len(nrow(d))) {
    in_store_only <- "note" %in% names(d) && grepl("^Not listed", d$note[i])
    hit <- match_item(d$name[i], if ("size" %in% names(d)) d$size[i] else "")
    if (is.null(hit)) {
      if (d$active[i] == "TRUE" && !in_store_only) { d$active[i] <- "FALSE"; changes$removed <- c(changes$removed, d$name[i]) }
      next
    }
    if (d$active[i] != "TRUE") { d$active[i] <- "TRUE"; changes$back <- c(changes$back, d$name[i]) }
    if (!is.na(hit$price) && abs(as.numeric(d$price[i]) - hit$price) > .001) {
      changes$price <- c(changes$price, sprintf("%s: $%s to $%s", d$name[i], d$price[i], fmt_price(hit$price)))
      d$price[i] <- fmt_price(hit$price)
    }
  }
  catalogs[[n]] <- d
}

# ---- 2. classify new products --------------------------------------------------
has <- function(x, p) grepl(p, x, ignore.case = TRUE, perl = TRUE)
SEASONAL <- "Pumpkin|Peppermint|Egg ?Nog|Gingerbread|Candy Cane|Holiday|Halloween|Valentine|Advent|Gift|Harvest|Thanksgiving|Christmas"

wine_style <- function(name, cat) {
  if (!has(cat, "Wine")) return(NA)
  if (has(name, paste0("Beer|\\bAle\\b|Lager|I\\.?P\\.?A|Stout|Porter|Bock|Cider|Radler|Hefe|Festbier|Oktoberfest|Brew|Vodka|\\bGin\\b|\\bRum\\b|Whisk|Bourbon|",
                       "Tequila|Mezcal|Liqueur|Cocktail|Margarita|Martini|Paloma|Cosmopolitan|Spritz|Amaro|Aperitivo|Sangria|Non-Alcoholic|Mulled|Gl.hwein|",
                       "Wine Based|Flavored|Watermelon|Peach Wine|Love, Olivia|with Spices|Bartender|White Zinfandel|Blush|Orange Wine|Frizzante|", SEASONAL))) return(NA)
  red <- has(name, "Red Wine|Pinot Noir|Cabernet|Merlot|Malbec|Syrah|Shiraz|Zinfandel|Meritage|Montepulciano|Nebbiolo|Chianti|Sangiovese|Tempranillo|Rioja|Bordeaux|Grenache|Garnacha|Barbera|Primitivo|Red Blend")
  if (has(name, "\\bRos[e\u00e9](?!\\w)") && !has(name, "Red Wine")) return("rose")
  if (has(name, "Prosecco")) return("prosecco")
  if (has(name, "Cr[e\u00e9]mant|Champagne|\\bBrut\\b|Sparkling|Blanc de Blancs|\\bCava\\b")) return("cremant")
  if (has(name, "Sauvignon Blanc")) return("sauv")
  if (has(name, "Pinot Grigio|Pino Grigio|Vinho Verde|Vermentino|Albari|Gr[u\u00fc]ner|Pinot Blanc|Picpoul|Soave")) return("lightwhite")
  if (has(name, "Chardonnay")) return("chard")
  if (has(name, "Riesling|Viognier|Gew[u\u00fc]rz")) return("aromatic")
  if (has(name, "Pinot Noir")) return("pinot")
  if (has(name, "Malbec") && !has(name, "Cabernet|&|Blend")) return("malbec")
  if (red && has(name, "Blend|Meritage| & |-|Bordeaux|Cabernet Franc|Merlot|Montepulciano|Nebbiolo|Chianti|Sangiovese|Tempranillo|Rioja|Grenache|Garnacha|Barbera|Primitivo|Red Wine")) return("redblend")
  if (has(name, "Cabernet Sauvignon|Zinfandel|Syrah|Shiraz|Petite Sirah")) return("cab")
  NA
}

cheese_style <- function(name, cat) {
  if (!has(cat, "Cheese") || has(cat, "Slices, Shreds, Crumbles")) return(NA)
  if (has(name, paste0("Slice|Shred|Grated|Crumbl|Snack|Spread|\\bDip\\b|Fondue|Paneer|Queso|Pizza|Bread Cheese|Cream Cheese|Ricotta|Mascarpone|",
                       "Cr[e\u00e8]me Fra|String|Cottage|Whipped|Cheesecake|Vegan|Dairy-Free|Alternative|Tray|Cube|Rosette|Pearl|Cassata|Neufchatel|",
                       "Blueberry|Taco|Ghost Pepper|Scotch Bonnet|Huntsman|Italian Truffle Cheese|", SEASONAL))) return(NA)
  rules <- c(stilton = "White Stilton|Wensleydale", blue = "Blue|Gorgonzola|Stilton|Cambozola|Roquefort|Bleu",
             gouda = "Gouda|Mimolette|Edam|Kaamt", brie = "Brie|Camembert|Triple Cr|Double Cr[e\u00e8]me|Soft Ripened|Saint Andr",
             goat = "Goat|Ch[e\u00e8]vre", fresh = "Mozzarella|Burrata", feta = "Feta",
             parm = "Parm|Reggiano|Grana|Pecorino|Asiago|Romano|Canestrato", sheep = "Manchego|Sheep|Sheepish",
             alpine = "Gruy|Comt|Fontina|Raclette|Emment|Swiss|Alpine|Appenzell|Jarlsberg|Saint Paulin|Vault",
             toscano = "Toscano|Ubriaco|Bigonda|Lagorai|Italian",
             cheddar = "Cheddar|Leicester|Gloucester|Derby|Huntsman|Somerset|Stripey|Promontory")
  for (s in names(rules)) if (has(name, rules[[s]])) return(s)
  NA
}

addon_class <- function(name, cat) {
  if (has(name, SEASONAL)) return(NULL)
  out <- function(category, icon, role) list(category = category, icon = icon, role = role)
  if (has(cat, "Chips, Crackers") && has(name, "Cracker|Crisps|Crispbread|Flatbread|Toasts|Lingue|Rye Chips|Pita Bite|Pretzel Slims") &&
      !has(name, "Potato|Plantain|Apple Chips|Rice Cracker|Animal|Graham|Seaweed|Bamba|Popcorn|Pork|Corn|Tortilla|Shrimp|Lentil|Papadum|Tempeh|Cheese Bites|Chili|Ranch|BBQ|Spicy|Cinnamon|Sugared")) {
    icon <- if (has(name, "Pretzel")) "rect:#9a5a22:#f6efe2:pretzel" else if (has(name, "Round")) "rd:#e4b455:" else
      if (has(name, "Crisps")) "rect:#d9c49a:#6b4a2a" else if (has(name, "Crispbread|Flatbread|Toasts|Lingue")) "rect:#e3cf9a:" else "sq:#d7b277:"
    return(out("Crackers & bread", icon, "carrier"))
  }
  if (has(cat, "Loaves, Rolls") && has(name, "Baguette|Ciabatta|Boule|Batard|Focaccia") && !has(name, "Sliced|Sandwich|Bun|Roll|Garlic|Cheese"))
    return(out("Crackers & bread", if (has(name, "Boule")) "boule:#f0dcae:" else "bread:#f2dfb2:", "carrier"))
  if (has(cat, "Fruits") && !has(name, "Avocado|Banana|Lemon|Lime|Orange|Mandarin|Grapefruit|Pineapple|Mango|Kiwi|Melon|Coconut|Medley|Smoothie|Puree")) {
    icon <- if (has(name, "Grape")) "grape:#6b3b7a:" else if (has(name, "Pear")) "apple:#9fae3c:" else if (has(name, "Apple")) "apple:#c7352d:" else
      if (has(name, "Fig")) "fig:#5b3a62:" else if (has(name, "Strawberr")) "berry:#d62a3a:" else if (has(name, "Raspberr|Cherr")) "bowl:#2a1010:#b3203a:round" else
      if (has(name, "Blueberr|Blackberr")) "bowl:#1a1a2a:#3b3f7a:round" else if (has(name, "Pomegranate")) "pom:#a3122f:" else if (has(name, "Persimmon|Plum|Apricot")) "disc:#e8932e:" else NA
    if (!is.na(icon)) return(out("Fruit & veggies", icon, "fresh"))
  }
  if (has(cat, "Veggies") && has(name, "Cucumber|Cherry Tomato|Tiny Tomato|Campari|Radish"))
    return(out("Fruit & veggies", if (has(name, "Tomato|Campari")) "tom:#d83a2b:" else "cuke:#5f9a45:", "fresh"))
  if (has(cat, "Nuts, Dried Fruits")) {
    if (has(name, "Apricot|Fig|Date|Cherr|Cranberr|Raisin|Prune") && !has(name, "Chile|Spiced|Leather|Square|Round|Nut|Chocolate")) {
      icon <- if (has(name, "Date")) "ov:#5a3420:" else if (has(name, "Apricot")) "disc:#e8932e:" else if (has(name, "Fig")) "fig:#b58a6a:" else "bowl:#3a1018:#b3203a:small"
      return(out("Fruit & veggies", icon, "sweet"))
    }
    if (has(name, "Almond|Pecan|Pistachio|Walnut|Cashew|Hazelnut|Macadamia|Marcona|Mixed Nuts|Nut Mix") &&
        !has(name, "Butter|Trail|Seed|Chocolate|Ranch|Chile|Lime|Garlic|Onion|Everything|Thai|Raw|Unsalted|Baking|Sliced|Slivered|Pieces")) {
      v <- if (has(name, "Almond|Marcona")) c("#c89b62", "almond") else if (has(name, "Pecan")) c("#8c5a34", "pecan") else
        if (has(name, "Pistachio")) c("#cdb98e", "pistachio") else if (has(name, "Walnut")) c("#b98a55", "walnut") else c("#c4a26a", "mixed")
      return(out("Nuts", paste0("bowl:#efe6d6:", v[1], ":", v[2]), if (has(name, "Candied|Honey|Maple|Sweet")) "sweet" else "crunch"))
    }
  }
  if (has(cat, "Nut Butters & Fruit Spreads|Honeys, Syrups|Condiments|Dressing & Seasoning|Dip/Spread") &&
      has(name, "Jam|Preserves|Fig Butter|Fruit Spread|Marmalade|Chutney|Honey|Mustard|Tapenade|Pesto|Quince|Membrillo|Jelly|Spreads for Chees") &&
      !has(name, "Peanut|Almond Butter|Cashew|Sunflower|Cookie|Granola|Cocoa|Hazelnut|Syrup|Caramel|Chocolate|Hummus|Dressing|Popcorn|Ketchup|Mayo|Concord|Pasta|Simmer|BBQ|Salsa|Roasted|Cashews|Macadamia|Yellow Mustard|Aioli")) {
    if (has(name, "Honey") && !has(name, "Mustard")) return(out("Spreads & condiments", "bowl:#e0a321::dipper", "sweet"))
    col <- if (has(name, "Mustard")) "#c9a227" else if (has(name, "Tapenade|Olive")) "#6a6a3a" else if (has(name, "Pesto")) "#5b8a3c" else
      if (has(name, "Chutney")) "#d98a1c" else if (has(name, "Fig")) "#5a2f3a" else if (has(name, "Apricot|Peach|Orange|Marmalade")) "#e5902d" else
      if (has(name, "Cherr|Berr|Cranberr")) "#a22537" else if (has(name, "Quince|Membrillo")) "#c9752d" else if (has(name, "Pepper")) "#c0392b" else "#b8864a"
    role <- if (has(name, "Mustard")) "tart" else if (has(name, "Tapenade|Pesto")) "salty" else "sweet"
    return(out("Spreads & condiments", paste0("bowl:", col, "::spoon"), role))
  }
  if (has(cat, "Beef, Pork|Charcuterie|Deli") && has(name, "Salami|Salame|Prosciutto|Soppressata|Chorizo|Coppa|Capicola|Bresaola|Mortadella|Charcut|Speck|Jam[o\u00f3]n|Serrano|Finocchiona") &&
      !has(name, "Sausage|Bacon|Pancetta|\\bHam\\b|Ground|Roast|Corned|Soy|Pizza|Pepperoni")) {
    icon <- if (has(name, "Prosciutto|Coppa|Capicola|Speck|Jam[o\u00f3]n|Serrano") && !has(name, "Bresaola|Beef")) "fold:#e6a09c:" else
      if (has(name, "Bresaola")) "roll:#7a2426::lean" else if (has(name, "Mortadella")) "roll:#eaaaa4::mort" else
      if (has(name, "Pepper")) "roll:#7a2c2a::pepper" else if (has(name, "Chorizo")) "roll:#c2481f::paprika" else "roll:#b3413a:"
    return(out("Cured meats", icon, "salty"))
  }
  if (has(cat, "Packaged Fish, Meat, Fruit|Pickle") && has(name, "Olive|Cornichon|Pickle|Gherkin|Artichoke|Pepperoncini|Picant|Roasted Red Pepper|Pickled|Dolma|Caper") &&
      !has(name, "Oil|Tuna|Sardine|Hummus|Tapenade|Spread|Chips|Popcorn|Tomato|Salad|Relish|Jalape|Sandwich")) {
    if (has(name, "Kalamata")) return(out("Olives & pickled", "bowl:#2a1f22:#4f2848:olive", "salty"))
    if (has(name, "Olive") && has(name, "Stuffed|Pimento")) return(out("Olives & pickled", "bowl:#2a1f22:#8aa03a:stuffed", "salty"))
    if (has(name, "Olive")) return(out("Olives & pickled", "bowl:#2a1f22:#7a8a3a:olive", "salty"))
    if (has(name, "Dolma")) return(out("Olives & pickled", "dolma:#6b7a34:", "salty"))
    if (has(name, "Cornichon|Pickle|Gherkin") && !has(name, "Onion")) return(out("Olives & pickled", "spear:#6a9a3a:", "tart"))
    if (has(name, "Artichoke")) return(out("Olives & pickled", "bowl:#2a2f18:#b5b26a:petal", "tart"))
    if (has(name, "Onion")) return(out("Olives & pickled", "bowl:#3a1030:#d066a0:ring", "tart"))
    if (has(name, "Roasted Red Pepper")) return(out("Olives & pickled", "fold:#c93a24::pepper", "tart"))
    return(out("Olives & pickled", "bowl:#2a1010:#d42a26:round", "tart"))
  }
  if (has(cat, "Candies") && has(name, "Dark Chocolate") && has(name, "Bar|Bark|Covered|Almond|Cherr|Pistachio|Fig|Orange|Thin") &&
      !has(name, "Peanut|Mint|Marshmallow|Calendar|Pretzel|Cookie|Joe|Caramel|Sunflower|Coffee|Candy|Corn|Rice|Crispy|Powerberr|Star|Heart|Chips|Coconut|White Chocolate|Cup|Gummy|Mango|Passion"))
    return(out("Dark chocolate", if (has(name, "Bar|Bark|Thin")) "choc:#4a2a20:" else "ov:#3a2218:", "sweet"))
  NULL
}

# ---- 3. add new items ------------------------------------------------------------
existing <- unique(unlist(lapply(catalogs, function(d) paste(norm(d$name), if ("size" %in% names(d)) d$size else ""))))
existing_names <- unique(unlist(lapply(catalogs, function(d) norm(d$name))))
items <- items[order(items$name), ]
for (i in seq_len(nrow(items))) {
  it <- items[i, ]
  if (is.na(it$price)) next
  ws <- wine_style(it$name, it$category)
  if (!is.na(ws)) {
    if (it$key %in% existing_names) next
    catalogs$wines <- rbind(catalogs$wines, data.frame(style = ws, name = it$name, price = fmt_price(it$price), note = "",
                                                         source = "auto", active = "TRUE", added = today))
    existing_names <- c(existing_names, it$key); changes$added <- c(changes$added, sprintf("Wine (%s): %s", ws, it$name)); next
  }
  cs <- cheese_style(it$name, it$category)
  if (!is.na(cs)) {
    if (paste(it$key, it$size_str) %in% existing || (it$key %in% existing_names && !nzchar(it$size_str))) next
    catalogs$cheeses <- rbind(catalogs$cheeses, data.frame(style = cs, name = it$name, size = it$size_str, price = fmt_price(it$price),
                                                             note = "", source = "auto", active = "TRUE", added = today))
    existing <- c(existing, paste(it$key, it$size_str)); existing_names <- c(existing_names, it$key)
    changes$added <- c(changes$added, sprintf("Cheese (%s): %s", cs, it$name)); next
  }
  ac <- addon_class(it$name, it$category)
  if (!is.null(ac) && !it$key %in% existing_names) {
    catalogs$addons <- rbind(catalogs$addons, data.frame(category = ac$category, name = it$name, price = fmt_price(it$price), size = it$size_str,
                                                           icon = ac$icon, role = ac$role, source = "auto", active = "TRUE", added = today))
    existing_names <- c(existing_names, it$key); changes$added <- c(changes$added, sprintf("%s: %s", ac$category, it$name))
  }
}

# ---- 4. write --------------------------------------------------------------------
write_csv(catalogs$wines, "wines.csv"); write_csv(catalogs$cheeses, "cheeses.csv"); write_csv(catalogs$addons, "addons.csv")
writeLines(format(as.POSIXct(Sys.time(), tz = "America/Denver"), "%B %e, %Y", tz = "America/Denver"), "data/last_checked.txt")
section <- function(title, x) if (length(x)) c(paste0("### ", title, " (", length(x), ")"), paste0("- ", x), "") else character()
report <- c(paste0("## Trader Joe's check, ", today), "",
            section("New items", changes$added), section("Price changes", changes$price),
            section("No longer listed (hidden)", changes$removed), section("Back on the website", changes$back))
if (!any(lengths(changes))) report <- c(report, "No changes.", "")
writeLines(report, "data/changes.md")
log <- if (file.exists("data/changelog.md")) readLines("data/changelog.md") else "# Trader Joe's catalog changes"
if (any(lengths(changes))) writeLines(c(log[1], "", report, log[-1]), "data/changelog.md")
cat(report, sep = "\n")
