# Pairing logic, product facts and board advice.

# ---- Traits used to explain why a pairing works -------------------------
# body / intensity run 1 (light) to 3 (big); rich = how fatty the cheese is
WINE_TRAITS <- list(
  cremant    = list(bubbles = TRUE,  acid = "high",        tannin = "none",   sweet = "dry",     body = 1,   dry_caution = TRUE,  tannic = FALSE, temp = "Serve ice cold, about 38 to 45 \u00b0F (Wine Folly)."),
  prosecco   = list(bubbles = TRUE,  acid = "medium-high", tannin = "none",   sweet = "off-dry", body = 1,   dry_caution = FALSE, tannic = FALSE, temp = "Serve ice cold, about 38 to 45 \u00b0F (Wine Folly)."),
  sauv       = list(bubbles = FALSE, acid = "high",        tannin = "none",   sweet = "dry",     body = 1,   dry_caution = TRUE,  tannic = FALSE, temp = "Serve ice cold, about 38 to 45 \u00b0F (Wine Folly, light whites)."),
  lightwhite = list(bubbles = FALSE, acid = "medium-high", tannin = "none",   sweet = "dry",     body = 1,   dry_caution = TRUE,  tannic = FALSE, temp = "Serve ice cold, about 38 to 45 \u00b0F (Wine Folly, light whites)."),
  chard      = list(bubbles = FALSE, acid = "medium",      tannin = "none",   sweet = "dry",     body = 2,   dry_caution = FALSE, tannic = FALSE, temp = "Serve fridge cold, about 44 to 55 \u00b0F, so the fuller body comes through (Wine Folly)."),
  aromatic   = list(bubbles = FALSE, acid = "high",        tannin = "none",   sweet = "dry",     body = 1.5, dry_caution = TRUE,  tannic = FALSE, temp = "Serve ice cold to fridge cold, about 38 to 50 \u00b0F (Wine Folly)."),
  rose       = list(bubbles = FALSE, acid = "medium-high", tannin = "none",   sweet = "dry",     body = 1,   dry_caution = FALSE, tannic = FALSE, temp = "Serve fridge cold, about 44 to 55 \u00b0F (Wine Folly); sparkling ros\u00e9 colder."),
  pinot      = list(bubbles = FALSE, acid = "medium-high", tannin = "low",    sweet = "dry",     body = 1.5, dry_caution = FALSE, tannic = FALSE, temp = "Serve cool, about 55 to 60 \u00b0F (Wine Folly, light reds)."),
  malbec     = list(bubbles = FALSE, acid = "medium",      tannin = "medium", sweet = "dry",     body = 2.5, dry_caution = FALSE, tannic = TRUE,  temp = "Serve slightly cool, about 60 to 68 \u00b0F (Wine Folly, bold reds)."),
  cab        = list(bubbles = FALSE, acid = "medium",      tannin = "high",   sweet = "dry",     body = 3,   dry_caution = FALSE, tannic = TRUE,  temp = "Serve slightly cool, about 60 to 68 \u00b0F (Wine Folly, bold reds)."),
  redblend   = list(bubbles = FALSE, acid = "medium-high", tannin = "medium", sweet = "dry",     body = 2.5, dry_caution = FALSE, tannic = TRUE,  temp = "Serve cool to slightly cool, about 55 to 65 \u00b0F depending on weight (Wine Folly).")
)
CHEESE_TRAITS <- list(
  brie    = list(rich = 3, salt = 1, intensity = 1,   tang = FALSE, sweetish = FALSE, rest = "Take it out about an hour ahead (at least 30 minutes) so the center softens. Soft cheeses can need more time in a cold room."),
  goat    = list(rich = 2, salt = 2, intensity = 2,   tang = TRUE,  sweetish = FALSE, rest = "Take it out 30 to 60 minutes ahead."),
  fresh   = list(rich = 2, salt = 1, intensity = 1,   tang = FALSE, sweetish = FALSE, rest = "Take it out about 30 minutes ahead so it is not fridge cold."),
  feta    = list(rich = 2, salt = 3, intensity = 2,   tang = TRUE,  sweetish = FALSE, rest = "Take it out about 30 minutes ahead."),
  sheep   = list(rich = 3, salt = 2, intensity = 2,   tang = FALSE, sweetish = FALSE, rest = "Take it out about an hour ahead; firm cheeses can take up to 2 hours in a cool room."),
  alpine  = list(rich = 3, salt = 2, intensity = 2,   tang = FALSE, sweetish = FALSE, rest = "Take it out about an hour ahead; firm cheeses can take up to 2 hours in a cool room."),
  toscano = list(rich = 3, salt = 2, intensity = 1.5, tang = FALSE, sweetish = FALSE, rest = "Take it out about an hour ahead."),
  cheddar = list(rich = 3, salt = 3, intensity = 3,   tang = FALSE, sweetish = FALSE, rest = "Take it out about an hour ahead; firm cheeses can take up to 2 hours in a cool room."),
  gouda   = list(rich = 3, salt = 2, intensity = 2.5, tang = FALSE, sweetish = TRUE,  rest = "Take it out about an hour ahead; firm cheeses can take up to 2 hours in a cool room."),
  parm    = list(rich = 3, salt = 3, intensity = 3,   tang = FALSE, sweetish = FALSE, rest = "Take it out about an hour ahead, and break it into rough chunks rather than slicing."),
  blue    = list(rich = 3, salt = 3, intensity = 3,   tang = FALSE, sweetish = FALSE, rest = "Take it out about an hour ahead."),
  stilton = list(rich = 2, salt = 2, intensity = 1.5, tang = TRUE,  sweetish = FALSE, rest = "Take it out about an hour ahead.")
)

# Why a pairing works (or doesn't), built from the traits above.
# Returns a data.frame of tag, text, type ("pos" or "neg").
explain_pair <- function(ws, cs, wn, cn) {
  w <- WINE_TRAITS[[ws]]; ch <- CHEESE_TRAITS[[cs]]
  out <- list()
  add <- function(tag, text, type) out[[length(out) + 1]] <<- data.frame(tag = tag, text = text, type = type, stringsAsFactors = FALSE)
  hi_acid <- w$acid %in% c("medium-high", "high")

  if (w$bubbles && ch$rich >= 2)
    add("Bubbles scrub fat", "The bubbles and acidity scrub butterfat off your tongue, so every bite of the cheese tastes as fresh as the first.", "pos")
  else if (hi_acid && ch$rich >= 3)
    add("Acid cuts fat", "The wine's acidity cuts through the rich, fatty texture of the cheese and keeps your mouth from feeling coated.", "pos")
  if (w$tannin %in% c("medium", "high") && ch$rich >= 3)
    add("Fat softens tannin", "Tannin is the dry, grippy feel in red wine. It binds to the fat and protein in the cheese, which softens the tannin and makes the wine taste smoother and fruitier.", "pos")
  if (w$tannin %in% c("medium", "high") && ch$rich < 3)
    add("Tannin clash", "Tannin needs plenty of fat to soften it. This cheese is too light, so the wine tastes dry, bitter or chalky.", "neg")
  if (w$tannin == "low" && ch$rich >= 2)
    add("Gentle on cheese", "The wine is low in tannin, so it will not fight the cheese. That is why it is so flexible.", "pos")
  if (ch$salt == 3 && w$sweet == "off-dry")
    add("Salt + sweet", "Salty cheese against a wine with a little sweetness is a classic contrast. The salt makes the fruit taste riper and the sweetness tames the salt.", "pos")
  if (ch$tang && hi_acid)
    add("Tang echoes tang", "Both are tangy, so the wine's acidity matches the cheese rather than fighting it.", "pos")
  if (!w$bubbles && w$body >= 2 && w$tannin == "none" && ch$rich >= 3)
    add("Rich meets rich", "A creamy, full-bodied white matches the cheese's richness. For more contrast, choose a leaner, less oaky bottle.", "pos")
  if (ch$sweetish && w$sweet == "dry" && hi_acid)
    add("Sweet vs. sour", "Caramel-sweet cheese makes a tart, dry wine taste sour and thin. The wine needs ripe fruit or some sweetness.", "neg")
  if (ch$intensity - w$body >= 1.5)
    add("Cheese overpowers", "The cheese is much stronger than the wine, so the wine tastes thin and disappears.", "neg")
  else if (ch$intensity == 3 && w$body < 3 && w$tannin == "none")
    add("Cheese overpowers", "This much pungency overwhelms a wine that has no tannin or sweetness to push back.", "neg")
  else if (w$body - ch$intensity >= 1.5)
    add("Wine overpowers", "The wine is big and the cheese is mild, so the cheese gets lost.", "neg")
  else if (abs(w$body - ch$intensity) <= 0.5)
    add("Similar weight", "Wine and cheese are about the same weight, so neither one hides the other.", "pos")

  if (!length(out)) return(data.frame(tag = character(), text = character(), type = character()))
  do.call(rbind, out)
}

# ---- Product facts, matched from the product name ---------------------
.rules <- function(...) { x <- list(...); data.frame(pattern = names(x), text = unlist(x), row.names = NULL, stringsAsFactors = FALSE) }
WINE_FACTS <- .rules(
  "Blanc de Blancs" = "Blanc de Blancs means white wine made only from white grapes (here, Chardonnay), so it is crisp and citrusy.",
  "Brut" = "Brut means dry, with very little sugar.",
  "Champagne" = "True Champagne comes from the Champagne region of France and is made with the traditional method, which gives fine bubbles and a toasty flavor.",
  "Cr[eé]mant" = "Crémant is French sparkling wine made the same way as Champagne, but from outside the Champagne region.",
  "Prosecco" = "Prosecco is Italian sparkling wine. It is fruitier and softer than Champagne, with larger, lighter bubbles.",
  "DOCG" = "DOCG is the highest quality tier in Italian wine law.",
  "Marlborough|New Zealand" = "New Zealand Sauvignon Blanc is known for grapefruit and fresh-cut grass.",
  "Vinho Verde" = "Vinho Verde is a Portuguese white that is light, tart and often a little fizzy.",
  "Vermentino" = "Vermentino is a crisp Italian white with citrus and a hint of salinity.",
  "Pinot Grigio|Pino Grigio" = "Pinot Grigio is light, dry and neutral, which makes it easy with mild cheese.",
  "Viognier" = "Viognier is rounder and more floral than most whites, with peach and apricot.",
  "Dry Riesling" = "Riesling is naturally high in acid; this one is dry, so it works like a crisp white.",
  "\\bRos[e\u00e9]\\b" = "Rosé gets its color from brief contact with red grape skins, so it has red-fruit flavor with white-wine freshness.",
  "Zinfandel" = "Zinfandel is ripe and jammy with higher alcohol, which loves salty, savory food.",
  "Syrah|Shiraz" = "Syrah is dark and peppery with firm tannin.",
  "Meritage" = "Meritage is a Bordeaux-style blend, usually led by Cabernet Sauvignon and Merlot.",
  "Montepulciano" = "Montepulciano d'Abruzzo is soft and plummy, with gentler tannin than most big reds.",
  "Nebbiolo" = "Nebbiolo is high in tannin and acid, with cherry and rose notes. It wants rich cheese.",
  "Susumaniello" = "Susumaniello is a red grape from Puglia in southern Italy: dark, spicy and fairly full.",
  "Malbec" = "Malbec is Argentina's best-known red: full-bodied with dark fruit. Murray's Cheese notes it is usually gentler in structure than Cabernet, with the fruit out front.",
  "Cabernet Sauvignon" = "Cabernet Sauvignon has firm tannin and dark fruit such as black currant."
)
CHEESE_FACTS <- .rules(
  "Truffle" = "Truffle adds an earthy, mushroomy note. It suits Pinot Noir and the toasty character of sparkling wine; keep the wine elegant.",
  "Hot Honey|Chil[ie]|Wildfire|Scotch Bonnet|Spicy|Ghost" = "Heat makes tannin and alcohol feel harsher. Bubbles, off-dry or fruity low-tannin wines handle it best.",
  "Honey" = "Honey sweetens the cheese, so choose a wine with a little sweetness or high acidity. A tannic red will taste bitter.",
  "Pepper" = "Pepper adds gentle spice that suits fruity reds and peppery Syrah.",
  "Soaked|Ubriaco" = "The rind is soaked in wine, so it already carries grape flavor. Matching that wine echoes it; a contrasting one gives a surprise.",
  "Cranberry|Mango|Apple|Strawberry|Blueberry|Figgy" = "Fruit in the cheese adds sweetness, so lean toward a fruity, higher-acid wine such as rosé or Prosecco.",
  "Smoked" = "Smoke needs a wine with some weight, like a bold red or an oaked Chardonnay. Delicate whites get lost.",
  "Caramel|Toffee" = "Sweet-savory cheese suits wines with ripe fruit. Very dry, tart wines taste sour next to it.",
  "Aged|1000 Day|Stravecchio|Cave|Over 55" = "Aging concentrates flavor and salt and creates crunchy crystals, so the cheese can stand up to tannin.",
  "Triple|Double Cr|Double Cream Brie|Saint Andr|Cambozola" = "Extra cream means extra butterfat. Acidity and bubbles balance it best.",
  "Burrata" = "Burrata is a mozzarella shell filled with cream. Keep the wine light and bright.",
  "Feta" = "Feta is brined, so it is salty and tangy. Crisp, fruity wines refresh it.",
  "Manchego" = "Manchego is a Spanish sheep's milk cheese: firm, nutty and gently sweet.",
  "Parmigiano|Parmesan" = "Parmigiano Reggiano is aged for years. Its crystals are protein crystals, not salt, and it has loads of umami.",
  "Gorgonzola|Blue English|Wildfire Blue|Cambozola" = "Blue cheese gets its veins and tang from mold. It is salty and strong, so it needs a bold or sweet partner."
)
facts_for <- function(name, tbl, max_n = 3) {
  hit <- tbl$text[vapply(tbl$pattern, function(p) grepl(p, name, ignore.case = TRUE), TRUE)]
  head(unique(hit), max_n)
}

# ---- Board advice -----------------------------------------------------
# How to choose a carrier (bread or cracker) and a topper (jam or meat)
CHEESE_ADVICE <- list(
  brie    = c(carrier = "Pick baguette or crostini over crackers. Soft cheese needs a crisp-edged base to spread on, and the chewy crumb of a baguette gives a soft-on-soft bite some texture. Keep it plain so the buttery rind leads.",
              topper = "Jam is the better call. Sweet-tart fruit against rich cream is the classic contrast. Add cured meat only if your wine is dry or sparkling, since salt and bubbles go together."),
  goat    = c(carrier = "Plain crostini or a thin cracker. The cheese is tangy and soft, so the base should stay neutral and crunchy.",
              topper = "Honey or berries, not meat. Sweetness softens the lemony tang, while salty meats fight a tangy cheese."),
  fresh   = c(carrier = "Ciabatta or baguette. Fresh mozzarella and burrata are mild and wet, so bread soaks them up, while a cracker goes soggy.",
              topper = "Skip the jam. Stay savory with tomatoes, pesto, olive oil and a little prosciutto."),
  feta    = c(carrier = "Pita crackers or flatbread. Salty, crumbly cheese wants a sturdy base for scooping.",
              topper = "Neither jam nor sweet meat. Go briny and fresh: olives, cucumber, roasted peppers, dolmas."),
  sheep   = c(carrier = "Plain crackers or sliced baguette. Firm, nutty cheese shines on a neutral base.",
              topper = "Both. Something sweet-tart (apricot or quince) and something cured (chorizo) is the Spanish tradition."),
  alpine  = c(carrier = "A sturdy cracker or sourdough. Dense, nutty cheese holds up to chewy, tangy bread, and sourdough's acidity cuts the richness.",
              topper = "Pickles and mustard before jam. Alpine cheese loves something sharp. Add prosciutto or apple for a little sweetness."),
  toscano = c(carrier = "Plain crackers or crostini. Avoid flavored crackers that bury the pepper or the wine-soaked rind.",
              topper = "Mild cured meat and fresh fruit. A light salami and grapes keep it Italian. Heavy jam is unnecessary."),
  cheddar = c(carrier = "Sturdy, salty crackers. Sharp, crumbly cheese wants crunch.",
              topper = "Fruit and mustard or a spicy-sweet condiment. Sweetness tames sharpness. Salami works too."),
  gouda   = c(carrier = "Plain crackers or seeded crisps. Gouda's crunchy crystals already add texture, so keep the base simple.",
              topper = "Sweet: dates, pecans, or mustard for contrast. Dark chocolate suits aged gouda."),
  parm    = c(carrier = "Skip the cracker or use thin crisps. Hard cheese is the crunch; break it into chunks.",
              topper = "Cured meat, figs and a spoon of fig butter. Salty-sweet is the point."),
  blue    = c(carrier = "Plain crackers or walnut bread. Blue is so strong that a flavored base is too much.",
              topper = "Sweet, not meat. Honey, figs or dark chocolate give blue cheese the contrast it needs."),
  stilton = c(carrier = "Plain or rosemary crisps. The cheese already has fruit in it.",
              topper = "Nuts and fresh fruit more than jam, since the cheese is already fruity.")
)
WINE_ADVICE <- list(
  cremant  = "Dry bubbles love salt and fat: salted nuts, prosciutto, crunchy snacks. Go easy on sweet spreads, because they make a brut wine taste sharp.",
  prosecco = "Prosecco's soft sweetness is fine with fruit, honey and cured meat. It is the most forgiving wine on the board.",
  sauv     = "Zesty, herbal wine likes fresh, briny things: cucumber, tomato, olives. Skip heavy sweet spreads.",
  lightwhite = "Keep everything light: fresh vegetables, olives, mild crackers. Strong flavors flatten this wine.",
  chard    = "Creamy Chardonnay echoes nuts and apples. Stone-fruit preserves work well.",
  aromatic = "Fruity chutney and pistachios suit Riesling and Viognier. Keep sweet items modest with a dry bottle.",
  rose     = "Rosé is flexible. Berries, cured meat, olives and fruit all work.",
  pinot    = "Earthy, cherry-fruit wine. Figs, mild cured meat and walnuts suit it.",
  malbec   = "Murray's Cheese suggests a dry-cured ham (they name jam\u00f3n serrano) as a contrast to Malbec's fruit; prosciutto is the closest here. Salty, fatty nuts and cured meat help with tannin. Skip spicy items.",
  cab      = "Big tannin needs fat and salt: salami, nuts, dark chocolate. Skip anything spicy or very sweet.",
  redblend = "Savory reds like cured meat, olives, roasted peppers and herby spreads."
)

BOARD_ROLES <- c(carrier = "A cracker or bread to carry the cheese", sweet = "Something sweet",
                 salty = "Something salty or cured", tart = "Something tart or briny",
                 fresh = "Something fresh", crunch = "Something crunchy")

GUIDE <- list(
  list(q = "Cracker or bread?",
       a = c("Offer both, and match the carrier to the cheese texture. Soft, creamy cheeses (brie, goat, burrata) go on baguette, crostini or ciabatta. The crisp crust gives texture, the chewy crumb holds a spread, and a good baguette is mild enough to let the cheese lead.",
             "Firm, crumbly or aged cheeses (cheddar, gouda, Manchego, Parmigiano) go well on crackers. Crackers stay crisp for hours, add salt, and do not compete with a strong cheese.",
             "Baguette is best the day you buy it. Crackers are the safe workhorse. Keep carriers plain for delicate cheeses and save flavored crisps (fig and olive, raisin rosemary) for sharp cheddar, blue or Stilton, where they act like a built-in condiment.")),
  list(q = "What makes a balanced board?",
       a = c("Aim for one item from each job: a carrier (cracker or bread), something sweet (jam, honey, dried fruit), something salty or cured (meat, olives), something fresh (fruit, vegetables), something tart or briny (pickles, mustard) and something crunchy (nuts). Most boards need only one or two items per job. The checklist in step 5 tracks this for you.")),
  list(q = "Should the sweet thing be sweeter than my wine?",
       a = c("No. Wine Folly's rule for food and wine is that the wine should be at least as sweet as the food; if it is less sweet, it tends to taste bitter and tart. So with a dry brut, sparkling or crisp white, keep honey and jam to small amounts or pick a tarter spread. Prosecco, rosé and fruity reds handle more sweetness. The app marks these as \"go easy\" when it applies.")),
  list(q = "How do I lay it out?",
       a = c("Murray's Cheese's advice: set the cheese out about an hour before guests arrive, give each cheese its own knife, and cut a few pieces ahead so people start eating. Arrange from mildest to strongest (Murray's suggests going clockwise from 12 o'clock) so guests taste in that order.",
             "Put spreads in a bowl with a spoon, and place honey or chutney between cheeses that suit it; Murray's puts blue and goat cheese on either side of the honey. Fill the gaps between cheeses with the extras.")),
  list(q = "How much do I need?",
       a = c("Use the guest planner at the top. Published guidance varies widely: Murray's Cheese suggests about 1 oz of each cheese per person, Castello suggests 1 to 1.75 oz for a starter and 7 to 8 oz when cheese is the meal. The planner uses 2.5 oz per person for an appetizer and 5 oz for a main event, which sits in the middle. Wine assumes 2 glasses per person for an appetizer and 3 for a main event, with 5 glasses to a bottle. The Learn tab has the details."))
)

# ---- Guest planner ----------------------------------------------------
plan_board <- function(guests, appetite) {
  main <- identical(appetite, "main")
  cheese_pp <- if (main) 5 else 2.5
  glasses   <- if (main) 3 else 2
  pieces_pp <- if (main) 8 else 5
  types <- if (guests <= 6) 3 else if (guests <= 12) 4 else 5  # Murray's: 3 to 5 cheeses
  bottles <- ceiling(guests * glasses / 5)
  list(
    guests = guests, types = types, cheese_oz = guests * cheese_pp,
    bottles = bottles, glasses = glasses,
    wine_types = min(bottles, if (guests <= 6) 1 else if (guests <= 14) 2 else 3),
    pieces = guests * pieces_pp,
    meat_oz = guests * (if (main) 3 else 1.5),
    fruit_oz = guests * (if (main) 5 else 3),
    nuts_oz = guests * (if (main) 1 else 0.5),
    spreads = max(1, ceiling(guests / 8))
  )
}
fmt_oz <- function(oz) if (oz >= 16) sprintf("%.1f lb", oz / 16) else sprintf("%g oz", round(oz, 1))
size_oz <- function(s) {
  n <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", s)))
  if (grepl("Lb", s)) n * 16 else if (grepl("Oz", s)) n else NA_real_
}
