# The "Learn" tab: an in-depth, sourced guide to wine and cheese pairing and cheese boards.
# Every claim below is tied to a source in LEARN_SOURCES. Sources were checked on 2026-10-29.

LEARN_SOURCES <- list(
  wf6       = c("Wine Folly: 6 Tips on Pairing Wine and Cheese", "https://winefolly.com/wine-pairing/6-tips-on-pairing-wine-and-cheese/"),
  wfideas   = c("Wine Folly: Wine and Cheese Pairing Ideas", "https://winefolly.com/tips/wine-cheese-pairing-ideas"),
  wftemp    = c("Wine Folly: Ideal Serving Temperature for Wine", "https://winefolly.com/tutorial/wine-serving-temperature/"),
  wfsweet   = c("Wine Folly: 5 Tips to Perfect Food and Wine Pairing", "https://winefolly.com/deep-dive/5-tips-to-perfect-food-and-wine-pairing/"),
  culture   = c("culture: An Expert's Guide to Pairing Wine and Cheese", "https://culturecheesemag.com/cheese-pairings/how-to-pair-wine-and-cheese/"),
  gf        = c("Gary Farrell Winery: Pinot Noir and Cheese", "http://www.garyfarrellwinery.com/pinot-noir-and-cheese-the-complete-pairing-guide/"),
  npr       = c("NPR / KQED: Pairing Wine and Cheese? Science Says White May Be a Better Choice", "https://www.kqed.org/bayareabites/113616/pairing-wine-and-cheese-science-says-white-may-be-a-better-choice-than-red"),
  ucd       = c("UC Davis: Effect of cheese on red wine flavor", "https://wine.ucdavis.edu/node/3911"),
  decanter  = c("Decanter: Wine and cheese incompatible, says research", "https://www.decanter.com/wine-news/wine-and-cheese-incompatible-says-research-95439/"),
  murraysdm = c("The Daily Meal: How to Build a Cheese Board (Adam Goddu, Murray's Cheese)", "https://www.thedailymeal.com/eat/how-build-cheese-board"),
  murraysmc = c("Marie Claire: How to Make a Cheese Plate (Murray's Cheese tips)", "https://www.marieclaire.com/food-cocktails/a16151/cheese-plate-instructions"),
  murraystt = c("Tasting Table: Cheese and Charcuterie for Malbec (John Montez, Murray's Cheese)", "https://www.tastingtable.com/1732825/cheese-charcuterie-pairing-malbec-wine"),
  wisc      = c("Wisconsin Cheese: Malbec Wine Pairing", "https://www.wisconsincheese.com/about-cheese/malbec-wine-pairing"),
  castello  = c("Castello: How much cheese to serve", "https://www.castellocheese.com/en-us/articles/how-much-cheese-to-serve/"),
  sheridans = c("Sheridans Cheesemongers: How long do I leave cheese out before serving?", "https://sheridanscheesemongers.com/faqs/how-long-do-i-leave-cheese-out-before-serving-2/"),
  bordbia   = c("Bord Bia: Serving your cheese", "https://www.bordbia.ie/farmhouse-cheese/serving-your-cheese/"),
  ttips     = c("Tasting Table: 12 Tips for the Ultimate Cheese Board", "https://www.tastingtable.com/1280089/tips-ultimate-cheese-board/"),
  androuet  = c("Androuet: How to Recognize Every Cheese Family", "https://androuet.com/en/how-to-recognize-every-cheese-family/"),
  dupleix   = c("Jill Dupleix: How to build a cheese board", "https://jilldupleix.substack.com/p/how-to-build-a-cheese-board")
)

# Video titles and channels confirmed through YouTube's own oEmbed data.
LEARN_VIDEOS <- list(
  c("How to Build a Great Cheese Board", "Food52 (Dear Test Kitchen)", "https://www.youtube.com/watch?v=KGfKTH6h67w"),
  c("How to Make the Ultimate Cheese Board", "Bon Appétit", "https://www.youtube.com/watch?v=WxMSWsauE-c"),
  c("How to Build the Perfect Cheese Board (Process, Tips, Description)", "The Local Palate", "https://www.youtube.com/watch?v=WKlH9EMPHbQ"),
  c("Wine & Cheese Pairing Secrets From a Sommelier", "VinePair", "https://www.youtube.com/watch?v=yJN7VsXCfqc"),
  c("How to Pair Wine & Cheese Like a Pro with 3 Sommeliers & Cypress Grove Cheese", "SOMMVIVANT", "https://www.youtube.com/watch?v=zyhvztkSqpc"),
  c("Wine and Cheese Pairing Tips | Best Cheeses with Cabernet Sauvignon", "Jordan Vineyard & Winery", "https://www.youtube.com/watch?v=-tct-xGGTNs")
)

cite <- function(...) {
  ids <- c(...)
  tags$span(class = "cites", lapply(ids, function(id) {
    s <- LEARN_SOURCES[[id]]
    tags$a(class = "cite", href = s[2], target = "_blank", rel = "noopener", title = s[1], sub(":.*", "", s[1]))
  }))
}
lp <- function(..., src = NULL) tags$p(..., if (!is.null(src)) cite(src))
li <- function(..., src = NULL) tags$li(..., if (!is.null(src)) cite(src))

learn_ui <- function() {
  div(
    class = "learn py-3 px-3",
    div(
      class = "learn-wrap",
      div(class = "learn-toc",
          div(class = "eyebrow", "On this page"),
          tags$ol(
            tags$li(tags$a(href = "#l-forces", "The forces behind a pairing")),
            tags$li(tags$a(href = "#l-science", "What the research says")),
            tags$li(tags$a(href = "#l-color", "Red or white?")),
            tags$li(tags$a(href = "#l-families", "Cheese families and what to pour")),
            tags$li(tags$a(href = "#l-regional", "Classic regional pairings")),
            tags$li(tags$a(href = "#l-board", "Building the board, step by step")),
            tags$li(tags$a(href = "#l-extras", "Choosing accompaniments")),
            tags$li(tags$a(href = "#l-serve", "Serving temperatures")),
            tags$li(tags$a(href = "#l-mistakes", "Common mistakes")),
            tags$li(tags$a(href = "#l-more", "Watch and read more")))),

      div(
        class = "learn-body",
        h2("The thinking behind wine and cheese"),
        lp(class = "lede", "A good pairing is a balancing act between a few things you can taste: how strong each one is, acidity, tannin, fat, salt and sweetness. This page explains each one, what the research found, and how cheesemongers build a board. Each claim links to its source. Where sources disagree, we say so."),

        h3(id = "l-forces", "1. The forces behind a pairing"),
        div(class = "force",
            h4("Match intensity first"),
            lp("Pair delicate with delicate and bold with bold. Wine Folly's example: a big, bold Cabernet Sauvignon overwhelms the delicate flavor of Gruyère, while Pinot Noir suits it. As a rough guide they suggest wines above 14.5% alcohol for intensely flavored cheeses and wines under 12% for delicate ones.", src = "wf6"),
            lp("Wine Folly calls pairing wine and cheese of equal intensity the most important takeaway.", src = "wfideas")),
        div(class = "force",
            h4("Acidity refreshes"),
            lp("Acid in wine cuts through the fat in cheese and refreshes your palate between bites. Gary Farrell gives this as one reason Pinot Noir, with its higher acidity, works with so many cheeses.", src = c("culture", "gf")),
            lp("Bubbles add to the effect. Wine Folly recommends sparkling wine with soft, creamy cheeses such as Brie and Camembert because the acidity and bubbles cleanse the palate after rich, sticky cheese.", src = "wf6")),
        div(class = "force",
            h4("Fat softens tannin, and salt sharpens it"),
            lp("Tannin is the drying, grippy feeling in red wine. Fat and protein in cheese soften it. That is why Wine Folly pairs bold reds with aged cheeses: aging removes moisture and concentrates fat. They suggest cheeses aged at least a year, such as cheddar, Gruyère, Manchego, gouda and Parmigiano-Reggiano.", src = "wf6"),
            lp("Salt works the other way. culture notes that tannins tend to be exaggerated by salt, which cheese has plenty of. A very salty cheese with a very tannic wine can taste harsh.", src = "culture")),
        div(class = "force",
            h4("Sweetness balances salt and funk"),
            lp("Sweet wine with salty, pungent cheese is a classic contrast. Wine Folly explains that sweetness softens the funk of stinky, washed-rind and blue cheeses and makes them taste creamier, while the cheese's pungency offsets the sweetness. Their examples are Port with Stilton and Sauternes with Roquefort.", src = "wf6"),
            lp("culture adds that residual sugar in a wine can smooth over the rough edges that make some wine-and-cheese pairings difficult.", src = "culture")),
        div(class = "force",
            h4("Alcohol"),
            lp("Salt and bitterness can make a wine's alcohol seem stronger. culture's fix is a cheese with plenty of fat to tame it.", src = "culture")),

        h3(id = "l-science", "2. What the research says"),
        div(class = "study",
            h4("2016: cheese made red wine less astringent"),
            lp("Sensory scientist Mara Galmarini and colleagues worked at the Centre des Sciences du Goût et de l'Alimentation in Dijon, France, and published in the Journal of Food Science. 31 regular wine and cheese consumers tasted four wines: a sweet white Pacherenc, a dry white Sancerre, a full-bodied red Burgundy and a tannic red Madiran. Each wine was tasted with and without four cheeses: Époisses, Comté, Roquefort and Crottin de Chavignol.", src = "npr"),
            lp("Eating cheese between sips shortened how long sourness and astringency lingered, and tasters liked the wines the same or more. The tannic Madiran, the least liked wine on its own, improved most. Galmarini's explanation is that fat from the cheese coats the mouth.", src = "npr")),
        div(class = "study",
            h4("2006: cheese muted red wine flavors"),
            lp("An earlier UC Davis study by Berenice Madrigal-Galan and Hildegarde Heymann found that cheese suppressed almost all red wine flavors, including berry, oak, sourness and astringency. The one aroma that increased was butter, which cheese itself contains. Stronger cheeses masked the wine more than milder ones. The study was not repeated with white or sweet wines.", src = c("ucd", "decanter"))),
        div(class = "takeaway",
            tags$b("What this means for you. "),
            "Cheese makes a tannic red feel smoother, which is why the pairing feels good, but it can also mute the wine's fruit. Strong cheeses mute more. That is one more reason to match intensity: a bold cheese needs a bold wine to still taste the wine."),

        h3(id = "l-color", "3. Red or white?"),
        lp("Whites usually pair with a wider range of cheeses than reds because they have no tannin. Blue cheese is the main exception: it tends to overwhelm most whites.", src = "wfideas"),
        lp("Light reds still have a place. Wine Folly matches Pinot Noir and Beaujolais with nutty, medium-firm cheeses such as Gruyère and Comté, with Brie and Camembert, and with Taleggio.", src = "wfideas"),
        lp("The Galmarini team's unpublished follow-up suggests serving white wine when you offer many cheeses.", src = "npr"),
        lp("If you need one cheese that works with several wines, Wine Folly's advice is a firm, nutty cheese such as Gruyère, Comté, Emmental or gouda: its fat handles red wine tannin and its mild flavor does not overwhelm delicate whites.", src = "wf6"),

        h3(id = "l-families", "4. Cheese families and what to pour"),
        lp("Cheesemongers group cheese by how it is made. Knowing the family tells you a lot about texture, strength and which wines will work."),
        div(class = "fam-table", tags$table(
          tags$thead(tags$tr(tags$th("Family"), tags$th("What it is"), tags$th("Examples"), tags$th("What to pour"))),
          tags$tbody(
            tags$tr(tags$td(tags$b("Fresh")),
                    tags$td("Unaged. Flavor depends mostly on salt and how much whey was drained.", cite("androuet")),
                    tags$td("Fresh chèvre, mozzarella, burrata"),
                    tags$td("Sauvignon Blanc with goat cheese is a regional classic from the Loire.", cite("wf6"), " Wisconsin Cheese advises against creamy and goat cheeses with a dark-fruited red like Malbec.", cite("wisc"))),
            tags$tr(tags$td(tags$b("Bloomy rind")),
                    tags$td("A white, edible rind of Penicillium candidum. The cheese softens from the outside in as it ripens.", cite("androuet")),
                    tags$td("Brie, Camembert, triple crème"),
                    tags$td("Sparkling wine", cite("wf6"), "; Chardonnay with Brie and Délice de Bourgogne", cite("wfideas"), "; Pinot Noir or Beaujolais", cite("wfideas", "gf"))),
            tags$tr(tags$td(tags$b("Washed rind")),
                    tags$td("The rind is washed in brine, wine or beer, which encourages B. linens bacteria. Often an orange rind and a strong smell, usually milder in flavor than the smell suggests.", cite("androuet")),
                    tags$td("Époisses, Taleggio"),
                    tags$td("Chardonnay with Époisses; Pinot Noir or Beaujolais with Taleggio", cite("wfideas"), "; sweet wines with funky cheeses", cite("wf6"))),
            tags$tr(tags$td(tags$b("Semi-firm and nutty")),
                    tags$td("Alpine-style cheeses with a dense, smooth texture and nutty flavor."),
                    tags$td("Gruyère, Comté, Fontina, Raclette"),
                    tags$td("Pinot Noir", cite("wf6", "wfideas", "gf"), "; off-dry Riesling with fondue", cite("wfideas"), ". The safest choice for mixed wines.", cite("wf6"))),
            tags$tr(tags$td(tags$b("Hard and aged")),
                    tags$td("Lower moisture and concentrated fat and flavor from long aging.", cite("wf6")),
                    tags$td("Aged cheddar, aged gouda, Manchego, Parmigiano-Reggiano"),
                    tags$td("Bold reds", cite("wf6"), ": Cabernet with aged cheddar, Garnacha with Manchego, Shiraz with smoked gouda", cite("wfideas"), "; Malbec with aged cheddar and gouda", cite("wisc", "murraystt"))),
            tags$tr(tags$td(tags$b("Blue")),
                    tags$td("Veined with Penicillium roqueforti. Wheels are pierced so air reaches the mold and the veins develop.", cite("androuet")),
                    tags$td("Roquefort, Stilton, Gorgonzola"),
                    tags$td("Sweet wines: Port with Stilton, Sauternes with Roquefort", cite("wf6"), ". Overwhelms most whites", cite("wfideas"), ". Malbec's fruit can work with milder blues", cite("wisc")))
          ))),
        div(class = "takeaway", tags$b("At Trader Joe's. "),
            "When this app checked traderjoes.com, there was no Port or dessert wine listed, so the classic sweet partners for blue cheese were not available. The app's blue cheese suggestions are milder options with a ripe red, and it says so on each card."),

        h3(id = "l-regional", "5. Classic regional pairings"),
        lp("A reliable shortcut is to pair wine and cheese from the same place, since local food and wine traditions grew up together.", src = "wf6"),
        tags$ul(class = "facts",
                li(tags$b("Sauvignon Blanc and goat cheese"), " from France's Loire Valley, such as Crottin de Chavignol.", src = c("wf6", "wfideas")),
                li(tags$b("Chardonnay and Époisses"), " from Burgundy.", src = "wf6"),
                li(tags$b("Garnacha and Manchego"), " from Spain.", src = "wf6"),
                li(tags$b("Port and Stilton"), ", and ", tags$b("Sauternes and Roquefort"), ", the classic sweet-and-salty matches.", src = "wf6"),
                li(tags$b("Barolo and Bra Duro"), ": a firm but not hard cheese softens Barolo's strong tannin and acidity.", src = "wfideas")),

        h3(id = "l-board", "6. Building the board, step by step"),
        h4("How many cheeses"),
        lp("Three to five is a good range; Murray's Cheese suggests that number.", src = "murraysmc"),
        lp("For a crowd, Adam Goddu of Murray's suggests five cheeses and two drizzles, such as honey.", src = "murraysdm"),
        lp("Tasting Table's expert advises not going above five.", src = "ttips"),
        h4("How much to buy"),
        lp("Published guidance varies a lot, so use a range:"),
        tags$ul(class = "facts",
                li("Murray's Cheese: about 1 oz of each cheese per person.", src = "murraysmc"),
                li("Castello: 1 to 1.75 oz per person as a starter at a dinner party, 2.5 to 4.5 oz for lunch, and 7 to 8 oz when the board is the meal.", src = "castello")),
        lp("This app's planner uses 2.5 oz per person for an appetizer and 5 oz for a main event, which sits in the middle of these ranges. Leftover cheese keeps if you wrap it well (see below)."),
        h4("Choosing a variety"),
        lp("Mix milk types (cow, goat, sheep), textures and colors, and pair something familiar such as cheddar with something less common.", src = "murraysdm"),
        lp("At the grocery store, firm cheeses such as cheddar or Gruyère are the easy picks. If you choose a soft cheese, check that it has at least seven days before its expiration date.", src = "murraysdm"),
        h4("Laying it out"),
        tags$ol(class = "facts steps",
                li("Take the cheese out of the fridge about an hour before guests arrive. It cuts more cleanly and tastes better warmer.", src = c("murraysdm", "sheridans")),
                li("Place the larger cheeses first and treat the board like a landscape.", src = "dupleix"),
                li("Arrange from mildest to strongest so guests taste in that order. Murray's suggests going clockwise from 12 o'clock.", src = "murraysmc"),
                li("Give each cheese its own knife, and cut a few pieces ahead to get people started.", src = "murraysmc"),
                li("Put spreads in a bowl with a spoon. Place honey or chutney between the cheeses that suit it; Murray's puts blue and goat cheese on either side of the honey.", src = "murraysdm"),
                li("Fill the gaps between cheeses with the extras: crackers, fruit, nuts.", src = "murraysdm")),
        h4("Leftovers"),
        lp("Wrap leftover cheese in wax paper, parchment or cheese paper. Sealed plastic bags or glass containers can trap ammonia from the cheese and make it taste bad.", src = "murraysdm"),

        h3(id = "l-extras", "7. Choosing accompaniments"),
        lp("Murray's Cheese's formula for every bite is base + cheese + other: a cracker or bread, a piece of cheese, and a topping.", src = "murraysdm"),
        tags$ul(class = "facts",
                li(tags$b("Sweet with blue and funky cheese. "), "Murray's examples include blue cheese on a high-cocoa chocolate square with a drizzle of honey. This follows the same sweet-and-salty logic as Port with Stilton.", src = c("murraysdm", "wf6")),
                li(tags$b("Pickled with washed-rind cheese. "), "Murray's pairs pickled vegetables (their example is pickled Brussels sprouts) with washed-rind cheese.", src = "murraysdm"),
                li(tags$b("Crunch. "), "Fried, salted Marcona almonds add crunch.", src = "murraysdm"),
                li(tags$b("Cured meat with red wine. "), "For Malbec, Murray's John Montez suggests a dry, nutty cured ham such as jamón serrano as a contrast to the wine's fruit.", src = "murraystt"),
                li(tags$b("Keep sweet things in check with dry wine. "), "Wine Folly's food-pairing rule is that the wine should be at least as sweet as the food; otherwise it tends to taste bitter and tart. With a dry sparkling or crisp white, use honey and jam sparingly.", src = "wfsweet")),
        div(class = "takeaway", tags$b("How the app's suggestions work. "),
            "The star suggestions in step 5 combine cheese-specific ideas with wine-specific ones. Where a suggestion comes straight from a source above (Marcona almonds, honey with blue, cured ham with Malbec, sweetness with dry wine), it follows that source. The rest are general pairing logic and good starting points, not rules."),

        h3(id = "l-serve", "8. Serving temperatures"),
        lp("Wine Folly's serving bands:", src = "wftemp"),
        div(class = "fam-table", tags$table(
          tags$thead(tags$tr(tags$th("Wine"), tags$th("Temperature"))),
          tags$tbody(
            tags$tr(tags$td("Sparkling and light-bodied whites"), tags$td("38 to 45 °F (ice cold)")),
            tags$tr(tags$td("Rosé and full-bodied whites"), tags$td("44 to 55 °F (fridge cold)")),
            tags$tr(tags$td("Light and medium-bodied reds"), tags$td("55 to 60 °F (cool)")),
            tags$tr(tags$td("Bold reds"), tags$td("60 to 68 °F (slightly cool)"))))),
        lp("For cheese, Sheridans Cheesemongers suggests taking it out about an hour before serving, a little longer for harder cheeses. Bord Bia suggests about 30 minutes for soft cheeses and 1 to 2 hours for firm ones, depending on the room.", src = c("sheridans", "bordbia")),

        h3(id = "l-mistakes", "9. Common mistakes"),
        tags$ul(class = "facts",
                li(tags$b("A big red with a delicate cheese. "), "The wine overwhelms it, like Cabernet with Gruyère.", src = "wf6"),
                li(tags$b("A very tannic red with a very salty cheese. "), "Salt exaggerates tannin.", src = "culture"),
                li(tags$b("Blue cheese with a light white. "), "Blue tends to overwhelm most whites.", src = "wfideas"),
                li(tags$b("Sweet spreads with a dry wine. "), "The wine tastes bitter and tart if it is less sweet than the food.", src = "wfsweet"),
                li(tags$b("Serving cheese straight from the fridge. "), "Give it time to warm up so the flavor comes through.", src = c("sheridans", "murraysdm")),
                li(tags$b("Too many cheeses. "), "Stick to three to five.", src = c("murraysmc", "ttips"))),

        h3(id = "l-more", "10. Watch and read more"),
        h4("Videos"),
        tags$ul(class = "facts", lapply(LEARN_VIDEOS, function(v) tags$li(tags$a(href = v[3], target = "_blank", rel = "noopener", v[1]), " · ", v[2]))),
        h4("All sources"),
        tags$ul(class = "facts", lapply(LEARN_SOURCES, function(s) tags$li(tags$a(href = s[2], target = "_blank", rel = "noopener", s[1])))),
        p(class = "note", "Pairing is personal. These are well-tested starting points from educators, cheesemongers and sensory research. Taste as you go and trust your own palate.")
      )
    )
  )
}
