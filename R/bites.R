# "Perfect bites": base + cheese + topping (Murray's Cheese's formula), built around
# one cheese so the bite also suits the wine it is paired with.

# Two good bases for each cheese type, best first, and why that base suits it
BITE_BASES <- list(
  brie    = c("Organic French Baguette", "Pane Guttiau Paper Thin Sardinian Crackers"),
  goat    = c("Organic French Baguette", "Fig & Olive Crisps"),
  fresh   = c("Ciabatta Baguette", "Organic French Baguette"),
  feta    = c("Pita Bite Crackers", "Ciabatta Baguette"),
  sheep   = c("Organic French Baguette", "Wheat Crisp Crackers"),
  alpine  = c("Sourdough Boules", "Wheat Crisp Crackers"),
  toscano = c("Wheat Crisp Crackers", "Organic French Baguette"),
  cheddar = c("Wheat Crisp Crackers", "Pretzel Slims"),
  gouda   = c("Wheat Crisp Crackers", "Golden Rounds Crackers"),
  parm    = c("Pane Guttiau Paper Thin Sardinian Crackers", "Organic French Baguette"),
  blue    = c("Wheat Crisp Crackers", "Raisin Rosemary Crisps"),
  stilton = c("Raisin Rosemary Crisps", "Wheat Crisp Crackers")
)
BASE_WHY <- c(
  brie = "Crusty bread holds a soft, spreadable cheese.", goat = "A sturdy base for a soft, tangy cheese.",
  fresh = "Bread soaks up the milky cheese.", feta = "A sturdy base for a crumbly, salty cheese.",
  sheep = "A neutral base lets the nutty cheese lead.", alpine = "Tangy bread cuts the richness of a dense cheese.",
  toscano = "A plain base won't bury the rind's flavor.", cheddar = "Crunch for a sharp, crumbly cheese.",
  gouda = "A plain base; the crystals already add crunch.", parm = "A thin crisp for a hard, salty cheese.",
  blue = "A plain base, since blue is strong enough.", stilton = "Rosemary and raisin suit a fruity cheese."
)

# cs/ws: cheese and wine style ids (ws may be NA); onboard: names of items already picked
perfect_bites <- function(cs, ws = NA, onboard = character()) {
  active <- ADDONS$name
  carriers <- ADDONS$name[ADDONS$role == "carrier"]
  bases <- intersect(BITE_BASES[[cs]], active)
  bases <- unique(c(intersect(onboard, BITE_BASES[[cs]]), bases, intersect(onboard, carriers)))
  if (!length(bases)) bases <- head(carriers, 2)
  if (!length(bases)) return(NULL)

  csug <- SUGGESTIONS[[cs]]; wsug <- if (is.na(ws)) character() else WINE_LEANIN[[ws]]
  cand <- setdiff(intersect(unique(c(names(csug), names(wsug))), active), carriers)
  if (!is.na(ws)) {
    wt <- WINE_TRAITS[[ws]]
    drop <- c(if (wt$dry_caution) ADDONS$name[ADDONS$role == "sweet"], if (wt$tannic) HEAT_ITEMS)
    kept <- setdiff(cand, drop)
    if (length(kept) >= 2) cand <- kept
  }
  if (!length(cand)) return(NULL)
  score <- vapply(cand, function(n) 3 * (n %in% onboard) + (n %in% names(csug)) + (n %in% names(wsug)) + (n %in% names(csug) && n %in% names(wsug)), 0)
  cand <- cand[order(-score)]
  roles <- ADDONS$role[match(cand, ADDONS$name)]

  # pick up to three toppings, each doing a different job where possible
  picks <- character()
  for (i in seq_along(cand)) if (!roles[i] %in% ADDONS$role[match(picks, ADDONS$name)]) picks <- c(picks, cand[i])
  picks <- head(unique(c(picks, cand)), 3)

  do.call(rbind, lapply(seq_along(picks), function(i) {
    top <- picks[i]
    why <- c(if (top %in% names(csug)) csug[[top]], if (top %in% names(wsug)) wsug[[top]])
    base <- bases[(i - 1) %% length(bases) + 1]
    data.frame(base = base, top = top,
               why = paste(c(why, if (identical(base, BITE_BASES[[cs]][1])) BASE_WHY[[cs]]), collapse = " "),
               stringsAsFactors = FALSE)
  }))
}
