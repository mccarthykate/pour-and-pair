# Suggestions for a second wine (or cheese) once you have picked one.
# "Same partners": another style that goes with the same cheeses (or wines), so guests can compare.
# "Round out": a different style that covers partners your picks don't, ideally from another group
# (a red after a sparkling, a firm cheese after a soft one).

STYLE_GROUP <- list(
  wine = c(cremant = "sparkling wine", prosecco = "sparkling wine", sauv = "white", lightwhite = "white", chard = "white",
           aromatic = "white", rose = "rosé", pinot = "red", malbec = "red", cab = "red", redblend = "red"),
  cheese = c(brie = "soft cheese", goat = "fresh cheese", fresh = "fresh cheese", feta = "fresh cheese", stilton = "crumbly cheese",
             sheep = "firm cheese", alpine = "firm cheese", toscano = "firm cheese", cheddar = "firm cheese", gouda = "firm cheese",
             parm = "hard cheese", blue = "blue cheese")
)
other_kind <- function(kind) if (kind == "wine") "cheese" else "wine"

# partner styles rated Good or Classic, named by style id
partners <- function(kind, style) {
  r <- PAIRINGS[PAIRINGS[[kind]] == style & PAIRINGS$rating >= 2, ]
  setNames(r$rating, r[[other_kind(kind)]])
}

companions <- function(kind, picked_styles) {
  S <- unique(picked_styles)
  if (!length(S)) return(NULL)
  ids <- if (kind == "wine") WINE_STYLES$id else CHEESE_STYLES$id
  cand <- setdiff(ids, S)
  if (!length(cand)) return(NULL)
  covered <- unique(unlist(lapply(S, function(s) names(partners(kind, s)))))
  groups <- STYLE_GROUP[[kind]]

  shared <- vapply(cand, function(c) {
    p <- partners(kind, c); hit <- intersect(names(p), covered)
    length(hit) + .1 * sum(p[hit] == 3)
  }, 0)
  fresh <- vapply(cand, function(c) {
    p <- partners(kind, c)
    length(setdiff(names(p), covered)) + .5 * !(groups[c] %in% groups[S]) + .05 * sum(p == 3)
  }, 0)

  out <- list()
  if (max(shared) > 0) {
    s <- names(which.max(shared))
    hit <- intersect(names(partners(kind, s)), covered)
    out$same <- list(style = s, partners = hit)
  }
  ranked <- setdiff(names(sort(fresh, decreasing = TRUE)), if (!is.null(out$same)) out$same$style)
  if (length(ranked) && fresh[ranked[1]] > 0) {
    r <- ranked[1]
    out$round <- list(style = r, partners = setdiff(names(partners(kind, r)), covered),
                      new_group = !(groups[r] %in% groups[S]), group = unname(groups[r]))
  }
  out
}

# Rating of a wine style with a cheese style: 3 classic, 2 good, 0 skip, NA not rated
pair_rating <- function(ws, cs) {
  r <- PAIRINGS$rating[PAIRINGS$wine == ws & PAIRINGS$cheese == cs]
  if (length(r)) r[1] else NA_integer_
}
