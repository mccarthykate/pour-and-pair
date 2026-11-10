# Illustrations for the mini cheese board, drawn as inline SVG.
# Items are drawn the way they really sit on a board (crackers shingled, apples
# fanned, grapes in a bunch, salami in a river, cheese cut with a knife beside it),
# shaded with gradients so they read as real food. board_svg() sizes everything
# to how full the board is: a sparse board gets big, generous portions, and items
# shrink as the board fills.

# ---- helpers ----------------------------------------------------------------
.n <- function(x) format(round(x, 2), nsmall = 1, trim = TRUE)
.many <- function(x) paste(unlist(x), collapse = "")
.g <- function(inner, x = 0, y = 0, s = 1, rot = 0, extra = "") {
  paste0('<g transform="translate(', .n(x), ' ', .n(y), ') rotate(', .n(rot), ') scale(', .n(s), ')"', extra, '>', .many(inner), '</g>')
}
.c <- function(x, y, r, fill, extra = "") .many(sprintf('<circle cx="%s" cy="%s" r="%s" fill="%s"%s/>', .n(x), .n(y), .n(r), fill, extra))
.e <- function(x, y, rx, ry, fill, rot = 0, extra = "") {
  .many(sprintf('<ellipse cx="%s" cy="%s" rx="%s" ry="%s" fill="%s" transform="rotate(%s %s %s)"%s/>',
                .n(x), .n(y), .n(rx), .n(ry), fill, .n(rot), .n(x), .n(y), extra))
}
.p <- function(d, fill, extra = "") paste0('<path d="', d, '" fill="', fill, '"', extra, '/>')
.dflt <- function(x, d) if (length(x) == 0 || is.na(x) || !nzchar(x)) d else x
.shade <- function(col, f) {
  m <- grDevices::col2rgb(col)[, 1] / 255
  m <- if (f < 0) m * (1 + f) else m + (1 - m) * f
  grDevices::rgb(m[1], m[2], m[3])
}
.stroke <- function(col, w = .6, f = -.3) sprintf(' stroke="%s" stroke-width="%s"', .shade(col, f), w)
.hl <- function(x, y, rx, ry, rot = -30, op = .5) .e(x, y, rx, ry, "#ffffff", rot, sprintf(' fill-opacity="%s"', op))
SH <- ' filter="url(#ds)"'

# Gradients are registered while a board is drawn and written into <defs>.
.defs <- new.env()
.reset_defs <- function() .defs$list <- character()
.reg <- function(id, def) { if (!id %in% names(.defs$list)) .defs$list[id] <- def; sprintf("url(#%s)", id) }
.key <- function(...) gsub("[^0-9a-zA-Z]", "", paste(..., sep = "_"))
# radial: lit from the upper left, the way a round fruit or olive catches light
.rg <- function(col, hi = .5, lo = -.35) {
  id <- .key("rg", col, hi, lo)
  .reg(id, sprintf('<radialGradient id="%s" cx=".36" cy=".3" r=".8"><stop offset="0" stop-color="%s"/><stop offset=".5" stop-color="%s"/><stop offset="1" stop-color="%s"/></radialGradient>',
                   id, .shade(col, hi), col, .shade(col, lo)))
}
# linear: lighter at the top, for flat faces such as crackers and cheese
.lg <- function(col, hi = .18, lo = -.12, angle = 90) {
  id <- .key("lg", col, hi, lo, angle)
  x2 <- round(cos(angle * pi / 180), 2); y2 <- round(sin(angle * pi / 180), 2)
  .reg(id, sprintf('<linearGradient id="%s" x1="0" y1="0" x2="%s" y2="%s"><stop offset="0" stop-color="%s"/><stop offset="1" stop-color="%s"/></linearGradient>',
                   id, abs(x2), abs(y2), .shade(col, hi), .shade(col, lo)))
}
# a "pool" gradient: pale at the edge, rich in the middle (fig flesh, jam)
.pool <- function(inner, outer) {
  id <- .key("pool", inner, outer)
  .reg(id, sprintf('<radialGradient id="%s" cx=".5" cy=".55" r=".6"><stop offset="0" stop-color="%s"/><stop offset="1" stop-color="%s"/></radialGradient>', id, inner, outer))
}

# ---- crackers and bread -------------------------------------------------------
.scallop <- function(r, bumps = 14, depth = 1.1) {
  t <- seq(0, 2 * pi, length.out = bumps * 8 + 1)
  rr <- r + depth * abs(sin(t * bumps / 2))
  paste0("M", paste(.n(rr * cos(t)), .n(rr * sin(t)), collapse = " L"), "Z")
}
.cracker <- function(shape, a, b, f) {
  edge <- .shade(a, -.22)
  dock <- function(xs, ys) paste0(.c(xs, ys, .95, .shade(a, -.38)), .c(xs - .35, ys - .35, .35, .shade(a, .5)))
  seeds <- if (nzchar(b)) paste0(.e(c(-6, 3, 7, -2, 5, -8, 1, 8), c(-2, -7, 3, 6, 6, 5, 0, -6), 1.3, .55, b, c(20, -40, 60, 10, -20, 70, 40, -60)),
                                 .c(c(-4, 6, -7, 2), c(-7, -1, 2, 3), .6, "#2a2420")) else ""
  switch(shape,
    sq = paste0('<rect x="-11.5" y="-11.5" width="23" height="23" rx="2.6" fill="', .lg(a), '"', .stroke(a, .7, -.25), '/>',
                '<rect x="-9.5" y="-9.5" width="19" height="19" rx="2" fill="none" stroke="', edge, '" stroke-opacity=".35" stroke-width="1.6"/>',
                dock(c(-5, 0, 5, -5, 0, 5, -5, 0, 5), c(-5, -5, -5, 0, 0, 0, 5, 5, 5)), seeds),
    rd = paste0(.p(if (f == "scallop") .scallop(11) else "M-12 0 A12 12 0 1 0 12 0 A12 12 0 1 0 -12 0Z", .rg(a, .25, -.15), .stroke(a, .6, -.25)),
                .c(0, 0, 8.6, "none", sprintf(' stroke="%s" stroke-opacity=".35" stroke-width="1.4"', edge)),
                dock(c(-4, 4, 0, -4, 4), c(-3.5, -3.5, 0, 3.5, 3.5))),
    rect = if (f == "pretzel") {
      paste0('<rect x="-6.5" y="-16" width="13" height="32" rx="3" fill="', .lg(a, .25, -.2, 0), '"', .stroke(a, .6, -.3), '/>',
             '<rect x="-4.5" y="-14" width="3" height="28" rx="1.5" fill="#ffffff" fill-opacity=".18"/>',
             .e(c(-2, 2, -3, 3, 0, -1, 2), c(-11, -6, -1, 4, 9, 13, -2), .9, .6, b, c(20, -30, 10, 60, -10, 40, 0)))
    } else {
      bits <- paste0(.e(c(-3, 3, -2, 3.5, -3.5, 1.5, 0), c(-11, -6, -1, 3, 8, 11, -14), c(1.8, 1.4, 1.6, 1.3, 1.5, 1.2, 1.1), 1.2, .dflt(b, .shade(a, -.45)), c(20, -40, 60, 10, -20, 70, 40)),
                     .e(c(2, -2.5, 3, -1), c(-9, 4, 9, -4), 1.3, .9, "#f3e3c0", c(30, -10, 50, 0)))
      paste0('<path d="M-6.6 -16.2 L6.4 -15.6 L7 16 L-6.8 16.4Z" fill="', .lg(a, .15, -.15, 0), '"', .stroke(a, .6, -.25), '/>', bits)
    },
    bread = paste0(.e(0, 0, 9.4, 13.6, .rg("#a8642a", .25, -.3)), .e(0, 0, 7.6, 11.6, .pool(.shade(a, .25), a)),
                   .e(c(-2.4, 2.2, -1, 3, -.5, -3, 1.5), c(-6, -3, 2.6, 6, -.4, 5, -8.4), c(1.4, 1.1, 1.5, .9, .8, .9, .8), c(1, .8, 1.1, .7, .6, .7, .5), .shade(a, -.2), c(10, -30, 20, 50, 0, 40, -10))),
    boule = paste0('<path d="M-17 7 C-18 -8 -9 -16 0 -16 C9 -16 18 -8 17 7 Z" fill="', .rg("#8e4c1c", .2, -.3), '"/>',
                   .c(c(-12, -6, 2, 9, 13), c(-6, -12, -14, -11, -4), .7, "#f6efe2", ' fill-opacity=".75"'),
                   '<path d="M-14.4 6 C-15 -6 -7.8 -13.2 0 -13.2 C7.8 -13.2 15 -6 14.4 6 Z" fill="', .pool(.shade(a, .3), a), '"/>',
                   .e(c(-7, 2, 7, -2, 6, -8, 1, 10), c(-4, -8, -2, 1, 3, 2, -3, 1), c(1.8, 1.2, 1.6, 1.3, 1, 1.1, .9, .8), c(1.2, .9, 1, .9, .7, .8, .6, .6), .shade(a, -.2), c(10, -20, 30, 0, 40, -10, 20, 0)))
  )
}

# ---- one shape function per item kind; each takes (color, color2, variant) ----
ITEM_SHAPES <- list(
  sq = function(a, b, f) .many(lapply(0:4, function(i) .g(.cracker("sq", a, b, f), -20 + i * 10, i * .8 - 1, 1, -7 + i * 3))),
  rd = function(a, b, f) .many(lapply(0:4, function(i) .g(.cracker("rd", a, b, f), -20 + i * 10, -abs(i - 2) * 2, 1))),
  rect = function(a, b, f) .many(lapply(0:5, function(i) .g(.cracker("rect", a, b, f), -21 + i * 8.4, 0, 1, -16 + i * 6.4))),
  bread = function(a, b, f) .many(lapply(0:5, function(i) .g(.cracker("bread", a, b, f), -26 + i * 10.4, -abs(i - 2.5) * 2.4, 1, -12 + i * 4.6))),
  boule = function(a, b, f) .many(lapply(0:2, function(i) .g(.cracker("boule", a, b, f), -15 + i * 14, i * 2.4, 1, -14 + i * 13))),

  # ---- fruit & vegetables ----------------------------------------------------
  grape = function(a, b, f) {
    bunch <- function(col) {
      rows <- c(4, 5, 4, 4, 3, 2, 1)
      g <- unlist(lapply(seq_along(rows), function(r) {
        n <- rows[r]
        vapply(seq_len(n), function(j) {
          x <- (j - (n + 1) / 2) * 8.2 + ifelse(r %% 2 == 0, 1.3, -1.3); y <- (r - 1) * 6.9 - 18
          paste0(.e(x, y, 5.2, 5.5, .rg(col, .45, -.4), (j * 23) %% 30 - 15), .hl(x - 1.7, y - 2.1, 1.7, 1.05, -30, .7))
        }, "")
      }))
      c('<path d="M0 -31 C1 -27 0 -24 -1 -21 M0 -26 C5 -28 9 -25 11 -21 M0 -26 C-5 -27 -9 -24 -10 -20" fill="none" stroke="#7a5a32" stroke-width="1.5" stroke-linecap="round"/>', g)
    }
    if (nzchar(b)) paste0(.g(bunch(a), -12, 2, .86, -12), .g(bunch(b), 14, 6, .86, 14)) else .many(bunch(a))
  },
  apple = function(a, b, f) {
    skin <- .lg(a, .15, -.25, 0)
    slice <- paste0('<path d="M-15 2 A15 8.5 0 0 1 15 2 Q0 -.4 -15 2Z" fill="', .pool("#fbf4d8", "#efe1b0"), '"', .stroke("#e7d9a8", .4, -.1), '/>',
                    '<path d="M-15 2 A15 8.5 0 0 1 15 2" fill="none" stroke="', skin, '" stroke-width="2.6" stroke-linecap="round"/>',
                    '<path d="M-6 .9 Q0 -2.6 6 .9" fill="none" stroke="#e2cf96" stroke-width=".7"/>',
                    .e(c(-2.3, 2.3), c(-.7, -.7), .75, 1.4, "#5a3016"))
    .many(lapply(0:5, function(i) .g(slice, -20 + i * 8, 4 - abs(i - 2.5) * 2.4, 1, 70 + i * 5)))
  },
  fig = function(a, b, f) {
    FIG <- "M0 -14 C4 -14 5 -9 8 -5 C12 0 12 8 7 12 C4 14 -4 14 -7 12 C-12 8 -12 0 -8 -5 C-5 -9 -4 -14 0 -14Z"
    seeds <- .e(c(-3, 2, 3, -2, 0, 4, -4, 1, -1, 3), c(-1, -3, 2, 4, 1, 6, 6, -6, 7, -1), .55, .9, "#fae6c8", c(10, -30, 40, 0, 20, -10, 30, 0, 15, 60))
    cut <- paste0(.p(FIG, .rg(a, .2, -.35)), '<g transform="scale(.82) translate(0 1.4)">', .p(FIG, .pool("#e9887f", "#f4dcc0")), '</g>',
                  '<g transform="scale(.5) translate(0 3)">', .p(FIG, .pool("#9e1f3c", "#d4566a")), '</g>', seeds)
    whole <- paste0(.p(FIG, .rg(a, .3, -.4)), '<rect x="-1.4" y="-18.5" width="2.8" height="5.5" rx="1.4" fill="', .shade(a, -.35), '"/>')
    paste0(.g(whole, 15, -7, .95, 20), .g(cut, -12, 0, 1, -20), .g(cut, 4, 9, 1, 15))
  },
  berry = function(a, b, f) {
    BERRY <- "M0 13 C-12 4 -11 -8 0 -7 C11 -8 12 4 0 13Z"
    seeds <- .e(c(-4, 3, 0, -3, 4, 0, -6, 6, 2, -2), c(-2, -3, 2, 6, 4, 9, 2, 1, -5, -4), .55, .85, "#f2d06a", c(10, -10, 0, 20, -20, 0, 30, -30, 0, 10))
    whole <- paste0(.p(BERRY, .rg(a, .35, -.35)), seeds,
                    '<path d="M-8 -8 L-2 -5 L-1 -12 L2 -5 L8 -8 L4 -3 L-4 -3Z" fill="', .rg("#4f8a3a", .3, -.3), '"/>')
    halfb <- paste0(.p(BERRY, .rg(a, .2, -.3)), '<g transform="scale(.82) translate(0 1)">', .p(BERRY, .pool("#fde4dc", "#ef8c8c")), '</g>',
                    '<path d="M0 -3 C-2 2 -1 7 0 9 C1 7 2 2 0 -3Z" fill="#fff3ee"/>')
    paste0(.g(whole, -12, -4, 1, -15), .g(whole, 10, -6, 1, 18), .g(halfb, -2, 8, 1, 0), .g(whole, 17, 10, .9, 30))
  },
  pom = function(a, b, f) {
    ar <- unlist(lapply(list(c(-6, -6), c(-1, -8), c(4, -7), c(8, -3), c(-8, 0), c(-3, -2), c(2, -2), c(7, 3), c(-5, 5), c(0, 3), c(5, 7), c(-1, 8), c(-9, 5), c(9, -8)),
                        function(p) paste0(.e(p[1], p[2], 2.5, 2.9, .rg("#c2183a", .45, -.3), 20), .hl(p[1] - .7, p[2] - .9, .7, .45, 0, .8))))
    paste0(.c(0, 0, 15.5, .rg(a, .2, -.35)), .c(0, 0, 13, "#f4e4cc"),
           '<path d="M-12 -2 Q0 1 12 -3 M-2 -12 Q0 0 3 12" stroke="#ead2b4" stroke-width="1.6" fill="none"/>', .many(ar),
           .many(lapply(list(c(20, 6), c(23, 12), c(18, 15), c(-19, 11), c(-16, 16)), function(p) .e(p[1], p[2], 2.5, 2.9, .rg("#c2183a", .45, -.3), 20))))
  },
  tom = function(a, b, f) {
    pts <- list(c(-14, 4), c(-4, -2), c(7, 3), c(16, -3), c(2, 10))
    vine <- '<path d="M-17 -9 C-8 -13 4 -13 19 -11 M-14 -9 L-14 -3 M-4 -12 L-4 -8 M7 -12 L7 -3 M16 -11 L16 -9" fill="none" stroke="#5c8a3a" stroke-width="1.6" stroke-linecap="round"/>'
    toms <- vapply(pts, function(p) paste0(.c(p[1], p[2], 6.8, .rg(a, .4, -.35)), .hl(p[1] - 2.2, p[2] - 2.5, 2.1, 1.2, -30, .65),
                                            '<path d="M', p[1] - 3.5, ' ', p[2] - 6, ' l3.5 1.8 l3.5 -1.8 l-2 2.6 l-1.5 -1 l-1.5 1z" fill="#4f8a3a"/>'), "")
    paste0(vine, .many(toms))
  },
  cuke = function(a, b, f) .many(lapply(0:6, function(i) {
    .g(paste0(.c(0, 0, 8.8, .rg(a, .1, -.35)), .c(0, 0, 7.4, .pool("#eaf6d4", "#cfe6a8")),
              .e(c(0, 2.6, -2.6, 2.6, -2.6, 0), c(-3, -1.5, -1.5, 1.5, 1.5, 3), .6, 1, "#a9c77f", c(0, 60, -60, -60, 60, 0))),
       -27 + i * 9, -abs(i - 3) * 1.8 + 3)
  })),
  ov = function(a, b, f) {
    pos <- list(c(-13, -5, 15), c(13, -6, 40), c(-10, 4, -20), c(10, 3, -5), c(0, 6, 10), c(-5, -3, 30), c(6, -4, -25), c(0, 0, 0))
    .many(lapply(pos, function(p) paste0(.e(p[1], p[2], 7.8, 4.6, .rg(a, .35, -.35), p[3]),
                                        .p(sprintf("M%s %s q3 -1.5 6 0", .n(p[1] - 3.5), .n(p[2] + .5)), "none", sprintf(' stroke="%s" stroke-width=".6" transform="rotate(%s %s %s)"', .shade(a, -.4), p[3], p[1], p[2])),
                                        .e(p[1] - 2, p[2] - 1.6, 2.8, .9, "#ffffff", p[3], ' fill-opacity=".3"'))))
  },
  disc = function(a, b, f) .many(lapply(0:6, function(i) {
    x <- c(-14, -4, 6, 15, -9, 2, 11)[i + 1]; y <- c(0, -3, 0, 2, 7, 8, 9)[i + 1]
    paste0(.e(x, y, 7.6, 6.6, .rg(a, .3, -.3), i * 25),
           .p(sprintf("M%s %s q3.5 2.4 7 0", .n(x - 3.5), .n(y)), "none", sprintf(' stroke="%s" stroke-width="1"', .shade(a, -.3))))
  })),
  dolma = function(a, b, f) .many(lapply(0:4, function(i) {
    .g(paste0('<rect x="-12.5" y="-6.2" width="25" height="12.4" rx="6.2" fill="', .lg(a, .2, -.25), '"', .stroke(a, .6), '/>',
              '<path d="M-8 -5 Q-4 0 -8 5 M-2 -6 Q2 0 -2 6 M4 -6 Q8 0 4 6" fill="none" stroke="', .shade(a, .3), '" stroke-width=".8"/>',
              '<rect x="-10" y="-4.6" width="20" height="2" rx="1" fill="#ffffff" fill-opacity=".2"/>'),
       -16 + i * 8, -8 + i * 5, 1, -30)
  })),

  # ---- bowls: nuts, olives, spreads, pickled things -------------------------------
  bowl = function(a, b, f) {
    f <- if (nzchar(f)) f else "none"
    rim <- paste0(.e(0, 5, 18.5, 9, "#cfc6b6"), .e(0, 2.5, 18.5, 9, .lg("#fdfaf3", .1, -.08), 0, ' stroke="#d4cbbb" stroke-width=".7"'),
                  .e(0, 2.5, 15.4, 6.8, .lg("#e9e1d2", -.05, .05)))
    if (nzchar(b)) {
      pts <- list(c(-10, 4), c(-3.5, 5.5), c(3.5, 5.5), c(10, 4), c(-12, 0), c(-6, 1), c(0, 1.5), c(6, 1), c(12, 0),
                  c(-8, -3.5), c(-2, -3), c(4, -3.5), c(10, -3), c(-4, -7.5), c(2.5, -7.5), c(-.5, -11))
      nut <- function(i, x, y) {
        r <- (i * 41) %% 70 - 35
        switch(f,
          almond = paste0(.p("M0 -5 C3 -4.2 3.6 1.8 0 5 C-3.6 1.8 -3 -4.2 0 -5Z", .rg(b, .25, -.3), sprintf(' transform="translate(%s %s) rotate(%s)"', .n(x), .n(y), r)),
                          .hl(x - .8, y - 1.2, .9, 2, r, .3)),
          pecan = paste0(.e(x, y, 5.6, 3.4, .rg(b, .2, -.35), r),
                         .p("M-4 0 Q-2 -2 0 0 Q2 2 4 0", "none", sprintf(' stroke="%s" stroke-width=".7" transform="translate(%s %s) rotate(%s)"', .shade(b, -.45), .n(x), .n(y), r))),
          pistachio = paste0(.e(x, y, 4.8, 3.6, .rg(b, .3, -.25), r), .e(x + .6, y - .4, 2.6, 1.7, .rg("#8ea84a", .2, -.2), r)),
          walnut = paste0(.p("M-4 -3 C-2 -5 2 -5 4 -3 C5 0 4 3 2 4 C0 2 0 2 -2 4 C-4 3 -5 0 -4 -3Z", .rg(b, .2, -.35),
                             sprintf(' transform="translate(%s %s) rotate(%s)"', .n(x), .n(y), r))),
          mixed = {
            cols <- c(b, "#d9b27a", "#a5793f", "#e6cf9c"); col <- cols[(i %% 4) + 1]
            paste0(.e(x, y, 4.4, 3, .rg(col, .25, -.3), r))
          },
          olive = paste0(.e(x, y, 4.4, 3.4, .rg(b, .45, -.4), r), .hl(x - 1.2, y - 1.2, 1.2, .6, r, .55)),
          trio = { cols <- c(b, "#3a2236", "#5c6a2a"); paste0(.e(x, y, 4.4, 3.4, .rg(cols[(i %% 3) + 1], .45, -.4), r), .hl(x - 1.2, y - 1.2, 1.2, .6, r, .55)) },
          stuffed = paste0(.e(x, y, 4.4, 3.4, .rg(b, .4, -.4), r), .c(x + .5, y, 1.4, "#c8281e")),
          stuffedy = paste0(.e(x, y, 4.4, 3.4, .rg(b, .4, -.4), r), .c(x + .5, y, 1.4, "#f2d64a")),
          round = paste0(.c(x, y, 3.8, .rg(b, .45, -.35)), .hl(x - 1.2, y - 1.3, 1.2, .7, 0, .6)),
          ring = .e(x, y, 3.8, 3, "none", 0, sprintf(' stroke="%s" stroke-width="1.6" stroke-opacity=".9"', b)),
          petal = paste0(.e(x, y, 4.8, 3.2, .rg(b, .3, -.3), r), .e(x + 1, y, 2.2, 1.4, "#e7e2b4", r)),
          small = .e(x, y, 2.6, 2, .rg(b, .35, -.35), r),
          .e(x, y, 4.3, 3, .rg(b, .3, -.3), r))
      }
      heap <- .many(lapply(seq_along(pts), function(i) nut(i, pts[[i]][1], pts[[i]][2])))
    } else {
      heap <- paste0(.e(0, 2.6, 15.2, 6.6, .pool(.shade(a, .2), .shade(a, -.15))),
                     .e(-5.5, .4, 5, 1.5, "#ffffff", -8, ' fill-opacity=".38"'), .e(5, 4.5, 2.4, .8, "#ffffff", -8, ' fill-opacity=".2"'))
    }
    tool <- switch(f,
      dipper = paste0('<g transform="translate(5 0) rotate(-35)"><rect x="-1.4" y="-30" width="2.8" height="28" rx="1.4" fill="', .lg("#b07a44", .2, -.2, 0), '"/>',
                      .e(0, -2, 4.6, 3.6, .rg("#c48a4c", .25, -.3)), '<path d="M-4.4 -3.4 h8.8 M-4.4 -1 h8.8" stroke="#8a5a2a" stroke-width=".7"/></g>'),
      spoon = paste0('<g transform="translate(7 -1) rotate(-50)"><rect x="-1.1" y="-27" width="2.2" height="21" rx="1.1" fill="', .lg("#c9ced3", .2, -.2, 0), '"/>',
                     .e(0, -4, 3.5, 4.9, .rg("#d4d9de", .4, -.2), 0, ' stroke="#a6adb4" stroke-width=".5"'), '</g>'),
      "")
    paste0(rim, heap, tool)
  },

  # ---- cured meats -----------------------------------------------------------
  roll = function(a, b, f) {
    slice <- function(x, y, k) {
      fat <- if (f == "lean") "" else .e(x + c(-4, 3, 5, -2, 1, -6, 6, -1), y + c(-3, -5, 2, 5, 0, 3, -2, -7),
                                        c(1.5, 1.2, 1.4, 1.1, 1, 1.1, .9, .9), c(1.1, .9, 1, .8, .8, .8, .7, .7),
                                        if (f == "mort") "#fbefe8" else "#f6d8cc", (k * 30 + c(0, 40, 80, 120, 20, 60, 100, 10)) %% 180)
      extra <- switch(f, pepper = .c(x + c(-6, -2, 3, 6, -4, 1, 5, -7), y + c(-4, 3, -6, 4, 6, -1, -1, 1), .8, "#1e1612"),
                      mort = .e(x + c(-5, 4, 0, -3, 6), y + c(-3, 2, 6, 4, -4), 1.6, 1.3, "#fff8f2", 0), "")
      paste0(.e(x, y, 10.4, 9.6, .rg(a, .2, -.3), k * 12, .stroke(a, .8, -.35)),
             .e(x, y, 9.2, 8.4, "none", k * 12, ' stroke="#ffffff" stroke-opacity=".18" stroke-width=".8"'), fat, extra)
    }
    .many(mapply(slice, -26 + (0:6) * 8.6, sin((0:6) * 1.05) * 5.5, 0:6))
  },
  fold = function(a, b, f) .many(lapply(0:2, function(i) {
    if (f == "pepper") {
      .g(paste0('<path d="M-17 2 C-10 -6 6 -7 17 -2 C15 4 6 7 -2 6 C-9 7 -14 6 -17 2Z" fill="', .lg(a, .25, -.25), '"', .stroke(a, .5), '/>',
                '<path d="M-12 0 C-5 -4 6 -5 13 -2" fill="none" stroke="#ffffff" stroke-opacity=".35" stroke-width="1.2"/>'),
         -4 + i * 6, -9 + i * 9, 1, -12 + i * 14)
    } else {
      .g(paste0('<path d="M-17 4 C-13 -10 -6 5 -1 -8 C4 -17 11 -1 17 -7 C16 3 11 11 2 10 C-6 14 -13 12 -17 4Z" fill="', .lg(a, .18, -.2), '"', .stroke(a, .6, -.22), '/>',
                '<path d="M-15 4 C-12 -7 -6 6 -1 -6 C4 -14 10 -1 15 -6" fill="none" stroke="#fdf0ea" stroke-width="2.6" stroke-opacity=".85" stroke-linecap="round"/>',
                '<path d="M-6 6 C-2 2 4 4 8 2" fill="none" stroke="#ffffff" stroke-opacity=".25" stroke-width="1.2"/>'),
         -6 + i * 7, -10 + i * 10, 1, -10 + i * 12)
    }
  })),

  # ---- sweets & pickles ------------------------------------------------------
  choc = function(a, b, f) {
    chunk <- function(w, h) {
      cols <- seq(-w / 2, w / 2 - 6.9, 7); rows <- seq(-h / 2, h / 2 - 6.9, 7)
      cells <- .many(lapply(cols, function(x) lapply(rows, function(y) paste0(
        '<rect x="', .n(x + .8), '" y="', .n(y + .8), '" width="5.4" height="5.4" rx=".8" fill="', .lg(a, .12, -.12), '"/>',
        '<path d="M', .n(x + .8), ' ', .n(y + 6.2), ' V', .n(y + .8), ' H', .n(x + 6.2), '" fill="none" stroke="#ffffff" stroke-opacity=".18" stroke-width=".6"/>'))))
      paste0('<rect x="', -w / 2, '" y="', -h / 2, '" width="', w, '" height="', h, '" rx="1.4" fill="', .shade(a, -.25), '"/>', cells)
    }
    paste0(.g(chunk(21, 14), -10, -4, 1, -12), .g(chunk(14, 14), 11, 2, 1, 14), .g(chunk(14, 7), -3, 12, 1, 4))
  },
  spear = function(a, b, f) paste0(.e(0, 4, 21, 10.5, "#cfc6b6"), .e(0, 2, 21, 10.5, .lg("#fdfaf3", .1, -.08), 0, ' stroke="#d4cbbb" stroke-width=".7"'),
                                   .many(lapply(0:3, function(i) .g(paste0('<rect x="-3.4" y="-15" width="6.8" height="30" rx="3.4" fill="', .lg(a, .15, -.25, 0), '"/>',
                                                                          '<rect x="-1" y="-13" width="2" height="26" rx="1" fill="#d9e9b0" fill-opacity=".65"/>',
                                                                          .c(c(-2.2, 2.4, -2.4, 2.2), c(-9, -3, 4, 10), .55, .shade(a, -.35))),
                                                                   -9 + i * 6, 2, 1, 62 + i * 6))))
)

draw_item <- function(spec, x, y, s = 1, rot = 0) {
  p <- strsplit(spec, ":", fixed = TRUE)[[1]]
  p <- c(p, rep("", max(0, 4 - length(p))))
  .g(ITEM_SHAPES[[p[1]]](.dflt(p[2], "#cccccc"), p[3], p[4]), x, y, s, rot, SH)
}

# ---- cheeses (functions, so their gradients register on each draw) --------------
.knife <- function(x, y, rot) {
  .g(paste0('<path d="M0 -2.6 L19 -2.6 Q25 -1.2 27 1.8 L0 1.8Z" fill="', .lg("#e2e6ea", .3, -.15), '" stroke="#a9b0b7" stroke-width=".5"/>',
            '<path d="M1 -1.6 L18 -1.6" stroke="#ffffff" stroke-opacity=".7" stroke-width=".6"/>',
            '<rect x="-16" y="-2.9" width="16" height="5.8" rx="2.4" fill="', .lg("#5a3c27", .2, -.25), '"/>', .c(c(-12, -5.5), c(0, 0), .8, "#d8c29a")), x, y, 1, rot)
}
# A wedge pointing left. paste = cut face colour, rind = rind colour.
.wedge <- function(paste, rind, decor_side = "", decor_top = "", rind_style = "") {
  top <- .lg(paste, .22, .02, 30); side <- .lg(paste, .02, -.12, 90)
  rindpath <- '<path d="M18 -16 L30 -6 L30 9" fill="none" stroke="RIND" stroke-width="4" stroke-linejoin="round"/>'
  rindpath <- sub("RIND", .shade(rind, 0), rindpath)
  extra <- switch(rind_style,
    basket = '<path d="M30 -5 L30 8" stroke="#5c3f22" stroke-width="4" stroke-dasharray="1.6 1.4"/>',
    wax = paste0('<path d="M18 -16 L30 -6 L30 9" fill="none" stroke="', .shade(rind, .25), '" stroke-width="1.4" stroke-opacity=".6"/>'),
    "")
  paste0('<path d="M-30 4 L18 -16 L30 -6 L30 9 L-30 18Z" fill="', side, '"/>',
         '<path d="M-30 4 L18 -16 L30 -6Z" fill="', top, '"/>', decor_side, decor_top, rindpath, extra,
         '<path d="M-30 4 L30 -6" stroke="#ffffff" stroke-opacity=".45" stroke-width=".8"/>')
}
.slices <- function(paste, rind, n = 3) {
  .many(lapply(seq_len(n), function(i) .g(paste0('<path d="M-15 3 L12 -3.4 L14.6 1.6 L-15 5.2Z" fill="', .lg(paste, .15, -.08), '"', .stroke(paste, .45, -.15), '/>',
                                                 '<path d="M12 -3.4 L14.6 1.6" stroke="', rind, '" stroke-width="2.6" stroke-linecap="round"/>'),
                                          -4 + i * 5, 23 + i * 4.6, 1, -6 + i * 3)))
}
.crumbs <- function(fill, fleck, pts) {
  shapes <- c("M-3.6 -1 L-1 -3.4 L2.8 -2.6 L3.8 .8 L1 3.2 L-2.8 2.4Z", "M-3 -2.4 L1.6 -3 L3.4 0 L1.8 2.8 L-2.2 2.6 L-3.6 .2Z",
              "M-2.6 -2.8 L2.4 -2.2 L3 1.6 L-.4 3.2 L-3.2 .8Z")
  .many(lapply(seq_along(pts), function(i) {
    p <- pts[[i]]
    .g(paste0(.p(shapes[(i - 1) %% 3 + 1], .lg(fill, .15, -.1), .stroke(fill, .4, -.12)), .e(.6, -.3, 1, .7, fleck, i * 30)), p[1], p[2], p[3], i * 40)
  }))
}
.block <- function(paste, rind = NULL) {
  paste0('<path d="M-24 -6 L8 -14 L26 -6 L26 10 L-6 18 L-24 10Z" fill="', .lg(paste, 0, -.12), '"/>',
         '<path d="M-24 -6 L8 -14 L26 -6 L-6 2Z" fill="', .lg(paste, .25, .05, 30), '"/>',
         '<path d="M-6 2 L26 -6 L26 10 L-6 18Z" fill="', .lg(paste, -.04, -.16, 0), '"/>',
         '<path d="M-24 -6 L-6 2 L26 -6" fill="none" stroke="#ffffff" stroke-opacity=".45" stroke-width=".7"/>',
         if (!is.null(rind)) paste0('<path d="M-24 -6 L8 -14 L26 -6" fill="none" stroke="', rind, '" stroke-width="2"/>') else "")
}
# mottled blue-green veins: irregular blotches rather than lines
.veins <- function(pts) {
  .many(lapply(seq_along(pts), function(i) {
    p <- pts[[i]]
    paste0(.e(p[1], p[2], 2.6 * p[3], 1.5 * p[3], .rg("#4f7480", .1, -.25), (i * 47) %% 180, ' fill-opacity=".85"'),
           .e(p[1] + 1.8, p[2] + .8, 1.2 * p[3], .8 * p[3], "#6d8f90", (i * 47) %% 180, ' fill-opacity=".8"'))
  }))
}
CHEESE_ICONS <- list(
  brie = function() {
    rind <- .lg("#fcf9ef", .1, -.06)
    mottle <- .e(c(-16, -6, 6, 16, -20, -2, 12, -10, 22), c(-5, -9, -8, -4, 1, 2, 3, 6, 0), c(4, 3, 3.5, 3, 2.6, 3, 2.4, 2.4, 2), c(1.6, 1.2, 1.4, 1.2, 1, 1.2, 1, 1, .8),
                 "#e7e1cc", c(10, -10, 20, 0, 30, -20, 10, 0, 40), ' fill-opacity=".7"')
    paste0(.e(0, 9, 33, 14.5, "#e8dfc4"), '<rect x="-33" y="-1" width="66" height="10" fill="', .lg("#f2ecda", .05, -.08), '"/>',
           .e(0, -1, 33, 14.5, rind, 0, ' stroke="#e3dcc6" stroke-width=".8"'), mottle,
           '<path d="M0 -1 L33 -1 A33 14.5 0 0 1 13 12.4 Z" fill="', .pool("#f8e3a2", "#f2d27c"), '"/>',
           '<path d="M0 -1 L13 12.4 L13 22.4 L0 9Z" fill="', .lg("#efd07a", .05, -.1), '"/>',
           '<path d="M13 12.4 Q16 16 14 20" fill="none" stroke="#f6dc94" stroke-width="2" stroke-linecap="round"/>',
           .g(paste0('<path d="M-13 2 L13 -6 L15.4 -2 L15.4 5 L-13 9Z" fill="', .lg("#f4d98a", .1, -.08), '"/>',
                     '<path d="M-13 2 L13 -6 L15.4 -2Z" fill="', rind, '" stroke="#e6dab8" stroke-width=".5"/>',
                     '<path d="M15.4 -2 L15.4 5" stroke="#fbf7ea" stroke-width="2.6"/>'), 35, 24, 1, 8),
           .knife(-21, 26, -8))
  },
  goat = function() {
    log <- .lg("#fbf8f0", .2, -.08)
    paste0('<rect x="-31" y="-10.5" width="47" height="22" rx="10.5" fill="', log, '" stroke="#e3dac3" stroke-width=".7"/>',
           '<rect x="-27" y="-8" width="40" height="3" rx="1.5" fill="#ffffff" fill-opacity=".6"/>',
           .e(16, .5, 5.2, 11, .pool("#fdfbf4", "#f1ead8"), 0, ' stroke="#e3dac3" stroke-width=".7"'),
           .many(lapply(0:2, function(i) .g(paste0(.e(0, 0, 9.5, 9.5, .pool("#fefcf6", "#f1eadb"), 0, ' stroke="#e3dac3" stroke-width=".7"'),
                                                   .c(c(-3, 2, 0, 4), c(-2, 1, 4, -3), .5, "#e2d9c2")), 28 + i * 9.5, 6 + i * 3.2, 1))),
           .knife(-6, 23, 4))
  },
  fresh = function() {
    paste0(.e(0, 5, 35, 19, "#d9d2c4"), .e(0, 3, 35, 19, .lg("#f7f3ec", .1, -.06), 0, ' stroke="#d4cdbf" stroke-width=".8"'),
           .e(0, 3.5, 29.5, 14.5, .pool("#f4f1ea", "#e4ded1")),
           .many(lapply(list(c(-13, 4, 12.5), c(12, 6, 11.5), c(-1, -6, 11.5)), function(p)
             paste0(.c(p[1], p[2], p[3], .rg("#fffdf8", .1, -.12)), .hl(p[1] - 3.5, p[2] - 4.5, 4, 2, -25, .95)))),
           .e(c(-4, 16), c(12, 12), c(4, 3), c(1.4, 1), "#d8c66a", 0, ' fill-opacity=".5"'))
  },
  feta = function() {
    paste0('<path d="M-26 -2 L10 -12 L26 -4 L26 10 L-10 20 L-26 12Z" fill="', .lg("#f1ece0", 0, -.08), '"/>',
           '<path d="M-26 -2 L10 -12 L26 -4 L-10 6Z" fill="', .lg("#fcfaf3", .2, 0, 30), '"/>',
           '<path d="M-10 6 L26 -4 L26 10 L-10 20Z" fill="', .lg("#ebe5d4", 0, -.08, 0), '"/>',
           .e(c(-18, -6, 6, 14, -12, 0, -20, 18), c(0, -4, -6, -2, 2, 0, 4, 4), c(1.2, 1, 1.3, .9, 1, 1.1, .9, 1), c(.8, .7, .9, .6, .7, .8, .6, .7), "#e3dcc8", 0),
           .crumbs("#f7f4ea", "#f7f4ea", list(c(31, 16, 1.1), c(39, 9, 1), c(25, 24, 1.2), c(36, 22, .9))),
           '<path d="M-24 -1 Q-15 -7 -4 -6" fill="none" stroke="#c5b462" stroke-width="1.4" stroke-opacity=".55"/>',
           .knife(-20, 26, -6))
  },
  sheep = function() paste0(.wedge("#f3dd98", "#7a5a32", rind_style = "basket",
                                   decor_side = .c(c(-16, -6, 4, 14), c(12, 9, 7, 5), .7, "#e2c47a")),
                            .slices("#efd78e", "#6e5030"), .knife(-27, 30, -14)),
  alpine = function() paste0(.wedge("#f1d38a", "#8d5f32",
                                    decor_side = paste0(.e(c(-16, -4, 8, 18, -10, 2), c(12, 9, 6, 4, 6, 12), c(1.8, 1.3, 2, 1.2, 1.1, 1), c(1.4, 1, 1.5, .9, .8, .8), "#d6ae5c"),
                                                        .e(c(-16.4, -4.3, 7.6), c(11.6, 8.7, 5.6), c(.8, .6, .9), c(.5, .4, .6), "#fff3cf", 0))),
                             .slices("#ecd18a", "#8d5f32"), .knife(-27, 30, -14)),
  toscano = function() paste0(.wedge("#f6e5b0", "#7c5f3a",
                                     decor_side = .c(c(-18, -8, 2, 12, 20, -12, 6, -2, 16), c(12, 10, 8, 6, 5, 6, 3, 13, 9), .65, "#2e261e"),
                                     decor_top = .c(c(22, 26, 24, 28), c(-12, -9, -7, -5), .7, "#2e261e")),
                              .slices("#f2dfa4", "#7c5f3a"), .knife(-27, 30, -14)),
  cheddar = function() paste0(.block("#f1c45e"),
                              .many(lapply(0:2, function(i) .g(paste0('<path d="M-11 -4.4 L11 -4.4 L11 4.4 L-11 4.4Z" fill="', .lg("#f4cb6a", .15, -.06), '"', .stroke("#f2c560", .5, -.2), '/>',
                                                                      '<path d="M-11 -4.4 L11 -4.4" stroke="#ffffff" stroke-opacity=".4" stroke-width=".6"/>'), 27 + i * 4, 19 + i * 5.4, 1, -12 + i * 4))),
                              .crumbs("#f2c560", "#f6d88a", list(c(38, 10, .8), c(42, 16, .6))),
                              .knife(-22, 24, -6)),
  gouda = function() paste0(.wedge("#f0c066", "#b2341a", rind_style = "wax",
                                   decor_side = .c(c(-14, 0, 14, -6, 8), c(11, 7, 4, 14, 10), c(1, .8, 1, .7, .7), "#fff2c6")),
                            .slices("#ecbb62", "#b2341a"), .knife(-27, 30, -14)),
  parm = function() {
    grain <- .c(c(-16, -6, 4, -10, 8, 12, -20, 0, 16, -2), c(6, 10, 6, -6, 10, -6, 2, -10, 4, 14), c(.9, .7, .8, .7, .9, .6, .7, .6, .8, .7), "#fffbe6")
    paste0('<path d="M-28 10 L-20 -8 L-2 -16 L18 -12 L26 2 L20 14 L-6 18Z" fill="', .lg("#ebd28c", .05, -.1), '"/>',
           '<path d="M-20 -8 L-2 -16 L18 -12 L4 -2 L-14 2Z" fill="', .lg("#f5e4ac", .2, 0, 30), '"/>',
           '<path d="M18 -12 L26 2 L20 14 L4 -2Z" fill="', .lg("#dcc070", 0, -.1, 0), '"/>',
           '<path d="M18 -12 L26 2 L20 14" fill="none" stroke="#c49a46" stroke-width="3.2" stroke-linejoin="round"/>', grain,
           .many(lapply(list(c(31, 19, 10), c(19, 27, -20), c(41, 9, 40), c(5, 29, 15), c(40, 22, -30)), function(p)
             .g(paste0('<path d="M-6.5 -4.5 L4 -6.5 L7.4 2 L-2 6.6 L-5 3Z" fill="', .lg("#f0da9a", .15, -.08), '"', .stroke("#e3c878", .5, -.1), '/>',
                       .c(c(-1, 2), c(0, -2), .5, "#fffbe6")), p[1], p[2], 1, p[3]))))
  },
  blue = function() paste0(.wedge("#f6f0df", "#cdbf9c",
                                  decor_side = .veins(list(c(-20, 10, 1), c(-10, 12, .9), c(-2, 9, 1.1), c(8, 6, .9), c(16, 5, 1), c(-14, 6, .7), c(4, 13, .8), c(22, 2, .7))),
                                  decor_top = .veins(list(c(-6, -4, .7), c(8, -9, .8), c(18, -10, .6)))),
                           .crumbs("#f4eedc", "#5a7f8a", list(c(27, 24, 1.1), c(34, 18, .9), c(21, 29, 1), c(33, 28, .8), c(40, 24, .7))),
                           .knife(-27, 30, -14)),
  stilton = function() paste0(.block("#f4ead0", "#e3d4ae"),
                              .e(c(-16, -4, 8, 18, 0, 14, -10, 4), c(0, -6, -2, 4, 10, 10, 6, 14), 2.4, 1.8, .rg("#c9483a", .3, -.2), c(10, 40, -20, 0, 30, -10, 60, 20)),
                              .e(c(-10, 4, 12, -18, 20), c(-6, 0, -8, 4, -2), 1.9, 1.5, .rg("#e8a13a", .3, -.2), c(0, 30, -30, 10, 50)),
                              .crumbs("#f4ead2", "#c9483a", list(c(31, 19, 1.1), c(38, 13, .9), c(25, 26, 1), c(36, 25, .8))),
                              .knife(-22, 24, -6))
)

# ---- wine ------------------------------------------------------------------
# kind: "sparkling", "burgundy" (sloped shoulders) or "bordeaux" (high shoulders)
.bottle <- function(col, kind) {
  glass <- switch(kind, sparkling = "#2f4a2c", burgundy = "#34452a", "#22301f")
  foil <- if (kind == "sparkling") "#d4b24a" else .shade(col, -.2)
  body <- if (kind == "bordeaux")
    "M-3.6 -92 h7.2 v28 c0 5 9.4 5 9.4 14 v52 c0 3 -2.4 5 -5 5 h-16 c-2.6 0 -5 -2 -5 -5 v-52 c0 -9 9.4 -9 9.4 -14z"
  else "M-3.6 -92 h7.2 v24 c0 10 10.4 14 10.4 28 v42 c0 3 -2.4 5 -5 5 h-18 c-2.6 0 -5 -2 -5 -5 v-42 c0 -14 10.4 -18 10.4 -28z"
  paste0(.p(body, .lg(glass, .12, -.25, 0)),
         '<rect x="-4.4" y="', if (kind == "sparkling") -96 else -95, '" width="8.8" height="', if (kind == "sparkling") 22 else 11, '" rx="1.6" fill="', .lg(foil, .25, -.2, 0), '"/>',
         '<rect x="-9.5" y="-36" width="19" height="25" rx="1" fill="', .lg("#f8f2e3", .1, -.06, 0), '"/>', '<rect x="-9.5" y="-26" width="19" height="6" fill="', col, '"/>',
         '<rect x="-6" y="-33" width="12" height="1.6" fill="#8a7a62"/>', '<rect x="-7.6" y="-62" width="2.4" height="54" rx="1.2" fill="#ffffff" fill-opacity=".2"/>')
}
.glass <- function(col, sparkling) {
  if (sparkling) {
    paste0('<path d="M-5.5 -38 h11 c0 18 -1 28 -5.5 30 c-4.5 -2 -5.5 -12 -5.5 -30z" fill="#ffffff" fill-opacity=".5" stroke="#9aa3ab" stroke-width=".8"/>',
           '<path d="M-4.6 -30 h9.2 c-.4 13 -1.6 19 -4.6 21 c-3 -2 -4.2 -8 -4.6 -21z" fill="', .lg(col, .2, -.12), '"/>',
           .c(c(-1.5, 1.2, 0, -.5, 1.8), c(-24, -20, -15, -27, -12), c(.7, .6, .7, .5, .5), "#ffffff", ' fill-opacity=".75"'),
           '<line x1="0" y1="-8" x2="0" y2="9" stroke="#9aa3ab" stroke-width="1.4"/>', .e(0, 10, 7, 2, "#9aa3ab"))
  } else {
    paste0('<path d="M-11 -34 h22 c0 18 -5 27 -11 27 s-11 -9 -11 -27z" fill="#ffffff" fill-opacity=".5" stroke="#9aa3ab" stroke-width=".8"/>',
           '<path d="M-10 -22 h20 c-1 10 -5 15 -10 15 s-9 -5 -10 -15z" fill="', .lg(col, .1, -.25), '"/>', .e(0, -22, 10, 2, .shade(col, .2)),
           '<path d="M-7 -31 q-2 8 0 16" fill="none" stroke="#ffffff" stroke-opacity=".7" stroke-width="1.4"/>',
           '<line x1="0" y1="-7" x2="0" y2="9" stroke="#9aa3ab" stroke-width="1.4"/>', .e(0, 10, 8, 2.2, "#9aa3ab"))
  }
}

# ---- layout ------------------------------------------------------------------
# viewBox 0 0 400 324; the board spans x 8-312, y 10-310. Each cheese and each item
# is treated as a circle. Everything is scaled so the board is about 80% covered
# (big, generous portions on a sparse board, smaller ones as it fills), then the
# pieces are nudged apart until they sit snugly without piling up, inside the edges.
SHAPE_R <- c(sq = 32, rd = 31, rect = 30, bread = 34, boule = 30, grape = 25, apple = 27, fig = 24, berry = 24,
             pom = 25, tom = 23, cuke = 35, ov = 20, disc = 22, dolma = 25, bowl = 22, roll = 34, fold = 24, choc = 22, spear = 24)
CHEESE_R <- 46
CHEESE_LAYOUT <- list(
  list(c(0, 0)),
  list(c(-34, -26), c(34, 28)),
  list(c(-42, -28), c(44, -22), c(0, 36)),
  list(c(-44, -32), c(44, -34), c(-44, 34), c(44, 32)),
  list(c(-48, -38), c(48, -38), c(0, 0), c(-48, 40), c(48, 40))
)
CAT_ORDER <- c("Fruit & veggies", "Spreads & condiments", "Cured meats", "Crackers & bread", "Olives & pickled", "Nuts", "Dark chocolate")

layout_board <- function(item_r, n_cheese) {
  N <- length(item_r); cx <- 160; cy <- 160
  tot <- pi * (sum(item_r^2) + n_cheese * (CHEESE_R * 1.25)^2)
  t <- if (tot > 0) max(.72, min(1.55, sqrt(.8 * 296 * 292 / tot))) else 1
  r <- c(rep(CHEESE_R * 1.25 * t, n_cheese), item_r * t)
  lay <- if (n_cheese > 0) CHEESE_LAYOUT[[n_cheese]] else list()
  sp <- if (N == 0) 1.15 else max(.8, t)
  x <- c(vapply(lay, function(p) cx + p[1] * sp, 0), numeric(N))
  y <- c(vapply(lay, function(p) cy + p[2] * sp, 0), numeric(N))
  if (N > 0) {
    R <- if (n_cheese == 0) (if (N == 1) 0 else 70) else 112
    ang <- (-90 + (seq_len(N) - 1) * 360 / N + 180 / N) * pi / 180
    x[n_cheese + seq_len(N)] <- cx + R * cos(ang)
    y[n_cheese + seq_len(N)] <- cy + R * .96 * sin(ang)
  }
  w <- c(rep(.3, n_cheese), rep(1, N))
  m <- r * 1.02 + 5
  for (it in seq_len(260)) {
    dx <- outer(x, x, "-"); dy <- outer(y, y, "-"); d <- sqrt(dx^2 + dy^2) + 1e-6
    ov <- pmax(outer(r, r, "+") * .84 - d, 0); diag(ov) <- 0
    x <- x + .45 * w * rowSums(ov * dx / d) + (cx - x) * .008 * w
    y <- y + .45 * w * rowSums(ov * dy / d) + (cy - y) * .008 * w
    x <- pmin(pmax(x, 10 + m), 310 - m); y <- pmin(pmax(y, 14 + m), 306 - m)
  }
  list(x = x, y = y, t = t)
}

board_svg <- function(cheese_styles = character(), wines = NULL, items = NULL) {
  .reset_defs()
  if (is.null(items)) items <- data.frame(category = character(), icon = character())
  items <- items[order(match(items$category, CAT_ORDER)), , drop = FALSE]
  N <- nrow(items)
  n <- min(length(cheese_styles), 5)
  shapes <- sub(":.*", "", items$icon)
  L <- layout_board(unname(SHAPE_R[shapes]), n)

  body <- character()
  for (i in seq_len(N)) {
    j <- n + i
    body <- c(body, draw_item(items$icon[i], L$x[j], L$y[j], L$t, ((i * 37) %% 24) - 12))
  }
  for (i in seq_len(n)) {
    body <- c(body, .g(CHEESE_ICONS[[cheese_styles[i]]](), L$x[i], L$y[i], min(2.05, 1.25 * L$t * (if (N == 0) 1.15 else 1)), 0, SH))
  }
  if (n == 0 && N == 0) {
    body <- c(body, '<text x="160" y="164" text-anchor="middle" font-size="15" fill="#6b4a26" font-family="Georgia, serif" font-style="italic">Pick a cheese to start your board</text>')
  }
  wine <- character()
  if (!is.null(wines) && nrow(wines)) {
    k <- min(nrow(wines), 3)
    for (i in seq_len(min(k, 2))) wine <- c(wine, .g(.glass(wines$color[i], wines$kind[i] == "sparkling"), 344 + (i - 1) * 30, 76 + (i - 1) * 8, 1.25, 0, SH))
    for (i in seq_len(k)) wine <- c(wine, .g(.bottle(wines$color[i], wines$kind[i]), 338 + (i - 1) * 25, 304 - (i - 1) * 2, 1.1, 0, SH))
  }

  # the board: end-grain style edge, oiled top, soft grain
  ys <- seq(22, 300, 11); kk <- length(ys)
  grain <- .many(sprintf('<path d="M8 %s C70 %s 150 %s 230 %s S300 %s 312 %s" fill="none" stroke="%s" stroke-opacity="%s" stroke-width="%s"/>',
                         ys, ys + rep_len(c(3, -2, 2, -3, 1), kk), ys + rep_len(c(-2, 3, -1, 2, -3), kk), ys + rep_len(c(2, -1, 3, -2, 1), kk),
                         ys + rep_len(c(-1, 2, -2, 1, 3), kk), ys + rep_len(c(1, -2, 2, -1, 0), kk),
                         rep_len(c("#7a5226", "#8a5e2c", "#a87a44"), kk), rep_len(c(".09", ".06", ".12", ".05"), kk), rep_len(c(.9, 1.6, .7, 1.2, .6), kk)))
  knots <- paste0(.e(236, 252, 9, 3.4, "none", -6, ' stroke="#7a5226" stroke-opacity=".12" stroke-width="1"'),
                  .e(236, 252, 4.5, 1.6, "#7a5226", -6, ' fill-opacity=".08"'))
  defs <- paste0('<defs>',
    '<linearGradient id="wood" x1="0" y1="0" x2="1" y2="1"><stop offset="0" stop-color="#e3c290"/><stop offset=".45" stop-color="#d4a970"/><stop offset="1" stop-color="#bf8f58"/></linearGradient>',
    '<radialGradient id="sheen" cx=".35" cy=".25" r=".8"><stop offset="0" stop-color="#ffffff" stop-opacity=".22"/><stop offset=".6" stop-color="#ffffff" stop-opacity="0"/></radialGradient>',
    '<linearGradient id="edge" x1="0" y1="0" x2="0" y2="1"><stop offset="0" stop-color="#a87444"/><stop offset="1" stop-color="#7c5128"/></linearGradient>',
    '<filter id="ds" x="-25%" y="-25%" width="150%" height="160%"><feDropShadow dx="0" dy="1.8" stdDeviation="1.4" flood-color="#3a2410" flood-opacity=".32"/></filter>',
    '<filter id="boardshadow" x="-10%" y="-10%" width="120%" height="125%"><feDropShadow dx="0" dy="6" stdDeviation="6" flood-color="#2a1a0c" flood-opacity=".28"/></filter>',
    '<clipPath id="boardclip"><rect x="8" y="10" width="304" height="300" rx="30"/></clipPath>',
    paste(.defs$list, collapse = ""), '</defs>')
  board <- c(
    '<rect x="8" y="16" width="304" height="300" rx="30" fill="url(#edge)" filter="url(#boardshadow)"/>',
    '<rect x="8" y="10" width="304" height="300" rx="30" fill="url(#wood)"/>',
    '<g clip-path="url(#boardclip)">', grain, knots, '<rect x="8" y="10" width="304" height="300" fill="url(#sheen)"/>', '</g>',
    '<rect x="9" y="11" width="302" height="298" rx="29" fill="none" stroke="#f6e2bd" stroke-opacity=".55" stroke-width="1.2"/>'
  )
  HTML(paste0('<svg viewBox="0 0 400 324" xmlns="http://www.w3.org/2000/svg" role="img" aria-label="Illustration of your cheese board">',
              defs, paste(board, collapse = ""), paste(body, collapse = ""), paste(wine, collapse = ""), '</svg>'))
}
