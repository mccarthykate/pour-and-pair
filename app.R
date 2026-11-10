# Pour & Pair: a wine and cheese pairing guide with a cheese board builder,
# using products sold at Trader Joe's.
# Run with shiny::runApp() or the "Run App" button in RStudio.

library(shiny)
library(bslib)
# Files in R/ are sourced automatically by Shiny.

theme <- bs_theme(
  version = 5,
  bg = "#f7f4f4", fg = "#2a1c23", primary = "#8a2144",
  base_font = font_google("Figtree", local = FALSE),
  heading_font = font_google("Fraunces", local = FALSE)
)

js_set <- function(input_id, value) {
  sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})", input_id, gsub("'", "\\\\'", value))
}
TOOLTIP_JS <- "
(function(){
  function box(){ return document.getElementById('tipbox'); }
  function show(el){
    var b = box(); if (!b) return;
    b.textContent = el.getAttribute('data-tip'); b.hidden = false;
    var r = el.getBoundingClientRect(), bw = b.offsetWidth, bh = b.offsetHeight;
    var x = Math.min(Math.max(8, r.left + r.width / 2 - bw / 2), window.innerWidth - bw - 8);
    var y = r.top - bh - 8; if (y < 8) y = r.bottom + 8;
    b.style.left = x + 'px'; b.style.top = y + 'px';
  }
  function hide(){ var b = box(); if (b) b.hidden = true; }
  function tipOf(e){ return e.target && e.target.closest ? e.target.closest('[data-tip]') : null; }
  document.addEventListener('mouseover', function(e){ var el = tipOf(e); if (el) show(el); });
  document.addEventListener('mouseout', function(e){ var el = tipOf(e); if (el && !el.contains(e.relatedTarget)) hide(); });
  document.addEventListener('focusin', function(e){ var el = tipOf(e); if (el) show(el); });
  document.addEventListener('focusout', hide);
  document.addEventListener('scroll', hide, true);
  document.addEventListener('shiny:value', hide);
})();
"
money <- function(x) sprintf("$%.2f", x)
styles_of <- function(kind) if (kind == "wine") WINE_STYLES else CHEESE_STYLES
products_of <- function(kind) if (kind == "wine") WINE_PRODUCTS else CHEESE_PRODUCTS
style_name <- function(kind, id) { s <- styles_of(kind); s$name[match(id, s$id)] }

ui <- page_fluid(
  theme = theme,
  title = "Pour & Pair",
  tags$head(tags$link(rel = "stylesheet", href = paste0("styles.css?v=", as.integer(file.mtime("www/styles.css")))), tags$script(HTML(TOOLTIP_JS))),
  div(class = "page-head px-3 pt-4",
      div(class = "eyebrow", "A beginner's guide to wine and cheese"),
      h1("Pour & Pair"),
      p(class = "lede", "Size your board for your guests, pick wine and cheese sold at Trader Joe's, and build the rest of the board. The Learn tab explains the thinking behind every pairing.")),
  navset_underline(
    id = "tab",
    nav_panel("Build your board", value = "build",
  div(
    class = "layout py-3 px-3",
    div(
      class = "main",

      h3(class = "h5 mb-2", "Five ideas to know"),
      div(class = "rules", lapply(seq_len(nrow(RULES)), function(i) {
        div(class = "rule", tags$b(RULES$title[i]), p(RULES$text[i]))
      })),
      div(class = "strategy",
          span(tags$b("Match"), " intensity"),
          span(tags$b("Contrast"), " acid vs. cream, sweet vs. salt"),
          span(tags$b("Share"), " a flavor, like nutty with nutty")),

      h2(class = "mt-4 mb-3", "Plan your board"),
      div(
        class = "tool",
        div(class = "plan-inputs",
            sliderInput("guests", "How many guests?", min = 2, max = 40, value = 8, step = 1, width = "100%"),
            radioButtons("appetite", "How will it be eaten?", width = "100%",
                         choices = c("Appetizer before a meal" = "app", "Main event (replaces dinner)" = "main"))),
        uiOutput("plan_tiles")
      ),

      h2(class = "mt-4 mb-3", "Pick your wines and cheeses"),
      div(
        class = "tool",
        radioButtons("mode", NULL, inline = TRUE,
                     choices = c("Start with wines" = "wine", "Start with cheeses" = "cheese")),
        div(div(class = "step-h", span(class = "eyebrow", "Step 1"), h3(textOutput("t1", inline = TRUE))),
            uiOutput("step1")),
        div(div(class = "step-h", span(class = "eyebrow", "Step 2"), h3(textOutput("t2", inline = TRUE))),
            uiOutput("step2")),
        div(div(class = "step-h", span(class = "eyebrow", "Step 3"), h3("The best wine for each cheese")),
            uiOutput("step3")),
        div(div(class = "step-h", span(class = "eyebrow", "Step 4"), h3("Fill out the board")),
            uiOutput("board"))
      ),
      p(class = "note mt-2", "Products and prices come from traderjoes.com (checked October 29, 2026). Stock and prices vary by store and season, and produce and seasonal items come and go, so check the shelf."),

      h2(class = "mt-4 mb-2", "How to choose what goes on the board"),
      div(class = "guide", lapply(seq_along(GUIDE), function(i) {
        g <- GUIDE[[i]]
        tags$details(open = if (i == 1) NA, tags$summary(g$q), lapply(g$a, p))
      })),

      h2(class = "mt-4", "Board-building tips"),
      div(class = "row g-3", lapply(TIPS, function(t) div(class = "col-md-6", div(class = "tip", t)))),

      h2(class = "mt-4", "Sources"),
      tags$ul(lapply(seq_len(nrow(SOURCES)), function(i) {
        tags$li(tags$a(href = SOURCES$url[i], target = "_blank", rel = "noopener", SOURCES$label[i]))
      }))
    ),
    tags$aside(class = "boardwrap collapsed", uiOutput("miniboard"))
  )),
    nav_panel("Learn: the why", value = "learn", learn_ui())
  ),
  div(id = "tipbox", class = "tipbox", role = "tooltip", hidden = NA)
)

server <- function(input, output, session) {
  # picked products (keys) and which type is open for browsing
  sel <- reactiveValues(wine = character(), cheese = character())
  open <- reactiveValues(first = NULL, second = NULL)  # second = c(anchor key or "browse", style id)
  addons <- reactiveVal(character())

  plan <- reactive({ req(input$guests, input$appetite); plan_board(input$guests, input$appetite) })
  kinds <- reactive({ req(input$mode); if (input$mode == "wine") c("wine", "cheese") else c("cheese", "wine") })
  labels <- c("0" = "Skip it", "2" = "Good", "3" = "Classic")
  plural <- function(kind) if (kind == "wine") "wines" else "cheeses"

  # ---- choices ----------------------------------------------------------
  observeEvent(input$mode, { open$first <- NULL; open$second <- NULL }, ignoreInit = TRUE)
  observeEvent(input$open1, open$first <- if (identical(open$first, input$open1)) NULL else input$open1)
  observeEvent(input$open2, {
    v <- strsplit(input$open2, "|", fixed = TRUE)[[1]]
    open$second <- if (identical(open$second, v)) NULL else v
  })
  observeEvent(input$toggle, {
    p <- strsplit(input$toggle, "|", fixed = TRUE)[[1]]
    cur <- sel[[p[1]]]
    sel[[p[1]]] <- if (p[2] %in% cur) setdiff(cur, p[2]) else c(cur, p[2])
  })
  observeEvent(input$add, {
    cur <- addons()
    addons(if (input$add %in% cur) setdiff(cur, input$add) else c(cur, input$add))
  })
  observeEvent(input$rm, addons(setdiff(addons(), input$rm)))
  observeEvent(input$clear, addons(character()))
  observeEvent(input$quick, addons(union(addons(), ADDONS$key[match(names(advice()$rec), ADDONS$name)])))

  prod_by_key <- function(kind, key) {
    df <- products_of(kind)
    df[df$key == key, , drop = FALSE]
  }
  picked <- function(kind) {
    df <- products_of(kind)
    df[match(sel[[kind]], df$key), , drop = FALSE]
  }

  # ---- planner ----------------------------------------------------------
  output$plan_tiles <- renderUI({
    p <- plan()
    tile <- function(label, big, small) div(class = "ptile", div(class = "lbl", label), div(class = "big", big), div(class = "sm", small))
    div(class = "ptiles",
        tile("Cheeses", paste(p$types, "kinds"), paste0(fmt_oz(p$cheese_oz), " total, about ", fmt_oz(p$cheese_oz / p$types), " of each")),
        tile("Wine", paste(p$bottles, if (p$bottles == 1) "bottle" else "bottles"),
             paste0(p$glasses, " glasses each, in ", p$wine_types, if (p$wine_types == 1) " style" else " different styles")),
        tile("Crackers & bread", paste("about", p$pieces, "pieces"),
             paste0("roughly ", round(p$pieces * 0.6), " crackers and ", round(p$pieces * 0.4), " bread slices")),
        tile("Cured meat", fmt_oz(p$meat_oz), "if you include it"),
        tile("Fruit & veggies", fmt_oz(p$fruit_oz), "a couple of kinds"),
        tile("Nuts", fmt_oz(p$nuts_oz), "in a small bowl"),
        tile("Spreads", paste(p$spreads, if (p$spreads == 1) "jar or bowl" else "jars or bowls"), "one bowl serves about 8"))
  })
  target <- function(kind) if (kind == "wine") plan()$wine_types else plan()$types

  # ---- shared pieces ----------------------------------------------------
  progress <- function(kind) {
    n <- length(sel[[kind]]); t <- target(kind)
    span(class = paste("prog", if (n >= t) "ok"), paste0(n, " of ", t, " ", plural(kind), " picked"),
         if (n > t) " (more than the planner suggests, which is fine)")
  }
  prod_chip <- function(kind, r) {
    label <- if (kind == "cheese") paste0(money(r$price), " · ", sub("Oz", " oz", sub("Lb", " lb", r$size))) else money(r$price)
    on <- r$key %in% sel[[kind]]
    tags$button(type = "button", class = paste("chip prod", if (on) "on"), `aria-pressed` = tolower(on),
                onclick = js_set("toggle", paste(kind, r$key, sep = "|")),
                if (on) span(class = "tick", "✓"), r$name, span(class = "px", label),
                if (grepl("^Not listed", r$note)) span(class = "warn", "ask in store"))
  }
  product_picker <- function(kind, sid) {
    df <- products_of(kind); df <- df[df$style == sid, ]
    sty <- styles_of(kind)
    chosen <- df[df$key %in% sel[[kind]], ]
    facts <- unique(unlist(lapply(seq_len(nrow(chosen)), function(i)
      c(facts_for(chosen$name[i], if (kind == "wine") WINE_FACTS else CHEESE_FACTS), if (nzchar(chosen$note[i])) chosen$note[i]))))
    tagList(
      div(class = "about", style = "margin-top:0;margin-bottom:10px", tags$b(paste0(sty$name[sty$id == sid], ".")), " ", sty$about[sty$id == sid],
          span(class = "hint", " Tap as many as you like.")),
      div(class = "chips", lapply(seq_len(nrow(df)), function(i) prod_chip(kind, df[i, ]))),
      if (length(facts)) div(class = "about", tags$b("About your picks"), tags$ul(class = "facts", lapply(facts, tags$li))))
  }
  picked_pills <- function(kind) {
    pk <- picked(kind)
    if (!nrow(pk)) return(NULL)
    sty <- styles_of(kind)
    div(class = "picked", span(class = "cl-h", paste("Your", plural(kind))),
        lapply(seq_len(nrow(pk)), function(i) {
          div(class = "bit pairbit", span(class = "dot", style = paste0("background:", sty$color[sty$id == pk$style[i]])), pk$name[i],
              tags$button(type = "button", title = "Remove", `aria-label` = paste("Remove", pk$name[i]),
                          onclick = js_set("toggle", paste(kind, pk$key[i], sep = "|")), "×"))
        }))
  }

  # ---- titles -----------------------------------------------------------
  output$t1 <- renderText(paste("Pick your", plural(kinds()[1])))
  output$t2 <- renderText(paste0("Pick ", plural(kinds()[2]), " for each ", kinds()[1]))

  # ---- step 1: pick several of the first kind ----------------------------
  output$step1 <- renderUI({
    k <- kinds()[1]; df <- styles_of(k)
    tagList(
      div(class = "step-sub", progress(k)),
      div(class = "chips", lapply(seq_len(nrow(df)), function(i) {
        n_on <- sum(products_of(k)$style[match(sel[[k]], products_of(k)$key)] == df$id[i], na.rm = TRUE)
        tags$button(type = "button", class = paste("chip", if (identical(open$first, df$id[i])) "on"),
                    onclick = js_set("open1", df$id[i]),
                    span(class = "dot", style = paste0("background:", df$color[i])), df$name[i],
                    if (n_on) span(class = "count", n_on))
      })),
      if (!is.null(open$first)) div(class = "mt-2", product_picker(k, open$first))
      else div(class = "empty mt-2", paste0("Open a type above to see the Trader Joe's ", plural(k), ". You can pick several, from as many types as you like.")),
      picked_pills(k),
      uiOutput("suggest"))
  })

  # suggestions for a second wine (or cheese) based on what is picked
  output$suggest <- renderUI({
    k <- kinds()[1]; o <- other_kind(k)
    pk <- picked(k)
    if (!nrow(pk)) return(NULL)
    cmp <- companions(k, pk$style)
    if (is.null(cmp)) return(NULL)
    sty <- styles_of(k)
    pnames <- function(ids) {
      nm <- style_name(o, ids)
      if (length(nm) > 3) paste0(paste(nm[1:3], collapse = ", "), " and more") else paste(nm, collapse = ", ")
    }
    card <- function(title, s, why) {
      prods <- products_of(k); prods <- prods[prods$style == s & !grepl("^Not listed", prods$note), ]
      prods <- head(prods[order(prods$price), ], 3)
      div(class = "sugg",
          div(class = "sugg-h", span(class = "eyebrow", title),
              span(class = "nm", span(class = "dot", style = paste0("background:", sty$color[sty$id == s])), style_name(k, s))),
          p(class = "why", why),
          div(class = "chips", lapply(seq_len(nrow(prods)), function(i) prod_chip(k, prods[i, ]))))
    }
    lead <- if (nrow(pk) == 1) pk$name[1] else paste("your", plural(k))
    div(class = "suggs",
        div(class = "cl-h", paste0("Suggested ", if (k == "wine") "wines" else "cheeses", " to go with ", lead)),
        div(class = "sugg-grid",
            if (!is.null(cmp$round)) card("Round out the lineup", cmp$round$style, paste0(
              "Adds a match for ", if (length(cmp$round$partners)) pnames(cmp$round$partners) else "a different style",
              ", which your current ", plural(k), " don't cover",
              if (cmp$round$new_group) paste0(", and brings a ", cmp$round$group, " to the table") else "", ".")),
            if (!is.null(cmp$same)) card(paste("Goes with the same", plural(o)), cmp$same$style, paste0(
              "Also pairs with ", pnames(cmp$same$partners), ", so guests can taste the two side by side with the same ", plural(o), "."))))
  })

  # ---- step 2: matches for each pick --------------------------------------
  output$step2 <- renderUI({
    k <- kinds()[1]; o <- kinds()[2]
    pk <- picked(k)
    ostyles <- styles_of(o); oprods <- products_of(o)
    picked_o_styles <- oprods$style[match(sel[[o]], oprods$key)]
    block <- function(anchor, title, rows) {
      opened <- !is.null(open$second) && open$second[1] == anchor
      div(class = "anchor",
          h4(title),
          div(class = "matches", lapply(seq_len(nrow(rows)), function(i) {
            r <- rows[i, ]; oid <- r[[o]]
            n_on <- sum(picked_o_styles == oid, na.rm = TRUE)
            on <- opened && open$second[2] == oid
            ex <- explain_pair(r$wine, r$cheese, "", "")
            tg <- head(ex$tag[ex$type == if (r$rating == 0) "neg" else "pos"], 2)
            tags$button(type = "button", class = paste("match", if (on) "on"),
                        onclick = js_set("open2", paste(anchor, oid, sep = "|")),
                        span(class = "top", span(class = "nm", ostyles$name[ostyles$id == oid]),
                             span(class = paste0("pill p", r$rating), labels[[as.character(r$rating)]])),
                        span(class = "why", r$reason),
                        if (length(tg)) span(class = "tagrow", lapply(tg, function(t) span(class = paste("ptag", if (r$rating == 0) "neg"), t))),
                        span(class = "cnt", if (n_on) paste0("✓ ", n_on, " picked · ") else "", sum(oprods$style == oid), " at Trader Joe's"))
          })),
          if (opened) div(class = "mt-2", product_picker(o, open$second[2])))
    }
    blocks <- lapply(seq_len(nrow(pk)), function(i) {
      rows <- PAIRINGS[PAIRINGS[[k]] == pk$style[i], ]
      rows <- rows[order(-rows$rating), ]
      block(pk$key[i], paste0("For ", pk$name[i]), rows)
    })
    browse_open <- !is.null(open$second) && open$second[1] == "browse"
    tagList(
      div(class = "step-sub", progress(o)),
      if (!nrow(pk)) div(class = "empty", paste0("Pick at least one ", k, " in step 1 and its best ", plural(o), " appear here, ranked."))
      else blocks,
      tags$details(class = "browse", open = if (browse_open) NA,
                   tags$summary(paste("Or browse every", o, "type")),
                   div(class = "chips mt-2", lapply(seq_len(nrow(ostyles)), function(i) {
                     tags$button(type = "button", class = paste("chip", if (browse_open && open$second[2] == ostyles$id[i]) "on"),
                                 onclick = js_set("open2", paste("browse", ostyles$id[i], sep = "|")),
                                 span(class = "dot", style = paste0("background:", ostyles$color[i])), ostyles$name[i])
                   })),
                   if (browse_open) div(class = "mt-2", product_picker(o, open$second[2]))),
      picked_pills(o))
  })

  # ---- step 3: the best wine for each cheese -------------------------------
  best_pairs <- reactive({
    w <- picked("wine"); ch <- picked("cheese")
    if (!nrow(w) || !nrow(ch)) return(NULL)
    do.call(rbind, lapply(seq_len(nrow(ch)), function(i) {
      rt <- vapply(w$style, function(ws) { r <- pair_rating(ws, ch$style[i]); if (is.na(r)) 1 else r }, 0)
      best <- which(rt == max(rt))
      data.frame(ckey = ch$key[i], cs = ch$style[i], cn = ch$name[i], rating = max(rt),
                 wkey = w$key[best[1]], ws = w$style[best[1]], wn = paste(w$name[best], collapse = " or "),
                 stringsAsFactors = FALSE)
    }))
  })

  output$step3 <- renderUI({
    bp <- best_pairs()
    if (is.null(bp)) return(div(class = "empty", "Once you have at least one wine and one cheese, this shows which of your wines suits each cheese best, and why."))
    w <- picked("wine")
    rows <- lapply(seq_len(nrow(bp)), function(i) {
      b <- bp[i, ]
      if (b$rating < 2) {
        alt <- PAIRINGS[PAIRINGS$cheese == b$cs & PAIRINGS$rating >= 2, ]
        alt <- alt[order(-alt$rating), ]
        return(div(class = "pairrow miss",
                   div(class = "pr-head", span(class = "nm", b$cn), span(class = "pill p0", "No good match")),
                   p(class = "why", paste0("None of your wines suit this cheese well.",
                                           if (nrow(alt)) paste0(" Try adding a ", tolower(style_name("wine", alt$wine[1])), ": ", alt$reason[1]) else "")),
                   if (nrow(alt)) div(class = "chips", lapply(seq_len(min(3, sum(WINE_PRODUCTS$style == alt$wine[1]))), function(j) {
                     prods <- WINE_PRODUCTS[WINE_PRODUCTS$style == alt$wine[1] & !grepl("^Not listed", WINE_PRODUCTS$note), ]
                     prods <- prods[order(prods$price), ]
                     if (j <= nrow(prods)) prod_chip("wine", prods[j, ])
                   }))))
      }
      pr <- PAIRINGS[PAIRINGS$wine == b$ws & PAIRINGS$cheese == b$cs, ]
      ex <- explain_pair(b$ws, b$cs, b$wn, b$cn); ex <- ex[ex$type == "pos", ]
      div(class = "pairrow",
          div(class = "pr-head", span(class = "nm", b$cn), span(class = "arrow", "→"), span(class = "wn", b$wn),
              span(class = paste0("pill p", b$rating), labels[[as.character(b$rating)]])),
          p(class = "why", pr$reason),
          tags$details(tags$summary("Why it works and how to serve it"),
                       tags$ul(class = "facts",
                               lapply(seq_len(nrow(ex)), function(j) tags$li(tags$b(paste0(ex$tag[j], ". ")), ex$text[j])),
                               tags$li(tags$b("Wine. "), WINE_TRAITS[[b$ws]]$temp), tags$li(tags$b("Cheese. "), CHEESE_TRAITS[[b$cs]]$rest))))
    })
    lonely <- w$name[!w$key %in% bp$wkey[bp$rating >= 2]]
    tagList(rows,
            if (length(lonely)) div(class = "about", tags$b("Not the top match for any cheese yet: "), paste(lonely, collapse = ", "),
                                    ". That's fine to keep, or pick a cheese for it in step 2."))
  })

  # ---- step 4: accompaniments -------------------------------------------
  ctx <- reactive({
    bp <- best_pairs()
    if (!is.null(bp)) return(data.frame(ws = ifelse(bp$rating >= 2, bp$ws, NA), cs = bp$cs, wn = bp$wn, cn = bp$cn, stringsAsFactors = FALSE))
    ch <- picked("cheese"); w <- picked("wine")
    if (nrow(ch)) return(data.frame(ws = NA, cs = ch$style, wn = NA, cn = ch$name, stringsAsFactors = FALSE))
    if (nrow(w)) return(data.frame(ws = w$style, cs = NA, wn = w$name, cn = NA, stringsAsFactors = FALSE))
    NULL
  })

  advice <- reactive({
    cx <- ctx(); rec <- list(); caut <- list()
    if (is.null(cx)) return(list(rec = rec, caut = caut))
    for (i in seq_len(nrow(cx))) {
      if (!is.na(cx$cs[i])) for (nm in names(SUGGESTIONS[[cx$cs[i]]]))
        rec[[nm]] <- unique(c(rec[[nm]], paste0("For the ", tolower(style_name("cheese", cx$cs[i])), ": ", SUGGESTIONS[[cx$cs[i]]][[nm]])))
      if (is.na(cx$ws[i])) next
      for (nm in names(WINE_LEANIN[[cx$ws[i]]]))
        rec[[nm]] <- unique(c(rec[[nm]], paste0("For the ", tolower(style_name("wine", cx$ws[i])), ": ", WINE_LEANIN[[cx$ws[i]]][[nm]])))
      wt <- WINE_TRAITS[[cx$ws[i]]]
      if (wt$dry_caution) for (nm in ADDONS$name[ADDONS$role == "sweet"])
        caut[[nm]] <- unique(c(caut[[nm]], "Sweeter than a dry wine, so it can make the wine taste sharp. Use a little."))
      if (wt$tannic) for (nm in HEAT_ITEMS)
        caut[[nm]] <- unique(c(caut[[nm]], "Heat makes tannin taste harsher."))
    }
    list(rec = rec, caut = caut)
  })

  output$board <- renderUI({
    cx <- ctx()
    if (is.null(cx)) return(div(class = "empty", "Pick wines and cheeses above and the cracker, fruit, spread and meat advice for them appears here."))
    adv <- advice()
    have <- unique(ADDONS$role[match(addons(), ADDONS$key)])
    checklist <- div(class = "checklist", span(class = "cl-h", "Balanced board checklist"),
                     lapply(names(BOARD_ROLES), function(r) span(class = paste("ck", if (r %in% have) "ok"), if (r %in% have) "✓ " else "○ ", BOARD_ROLES[[r]])))
    notes <- lapply(seq_len(nrow(cx)), function(i) {
      ws <- cx$ws[i]; cs <- cx$cs[i]
      title <- paste(c(if (!is.na(cx$cn[i])) cx$cn[i], if (!is.na(ws)) cx$wn[i]), collapse = " + ")
      tags$details(class = "adv", open = if (i == 1) NA,
          tags$summary(title),
          tags$ul(class = "facts",
                  if (!is.na(cs)) tags$li(tags$b("Bread or cracker? "), CHEESE_ADVICE[[cs]][["carrier"]]),
                  if (!is.na(cs)) tags$li(tags$b("Jam or meat? "), CHEESE_ADVICE[[cs]][["topper"]]),
                  if (!is.na(ws)) tags$li(tags$b("Because of the wine. "), WINE_ADVICE[[ws]]),
                  if (!is.na(ws) && WINE_TRAITS[[ws]]$dry_caution) tags$li(tags$b("Go easy on. "), "Honey, jam, chutney, candied nuts and chocolate. They are sweeter than this dry wine and will make it taste sharp. If you want a spread, pick the tartest one (apricot preserves, mustard) and keep it small."),
                  if (!is.na(ws) && WINE_TRAITS[[ws]]$tannic) tags$li(tags$b("Go easy on. "), "Spicy items such as hot honey, jalapeño crisps and chorizo. Heat makes tannin taste harsher.")))
    })
    cats <- lapply(ADDON_CATS, function(cat) {
      items <- ADDONS[ADDONS$category == cat, ]
      items <- items[order(!(items$name %in% names(adv$rec))), ]
      div(class = "mb-3",
          div(class = "cat-h", cat),
          div(class = "chips", lapply(seq_len(nrow(items)), function(i) {
            nm <- items$name[i]; rec <- nm %in% names(adv$rec); cau <- nm %in% names(adv$caut)
            tip <- paste(c(adv$rec[[nm]], adv$caut[[nm]]), collapse = " ")
            tags$button(type = "button", `data-tip` = if (nzchar(tip)) tip,
                        class = paste("chip prod", if (items$key[i] %in% addons()) "on", if (rec) "rec", if (cau) "caut"),
                        onclick = js_set("add", items$key[i]),
                        if (rec) span(class = "star", "★"), nm,
                        span(class = "px", money(items$price[i])), if (cau) span(class = "warn", "go easy"),
                        if (nzchar(tip)) span(class = "visually-hidden", paste0(" ", tip)))
          })))
    })
    tagList(checklist, notes, cats,
            div(class = "d-flex gap-2 flex-wrap align-items-center mt-2",
                tags$button(type = "button", class = "btn btn-outline-secondary btn-sm", onclick = js_set("quick", "1"), "Add the suggested items"),
                tags$button(type = "button", class = "btn btn-outline-secondary btn-sm", onclick = js_set("clear", "1"), "Clear board items"),
                span(class = "hint", "★ = suggested for your pairings. Hover over (or tab to) an item to see why. \"go easy\" = use a small amount.")))
  })

  # ---- shopping list with quantities ------------------------------------
  shop <- reactive({
    p <- plan()
    cks <- sel$cheese; wks <- sel$wine
    rows <- list()
    push <- function(name, qty, price, size = "") rows[[length(rows) + 1]] <<- data.frame(name = name, qty = qty, price = price, size = size, stringsAsFactors = FALSE)
    for (k in cks) {
      r <- prod_by_key("cheese", k); sz <- size_oz(r$size)
      push(r$name, if (is.na(sz)) 1 else max(1, ceiling(p$cheese_oz / length(cks) / sz)), r$price, r$size)
    }
    for (k in wks) {
      r <- prod_by_key("wine", k)
      push(r$name, max(1, ceiling(p$bottles / length(wks))), r$price, "750 mL")
    }
    it <- ADDONS[match(addons(), ADDONS$key), ]
    if (nrow(it)) {
      shape <- sub(":.*", "", it$icon)
      is_br <- it$role == "carrier" & shape %in% c("bread", "boule")
      is_cr <- it$role == "carrier" & !is_br
      is_mt <- it$category == "Cured meats"
      for (i in seq_len(nrow(it))) {
        q <- 1
        if (is_cr[i]) q <- max(1, ceiling(p$pieces * (if (any(is_br)) 0.6 else 1) / sum(is_cr) / 35))
        if (is_br[i]) q <- max(1, ceiling(p$pieces * (if (any(is_cr)) 0.4 else 1) / sum(is_br) / 18))
        if (is_mt[i]) { sz <- size_oz(it$size[i]); q <- max(1, ceiling(p$meat_oz / sum(is_mt) / sz)) }
        push(it$name[i], q, it$price[i], it$size[i])
      }
    }
    if (length(rows)) do.call(rbind, rows) else NULL
  })

  # ---- floating mini board ----------------------------------------------
  output$miniboard <- renderUI({
    p <- plan()
    cks <- sel$cheese; wks <- sel$wine
    cs <- CHEESE_PRODUCTS$style[match(cks, CHEESE_PRODUCTS$key)]
    wr <- WINE_PRODUCTS[match(wks, WINE_PRODUCTS$key), ]
    wines <- if (nrow(wr)) data.frame(
      color = WINE_STYLES$color[match(wr$style, WINE_STYLES$id)],
      kind = ifelse(wr$style %in% c("cremant", "prosecco") | grepl("Prosecco|Brut|Sparkling|Champagne|Fizz|Vivace", wr$name), "sparkling",
                    ifelse(wr$style %in% c("pinot", "chard"), "burgundy", "bordeaux")),
      stringsAsFactors = FALSE) else NULL
    items <- ADDONS[match(addons(), ADDONS$key), ]
    s <- shop()
    n <- length(cks) + length(wks) + nrow(items)
    total <- if (is.null(s)) 0 else sum(s$qty * s$price)
    list_txt <- if (is.null(s)) "" else paste0(s$qty, " x ", s$name, if_else_size(s$size), " (", money(s$price), " each)", collapse = "\n")

    tagList(
      div(class = "bw-head",
          span(class = "bw-title", "Your board"),
          tags$button(type = "button", class = "bw-toggle",
                      onclick = "this.closest('aside').classList.toggle('collapsed')",
                      paste0(n, if (n == 1) " item" else " items", " · show/hide"))),
      div(class = "bw-body",
          div(class = "bw-art", board_svg(cs, wines, items)),
          div(class = "bw-goal",
              span(class = if (length(cks) >= p$types) "ok", paste0("Cheeses ", length(cks), " of ", p$types)),
              span(class = if (length(wks) >= p$wine_types) "ok", paste0("Wines ", length(wks), " of ", p$wine_types, " (", p$bottles, " bottles)"))),
          if (length(wks) + length(cks)) div(class = "bits", lapply(c(if (length(wks)) paste0("wine|", wks), if (length(cks)) paste0("cheese|", cks)), function(v) {
            k <- strsplit(v, "|", fixed = TRUE)[[1]]
            nm <- prod_by_key(k[1], k[2])$name
            div(class = "bit pairbit", span(class = "ptxt", nm),
                tags$button(type = "button", title = "Remove", `aria-label` = paste("Remove", nm), onclick = js_set("toggle", v), "×"))
          })),
          if (nrow(items)) div(class = "bits", lapply(seq_len(nrow(items)), function(i) {
            div(class = "bit", items$name[i],
                tags$button(type = "button", title = "Remove", `aria-label` = paste("Remove", items$name[i]), onclick = js_set("rm", items$key[i]), "×"))
          })),
          div(class = "bw-foot",
              if (n > 0) div(class = "bw-total", span("About"), span(money(total))),
              if (n > 0) tags$details(tags$summary("Shopping list"), tags$pre(list_txt)),
              div(class = "hint", "Quantities follow your guest count. Website prices; they can differ in store."))))
  })
}

if_else_size <- function(sz) ifelse(nzchar(sz), paste0(" [", sz, "]"), "")

shinyApp(ui, server)
